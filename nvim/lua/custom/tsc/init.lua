local success, pcall_result = pcall(require, "notify")
local utils = require("custom.tsc.utils")

local M = {}

local nvim_notify

if success then
  nvim_notify = pcall_result
end

local DEFAULT_CONFIG = {
  auto_open = false,
  bin_path = utils.find_tsc_bin(),
  enable_progress_notifications = false,
  use_diagnostics = false,
  args = nil,
  spinner = { "⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷" },
}

local DEFAULT_NOTIFY_OPTIONS = {
  title = "TSC",
  hide_from_history = false,
}

local config = {}
local is_running = false
local pid

local function get_notify_options(...)
  local overrides = {}

  for _, opts in ipairs({ ... }) do
    for key, value in pairs(opts) do
      overrides[key] = value
    end
  end

  return vim.tbl_deep_extend("force", {}, DEFAULT_NOTIFY_OPTIONS, overrides)
end

local function format_notification_msg(msg, spinner_idx)
  if spinner_idx == 0 or spinner_idx == nil then
    return string.format(" %s ", msg)
  end

  return string.format(" %s %s ", config.spinner[spinner_idx], msg)
end

M.run = function()
  if is_running then
    -- vim.notify(format_notification_msg("Type-checking already in progress"), vim.log.levels.WARN, get_notify_options())
    return
  end

  -- Closed over state
  local tsc = config.bin_path
  local errors = {}
  local files_with_errors = {}
  local notify_record

  if not utils.is_executable(tsc) then
    vim.notify(
      format_notification_msg(
        "tsc was not available or found in your node_modules or $PATH. Please run install and try again."
      ),
      vim.log.levels.ERROR,
      get_notify_options()
    )

    return
  end

  is_running = true

  local icons = require("nvim-web-devicons")

  if config.enable_progress_notifications then
    vim.notify("  " .. icons.get_icon_by_filetype("typescript") .. "  Compiling...", nil, get_notify_options())
    -- vim.notify("👀 Building & Watching  🇹🇸, kick back and relax 🚀", nil, get_notify_options())
  end

  local function on_stdout(_, output)
    local result = utils.parse_tsc_output(output)

    errors = result.errors
    files_with_errors = result.files

    utils.set_qflist(errors)

    if config.use_diagnostics then
      local namespace_id = vim.api.nvim_create_namespace("tsc_diagnostics")
      vim.diagnostic.reset(namespace_id)

      for _, error in ipairs(errors) do
        local bufnr = vim.fn.bufnr(error.filename)
        if bufnr == -1 then
          vim.notify("Buffer not found for " .. error.filename, vim.log.levels.ERROR, get_notify_options())
          return
        end
        local diagnostic = {
          bufnr = bufnr,
          lnum = error.lnum - 1,
          col = error.col - 1,
          severity = vim.diagnostic.severity.ERROR,
          message = error.text,
          source = "tsc",
        }
        vim.diagnostic.set(namespace_id, bufnr, { diagnostic }, {})
      end
    end

    if config.enable_progress_notifications then
      if #errors == 0 then
        vim.notify(
          format_notification_msg("Type-checking complete. No errors found 🎉"),
          nil,
          get_notify_options((notify_record and { replace = notify_record.id }))
        )
        return
      end

      -- Clear any previous notifications if the user has nvim-notify installed
      if nvim_notify ~= nil then
        nvim_notify.dismiss()
      end

      vim.notify(
        format_notification_msg(
          string.format("Type-checking complete. Found %s errors across %s files 💥", #errors, #files_with_errors)
        ),
        vim.log.levels.ERROR,
        get_notify_options()
      )
    end
  end

  local total_output = {}

  local function watch_on_stdout(_, output)
    for _, v in ipairs(output) do
      table.insert(total_output, v)
    end

    for _, value in pairs(output) do
      if string.find(value, "Watching for file changes") then
        on_stdout(_, total_output)
        total_output = {}
      end
    end
  end

  local on_exit = function()
    is_running = false
  end

  local opts = {
    on_stdout = watch_on_stdout,
    on_exit = on_exit,
    stdout_buffered = false,
  }

  pid = vim.fn.jobstart(tsc .. " " .. config.args, opts)
end

function M.stop()
  if pid then
    vim.fn.jobstop(pid)
  end
end

function M.is_running()
  return is_running
end

function M.setup(opts)
  config = vim.tbl_deep_extend("force", config, DEFAULT_CONFIG, opts or {})

  vim.api.nvim_create_user_command("TSC", function()
    M.run()
  end, { desc = "Run `tsc` asynchronously and load the results into a qflist", force = true })

  vim.api.nvim_create_user_command("TSCStop", function()
    M.stop()
  end, { desc = "Stop `tsc` compilation", force = true })

  if config.enable_progress_notifications then
    vim.api.nvim_create_autocmd("BufWritePre", {
      pattern = "*.{ts,tsx}",
      desc = "Run tsc.nvim in watch mode automatically when saving a TypeScript file",
      callback = function()
        vim.notify("Type-checking your project via watch mode, hang tight 🚀", nil, get_notify_options())
      end,
    })
  end

  vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
    pattern = "*.{ts,tsx}",
    desc = "Start tsc.nvim in watch mode automatically when opening a TypeScript file",
    callback = function()
      M.run()
    end,
  })
end

return M
