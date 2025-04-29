local M = {
  pid = nil,
  is_running = false,
  total_errors = 0,
}

local config = {
  bin_path = (function()
    local node_modules_tsc_binary = vim.fn.findfile("node_modules/.bin/tsc", ".;")

    if node_modules_tsc_binary ~= "" then
      return node_modules_tsc_binary
    end

    return "tsc"
  end)(),
  enable_progress_notifications = false,
  use_diagnostics = false,
  args = nil,
}

local function get_notify_options(...)
  local overrides = {}

  for _, opts in ipairs({ ... }) do
    for key, value in pairs(opts) do
      overrides[key] = value
    end
  end

  return vim.tbl_deep_extend("force", {}, {
    title = "TSC",
    hide_from_history = false,
  }, overrides)
end

M.parse_tsc_output = function(output)
  local errors = {}
  local files = {}

  if output == nil then
    return { errors = errors, files = files }
  end

  for _, line in ipairs(output) do
    local filename, lineno, colno, message = line:match("^(.+)%((%d+),(%d+)%)%s*:%s*(.+)$")
    if filename ~= nil then
      table.insert(errors, {
        filename = filename,
        lnum = tonumber(lineno),
        col = tonumber(colno),
        text = message,
        type = "E",
      })

      if vim.tbl_contains(files, filename) == false then
        table.insert(files, filename)
      end
    end
  end

  return { errors = errors, files = files }
end

M.set_qflist = function(errors)
  vim.fn.setqflist({}, "r", { title = "TSC", items = errors })
end

M.run = function()
  if M.is_running then
    return
  end

  local tsc = config.bin_path
  local errors = {}
  local files_with_errors = {}
  local notify_record

  local is_tsc_executable = vim.fn.executable(tsc) == 1 or false

  if not is_tsc_executable then
    vim.notify(
      "tsc was not available or found in your node_modules or $PATH. Please run install and try again.",
      vim.log.levels.ERROR,
      get_notify_options()
    )

    return
  end

  M.is_running = true

  if config.enable_progress_notifications then
    vim.notify(
      "  " .. require("nvim-web-devicons").get_icon_by_filetype("typescript") .. "  Compiling...",
      nil,
      get_notify_options()
    )
  end

  local function on_stdout(_, output)
    local result = M.parse_tsc_output(output)

    errors = result.errors
    files_with_errors = result.files

    M.set_qflist(errors)
    M.total_errors = #errors

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
          "Type-checking complete. No errors found 🎉",
          nil,
          get_notify_options((notify_record and { replace = notify_record.id }))
        )
        return
      end

      vim.notify(
        string.format("Type-checking complete. Found %s errors across %s files 💥", #errors, #files_with_errors),
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
    M.is_running = false
  end

  M.pid = vim.fn.jobstart(tsc .. " " .. config.args, {
    on_stdout = watch_on_stdout,
    on_exit = on_exit,
    stdout_buffered = false,
  })
end

function M.stop()
  if M.pid then
    vim.fn.jobstop(M.pid)
  end
end

function M.setup(opts)
  config = vim.tbl_deep_extend("force", config, opts or {})

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
