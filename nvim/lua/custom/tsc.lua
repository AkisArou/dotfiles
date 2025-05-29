local M = {
  pid = nil,
  is_running = false,
  total_errors = 0,
}

local bin_path = (function()
  local node_modules_tsc_binary = vim.fn.findfile("node_modules/.bin/tsc", ".;")

  if type(node_modules_tsc_binary) == "string" and node_modules_tsc_binary ~= "" then
    return node_modules_tsc_binary
  end

  return "tsc"
end)()

local config = {
  bin_path = bin_path,
  enable_progress_notifications = false,
  use_diagnostics = false,
  args = nil,
}

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

  if not vim.fn.executable(tsc) == 1 or false then
    vim.notify(
      "tsc was not available or found in your node_modules or $PATH. Please run install and try again.",
      vim.log.levels.ERROR
    )
    return
  end

  M.is_running = true
  M.notify_start()

  local errors = {}
  local files_with_errors = {}

  local total_output = {}

  local function on_stdout(_, output)
    for _, v in ipairs(output) do
      table.insert(total_output, v)
    end

    for _, value in pairs(output) do
      if string.find(value, "Watching for file changes") then
        local result = M.parse_tsc_output(total_output)

        errors = result.errors
        files_with_errors = result.files

        M.set_qflist(errors)
        M.total_errors = #errors
        M.show_diagnostics(errors)
        M.notify_progress(errors, files_with_errors)
        total_output = {}
      end
    end
  end

  local on_exit = function()
    M.is_running = false
  end

  M.pid = vim.fn.jobstart(tsc .. " " .. config.args, {
    on_stdout = on_stdout,
    on_exit = on_exit,
    stdout_buffered = false,
  })
end

function M.show_diagnostics(errors)
  if not config.use_diagnostics then
    return
  end

  local namespace_id = vim.api.nvim_create_namespace("tsc_diagnostics")

  vim.diagnostic.reset(namespace_id)

  for _, error in ipairs(errors) do
    local bufnr = vim.fn.bufnr(error.filename)

    if bufnr == -1 then
      vim.notify("Buffer not found for " .. error.filename, vim.log.levels.ERROR)
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

function M.notify_progress(errors, files_with_errors)
  if config.enable_progress_notifications then
    if #errors == 0 then
      vim.notify("Type-checking complete. No errors found 🎉", nil)
      return
    end

    vim.notify(
      string.format("Type-checking complete. Found %s errors across %s files 💥", #errors, #files_with_errors),
      vim.log.levels.ERROR
    )
  end
end

function M.notify_start()
  if not config.enable_progress_notifications then
    return
  end

  vim.notify("  " .. require("nvim-web-devicons").get_icon_by_filetype("typescript") .. "  Compiling...", nil)
end

function M.stop()
  if not M.pid then
    return
  end

  vim.fn.jobstop(M.pid)
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
        vim.notify("Type-checking your project via watch mode, hang tight 🚀", nil)
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
