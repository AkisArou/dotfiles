local trouble = require("trouble")

-- global variable
TSC_ERRORS_COUNT = 0

local M = {}

M.is_executable = function(cmd)
  return cmd and vim.fn.executable(cmd) == 1 or false
end

M.find_tsc_bin = function()
  local node_modules_tsc_binary = vim.fn.findfile("node_modules/.bin/tsc", ".;")

  if node_modules_tsc_binary ~= "" then
    return node_modules_tsc_binary
  end

  return "tsc"
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

M.set_qflist = function(errors, opts)
  vim.fn.setqflist({}, "r", { title = "TSC", items = errors })

  TSC_ERRORS_COUNT = #errors

  if #errors > 0 and opts.auto_open then
    trouble.refresh()
    M.open_qflist()
  end

  if #errors == 0 then
    trouble.refresh()
    M.close_qflist()
  end
end

M.open_qflist = function()
  require("trouble").open({
    mode = "quickfix",
    win = {
      type = "split",
      size = 30,
    },
    focus = true,
  })
end

M.close_qflist = function()
  trouble.close()
end

return M
