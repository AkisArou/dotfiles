local has_trouble, pcall_trouble = pcall(require, "trouble")
local trouble = has_trouble and pcall_trouble or nil

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
  local DEFAULT_OPTS = { use_trouble = false }
  local final_opts = vim.tbl_extend("force", DEFAULT_OPTS, opts or {})

  vim.fn.setqflist({}, "r", { title = "TSC", items = errors })

  if #errors > 0 then
    M.open_qflist(final_opts.use_trouble)
  end

  if #errors == 0 then
    -- trouble needs to be refreshed when list is empty.
    if final_opts.use_trouble and trouble ~= nil then
      trouble.refresh()
    end

    M.close_qflist(final_opts.use_trouble)
  end
end

--- open the qflist
--- @param use_trouble boolean: if trouble should be used as qflist provider
--- @return nil
M.open_qflist = function(use_trouble)
  if use_trouble and trouble ~= nil then
    require("trouble").open({
      mode = "quickfix",
      win = {
        position = "right",
      },
      focus = false,
    })
    -- vim.cmd("lua Trouble quickfix toggle focus=false win.position=right<cr>")
  else
    vim.cmd("copen")
  end
end

--- close the qflist
--- @param use_trouble boolean: if trouble should be used as qflist provider
--- @return nil
M.close_qflist = function(use_trouble)
  if use_trouble and trouble ~= nil then
    trouble.close()
  else
    vim.cmd("cclose")
  end
end

return M
