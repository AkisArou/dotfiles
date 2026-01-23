local M = {}

local _, conform = pcall(require, "conform")

M.write_format = function()
  if not vim.bo.modifiable then
    return
  end

  conform.format()
  vim.cmd("silent w")
end

M.write_format_all = function()
  if not vim.bo.modifiable then
    return
  end

  conform.format()
  vim.cmd("silent wall")
end

return M
