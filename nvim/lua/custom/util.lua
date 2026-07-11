local M = {}

local ok, conform = pcall(require, "conform")

---@alias WriteCommand "w"|"wall"

---@param cmd WriteCommand
local function format_and_write(cmd)
  if not vim.bo.modifiable then
    return
  end

  if ok then
    conform.format()
  end

  vim.cmd("silent " .. cmd)
end

M.write_format = function()
  format_and_write("w")
end

M.write_format_all = function()
  format_and_write("wall")
end

return M
