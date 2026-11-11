local M = {}

---@param cmd "w" | "wall"
local function write(cmd)
  if not vim.bo.modifiable then
    return
  end

  vim.cmd("silent " .. cmd)
end

M.write = function()
  write("w")
end

M.write_all = function()
  write("wall")
end

return M
