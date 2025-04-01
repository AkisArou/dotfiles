local util = {}

util.write_format = function()
  if not vim.bo.modifiable then
    return
  end

  require("conform").format()
  vim.cmd("silent wall")
end

return util
