local M = {}

M.write = function()
  if not vim.bo.modifiable or vim.bo.readonly or vim.bo.buftype ~= "" or vim.api.nvim_buf_get_name(0) == "" then
    return
  end

  vim.cmd("silent write")
end

M.write_all = function()
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].modified then
      vim.api.nvim_buf_call(buf, M.write)
    end
  end
end

return M
