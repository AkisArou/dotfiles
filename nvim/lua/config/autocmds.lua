vim.cmd([[
  augroup highlight_yank
  autocmd!
  au TextYankPost * silent! lua vim.highlight.on_yank({higroup="Visual", timeout=200})
  augroup END
]])

function Close_empty_unnamed_buffers()
  local buffers = vim.api.nvim_list_bufs()

  for _, bufnr in ipairs(buffers) do
    if vim.api.nvim_buf_is_loaded(bufnr) and vim.api.nvim_buf_get_name(bufnr) == "" then
      vim.api.nvim_buf_delete(bufnr, {
        force = true,
      })
    end
  end
end

vim.api.nvim_command("autocmd BufReadPost * lua Close_empty_unnamed_buffers()")
