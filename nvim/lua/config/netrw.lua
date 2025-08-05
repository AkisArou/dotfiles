-- Netrw global settings
vim.g.netrw_banner = 0
vim.g.netrw_dirhistmax = 0
vim.g.netrw_keepdir = 1
vim.g.netrw_localmkdir = "mkdir -p"
vim.g.netrw_localcopycmd = "cp -r"
vim.g.netrw_localrmdir = "rm -r"
vim.g.netrw_list_hide = [[^\./\=$,\.\./]]
vim.g.netrw_hide = 1 -- make sure hiding is enabled

-- Keymap to open netrw and highlight current file
vim.keymap.set("n", "<leader>e", ':Ex | silent! call search(expand("#:t"))<CR>', { silent = true })

-- Use the exact original Vimscript fallback to override netrw’s control
vim.api.nvim_create_augroup("netrw_mapping", { clear = true })

vim.api.nvim_create_autocmd("FileType", {
  group = "netrw_mapping",
  pattern = "netrw",
  callback = function()
    vim.cmd([[
      " These are buffer-local, so this must run after netrw is initialized
      nmap <buffer> <C-c> :bw<CR>
      nmap <buffer> H u
      nmap <buffer> h -^
      nmap <buffer> l <CR>
      nmap <buffer> . gh
      nmap <buffer> P <C-w>z
      nmap <buffer> L <CR>:Lexplore<CR>
      nmap <buffer> <Leader>dd :Lexplore<CR>
      nmap <buffer> % := require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.netrw").create()<CR>
      nmap <buffer> R := require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.netrw").rename()<CR>
      nmap <buffer> D := require("custom.nvim-lsp-file-operations.lua.lsp-file-operations.netrw").delete()<CR>
    ]])
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "netrw",
  callback = function()
    vim.cmd("normal! gg") -- move to top of file
  end,
})
