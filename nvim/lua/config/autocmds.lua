-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

-- Disable the concealing in some file formats
-- The default conceallevel is 3 in LazyVim
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "json", "jsonc", "markdown" },
  callback = function()
    vim.opt.conceallevel = 0
  end,
})

-- Npm workspaces lsp
local c = vim.lsp.start_client({
  config = {
    cmd = { "npx", "npm-workspaces-lsp", "--stdio" },
  },
  name = "npm-workspaces-lsp",
  cmd = { "npx", "npm-workspaces-lsp", "--stdio" },
  root_dir = vim.loop.cwd(),
})

vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
  pattern = { "package.json" },
  callback = function()
    vim.lsp.buf_attach_client(0, c)
  end,
})
--
