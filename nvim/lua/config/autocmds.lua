-- local util = require("util.util")

-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

-- Disable the concealing in some file formats
-- The default conceallevel is 3 in LazyVim
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "*", "json", "jsonc", "markdown" },
  callback = function()
    vim.opt.conceallevel = 0
  end,
})

-- highlight yanked text for 200ms using the "Visual" highlight group
vim.cmd([[
  augroup highlight_yank
  autocmd!
  au TextYankPost * silent! lua vim.highlight.on_yank({higroup="Visual", timeout=200})
  augroup END
]])

-- CSS variables lsp

-- local file_path = util.get_css_variables_language_server_path()
--
-- -- Patch lsp (null TypeError in line 249)
-- local com = "grep -F "
--     .. "settings = settings || defaultSettings;"
--     .. " "
--     .. file_path
--     .. " || "
--     .. "sed -i '249i\\   settings = settings || defaultSettings;"
--     .. file_path
--
-- vim.fn.system(com)
