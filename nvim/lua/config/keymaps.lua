-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local Util = require("lazyvim.util")
local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }

keymap("n", "<leader>h", "<cmd>nohlsearch<CR>", opts)
-- keymap("n", "<leader>w", ":w", opts)

-- Move blocks
keymap("v", "J", ":m '>+1<CR>gv=gv")
keymap("v", "K", ":m '<-2<CR>gv=gv")

-- Cursor stays in place when moving screen
keymap("n", "<C-d>", "<C-d>zz")
keymap("n", "<C-u>", "<C-u>zz")

-- Buffer close all
function closeAllBuffers()
  vim.cmd("BufferLineCloseOthers")
  require("mini.bufremove").delete()
end
keymap("n", "<leader>ba", "<cmd>lua closeAllBuffers()<CR>", { desc = "Delete all" })

-- Better paste
keymap("v", "p", "P", opts)

-- delete char without copying
keymap("n", "x", '"_x')

-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Undo
keymap("n", "<leader>uu", vim.cmd.UndotreeToggle)

-- lazygit
keymap("n", "<leader>gg", function()
  Util.terminal({ "gitui" })
end, { desc = "GitUI" })
