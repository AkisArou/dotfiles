local map = vim.keymap.set
local opts = { noremap = true, silent = true }

map("i", "<C-c>", "<Esc>")
map("n", "<leader>h", "<cmd>nohlsearch<CR>", opts)
map({ "i", "x", "n", "s" }, "<C-s>", "<cmd>w<cr><esc>", { desc = "Save File" })
map({ "n", "v" }, "<C-j>", "6j", opts)
map({ "n", "v" }, "<C-k>", "6k", opts)

-- Resize window using <ctrl> arrow keys
map("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Increase window height" })
map("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Decrease window height" })
map("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Decrease window width" })
map("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Increase window width" })

-- Move blocks
map("v", "J", ":m '>+1<CR>gv=gv")
map("v", "K", ":m '<-2<CR>gv=gv")

-- delete char without copying
map("n", "x", '"_x')

-- keep yanked text on paste
map("x", "p", [["_dP]], opts)

-- Stay in indent mode
map("v", "<", "<gv", opts)
map("v", ">", ">gv", opts)

-- https://github.com/mhinz/vim-galore#saner-behavior-of-n-and-n
map("n", "n", "'Nn'[v:searchforward].'zv'", { expr = true, desc = "Next Search Result" })
map("x", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
map("o", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
map("n", "N", "'nN'[v:searchforward].'zv'", { expr = true, desc = "Prev Search Result" })
map("x", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })
map("o", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })

-- Add undo break-points
map("i", ",", ",<c-g>u")
map("i", ".", ".<c-g>u")
map("i", ";", ";<c-g>u")

-- This won't work in all terminal emulators/tmux/etc. Try your own mapping
-- or just use <C-\><C-n> to exit terminal mode
map("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

-- qflist
map("n", "<leader>xq", "<cmd>copen<CR>", opts)

-- Keybinding for saving and closing the current buffer
local function format_and_switch_buffer(cmd)
  if vim.bo.modifiable then
    vim.cmd("lua require('conform').format()")
  end

  vim.cmd(cmd)
end

map("n", "[b", function()
  format_and_switch_buffer("bprevious")
end, opts)

map("n", "]b", function()
  format_and_switch_buffer("bnext")
end, opts)

vim.keymap.set("n", "<leader>uu", vim.cmd.Undotree)

local difftool_floating_window = require("custom.floating-window").create_floating_window_api(function(state)
  if vim.bo[state.floating.buf].buftype ~= "terminal" then
    vim.cmd("terminal git difftool -d")
  end

  vim.cmd("startinsert")
end)

vim.keymap.set({ "n", "t" }, "<leader>gd", difftool_floating_window.toggle_fresh)

vim.keymap.set("n", "<leader>t", function()
  vim.cmd.vnew()
  vim.cmd.term()
  vim.cmd.wincmd("J")
  vim.api.nvim_win_set_height(0, 15)
end)

vim.keymap.set("n", "<leader>cp", require("custom.pnpm").install)
