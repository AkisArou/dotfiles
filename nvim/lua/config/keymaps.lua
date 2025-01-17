local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }

keymap("n", "<leader>h", "<cmd>nohlsearch<CR>", opts)
keymap("n", "<leader>w", ":w<CR>", opts)
keymap("n", "<leader>l", ":Lazy<CR>", opts)

-- Resize window using <ctrl> arrow keys
keymap("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Increase window height" })
keymap("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Decrease window height" })
keymap("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Decrease window width" })
keymap("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Increase window width" })

-- Move blocks
keymap("v", "J", ":m '>+1<CR>gv=gv")
keymap("v", "K", ":m '<-2<CR>gv=gv")

-- Cursor stays in place when moving screen
keymap("n", "<C-d>", "m`<C-d>zz")
keymap("n", "<C-u>", "m`<C-u>zz")

-- Better paste
keymap("v", "p", "P", opts)

-- delete char without copying
keymap("n", "x", '"_x')

-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Undo
keymap("n", "<leader>uu", vim.cmd.UndotreeToggle)

-- Keybinding for saving and closing the current buffer
vim.api.nvim_set_keymap("n", "<S-h>", "<cmd>bnext<CR>", opts)
vim.api.nvim_set_keymap("n", "<S-l>", "<cmd>bprevious<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>bd", ":lua require('custom.bufdelete').CloseCurrentBuffer()<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>bo", ":lua require('custom.bufdelete').CloseOtherBuffers()<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>ba", ":lua require('custom.bufdelete').CloseAllBuffers()<CR>", opts)
