local map = vim.keymap.set
local opts = { noremap = true, silent = true }

map("n", "<leader>h", "<cmd>nohlsearch<CR>", opts)
map("n", "<leader>w", ":w<CR>", opts)
map("n", "<leader>l", ":Lazy<CR>", opts)

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

-- Undo
map("n", "<leader>uu", vim.cmd.UndotreeToggle)

-- Add undo break-points
map("i", ",", ",<c-g>u")
map("i", ".", ".<c-g>u")
map("i", ";", ";<c-g>u")

-- Keybinding for saving and closing the current buffer
map("n", "[b", "<cmd>lua require('conform').format()<CR><cmd>bprevious<CR>", opts)
map("n", "]b", "<cmd>lua require('conform').format()<CR><cmd>bnext<CR>", opts)

-- qflist
map("n", "<leader>xq", "<cmd>copen<CR>", opts)
