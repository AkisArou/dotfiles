local map = vim.keymap.set
local opts = { noremap = true, silent = true }

map("n", "<leader>h", "<cmd>nohlsearch<CR>", opts)
map("n", "<leader>w", ":w<CR>", opts)
map("n", "<leader>l", ":Lazy<CR>", opts)

--  See `:help wincmd` for a list of all window commands
map("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" })
map("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" })
map("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
map("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" })

-- Resize window using <ctrl> arrow keys
map("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Increase window height" })
map("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Decrease window height" })
map("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Decrease window width" })
map("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Increase window width" })

-- Move blocks
map("v", "J", ":m '>+1<CR>gv=gv")
map("v", "K", ":m '<-2<CR>gv=gv")

-- Better paste
map("v", "p", "P", opts)

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
map("n", "<S-h>", "<cmd>bprevious<CR>", opts)
map("n", "<S-l>", "<cmd>bnext<CR>", opts)

local function lazykeys(keys)
  keys = vim.api.nvim_replace_termcodes(keys, true, false, true)
  return function()
    local old = vim.o.lazyredraw
    vim.o.lazyredraw = true
    vim.api.nvim_feedkeys(keys, "nx", false)
    vim.o.lazyredraw = old
  end
end

-- Cursor stays in place when moving screen
vim.keymap.set("n", "<c-d>", lazykeys("<c-d>zz"), { desc = "Scroll down half screen" })
vim.keymap.set("n", "<c-u>", lazykeys("<c-u>zz"), { desc = "Scroll down half screen" })
