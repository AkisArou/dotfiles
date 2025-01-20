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
vim.api.nvim_set_keymap("n", "<S-h>", "<cmd>bprevious<CR>", opts)
vim.api.nvim_set_keymap("n", "<S-l>", "<cmd>bnext<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>bd", ":lua require('custom.bufdelete').CloseCurrentBuffer()<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>bo", ":lua require('custom.bufdelete').CloseOtherBuffers()<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>ba", ":lua require('custom.bufdelete').CloseAllBuffers()<CR>", opts)

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
