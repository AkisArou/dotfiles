-- Shorten function name
local keymap = vim.keymap.set
-- Silent keymap option
local opts = { silent = true }

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Write-format
function save_and_format()
  vim.cmd(':w')
  vim.cmd(':Format')
end

keymap("n", "<leader>w", ":lua save_and_format()<CR>", opts)

-- Normal --
-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Quotes
keymap("n", "ciq", 'ci"')
keymap("n", "caq", 'ca"')
keymap("n", "diq", 'di"')
keymap("n", "daq", 'da"')
keymap("n", "yiq", 'yi"')
keymap("n", "yaq", 'ya"')
keymap("n", "viq", 'vi"')
keymap("n", "vaq", 'va"')

-- Resize with arrows
keymap("n", "<C-Up>", ":resize -2<CR>", opts)
keymap("n", "<C-Down>", ":resize +2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)

-- Navigate buffers
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)

-- Clear highlights
keymap("n", "<leader>h", "<cmd>nohlsearch<CR>", opts)

-- Close buffers
keymap("n", "<leader>bd", "<cmd>Bdelete!<CR>", opts)

function close_all_buffers_except_nvimtree()
  for _, buf in ipairs(vim.fn.getbufinfo({ bufloaded = true })) do
    -- Check if the buffer's name is not 'NvimTree'
    local buf_filetype = vim.fn.getbufvar(buf.bufnr, "&filetype")

    -- vim.api.nvim_echo({{buf_filetype, 'None'}}, false, {})

    if buf_filetype ~= 'NvimTree' and buf_filetype ~= "toggleterm" then
      vim.cmd('Bdelete ' .. buf.bufnr)
    end
  end
end

keymap("n", "<leader>ba", "[[:lua close_all_buffers_except_nvimtree()<CR>]]", opts)

function close_all_other_buffers_except_current_and_nvimtree()
  local current_bufnr = vim.fn.bufnr('')
  for _, buf in ipairs(vim.fn.getbufinfo({ bufloaded = true })) do
    local buf_filetype = vim.fn.getbufvar(buf.bufnr, "&filetype")

    if buf.bufnr ~= current_bufnr and buf_filetype ~= 'NvimTree' and buf_filetype ~= "toggleterm" then
      vim.cmd('Bdelete ' .. buf.bufnr)
    end
  end
end

keymap("n", "<leader>bo", "[[:lua close_all_other_buffers_except_current_and_nvimtree()<CR>]]", opts)

-- Move blocks
keymap("v", "J", ":m '>+1<CR>gv=gv")
keymap("v", "K", ":m '<-2<CR>gv=gv")

-- Cursor stays in place when moving screen
keymap("n", "<C-d>", "<C-d>zz")
keymap("n", "<C-u>", "<C-u>zz")

-- Better paste
keymap("v", "p", "P", opts)

-- delete char without copying
keymap("n", "x", '"_x')

-- Insert --
-- Press jk fast to enter
keymap("i", "jk", "<ESC>", opts)

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Plugins --

-- NvimTree
keymap("n", "<leader>e", ":NvimTreeToggle<CR>", opts)

-- Telescope
keymap("n", "<leader>ff", ":CocList files<CR>", opts)
keymap("n", "<leader>ft", ":Telescope live_grep<CR>", opts)
keymap("n", "<leader>fp", ":Telescope projects<CR>", opts)
keymap("n", "<leader>fb", ":Telescope buffers<CR>", opts)

-- Git
keymap("n", "<leader>gg", "<cmd>lua _LAZYGIT_TOGGLE()<CR>", opts)
keymap("n", "<leader>gs", vim.cmd.Git)

-- Comment
keymap("n", "<leader>/", "<cmd>lua require('Comment.api').toggle.linewise.current()<CR>", opts)
keymap("x", "<leader>/", "<esc><cmd>lua require('Comment.api').toggle.linewise(vim.fn.visualmode())<CR>", opts)

-- Undotree
keymap("n", "<leader>u", vim.cmd.UndotreeToggle)

-- Toggleterm
keymap("n", "<C-t>", ":lua require('toggleterm').toggle_all()<CR>")

