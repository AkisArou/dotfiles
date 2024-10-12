local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }

keymap("n", "<leader>h", "<cmd>nohlsearch<CR>", opts)
keymap("n", "<leader>w", ":w<CR>", opts)
keymap("n", "<leader>l", ":Lazy<CR>", opts)

keymap("i", "jk", [[<C-\><C-n>]]) -- no need to escape the '\'

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

-- Buffer close all
keymap("n", "<leader>ba", function()
  vim.cmd("wa")
  vim.cmd("BufferLineCloseOthers")
  require("mini.bufremove").delete()
end, { desc = "Delete all" })

-- Better paste
keymap("v", "p", "P", opts)

-- delete char without copying
keymap("n", "x", '"_x')

-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Undo
keymap("n", "<leader>uu", vim.cmd.UndotreeToggle)

-- Close all buffers except the current one and avoid closing neo-tree
function CloseAllBuffersExceptCurrent()
  -- Get the current buffer
  local current_buf = vim.api.nvim_get_current_buf()

  -- Get all buffers
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    -- Check if buffer is valid, loaded, and is not neo-tree
    local buf_name = vim.api.nvim_buf_get_name(buf)
    if vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_buf_is_loaded(buf) then
      -- Avoid closing neo-tree buffer
      if not buf_name:match("neo%-tree") and buf ~= current_buf then
        vim.api.nvim_buf_delete(buf, {})
      end
    end
  end
end

function CloseAllBuffers()
  -- Get all buffers
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    -- Check if buffer is valid, loaded, and is not neo-tree
    local buf_name = vim.api.nvim_buf_get_name(buf)
    if vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_buf_is_loaded(buf) then
      -- Avoid closing neo-tree buffer
      if not buf_name:match("neo%-tree") then
        vim.api.nvim_buf_delete(buf, {})
      end
    end
  end
end

vim.api.nvim_set_keymap("n", "<leader>n", "<cmd>bnext<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>p", "<cmd>bprevious<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>bo", ":lua CloseAllBuffersExceptCurrent()<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>ba", ":lua CloseAllBuffers()<CR>", opts)
