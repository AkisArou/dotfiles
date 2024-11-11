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

function CloseAllBuffersExceptCurrent()
  local current_buf = vim.api.nvim_get_current_buf()

  -- Get all buffers
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    -- Check if buffer is valid, loaded, and is not neo-tree
    local buf_name = vim.api.nvim_buf_get_name(buf)
    if vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_buf_is_loaded(buf) then
      if not buf_name:match("NeogitStatus") and buf ~= current_buf then
        -- Save the buffer if it's modified
        if vim.api.nvim_buf_get_option(buf, "modified") then
          vim.api.nvim_command("bufdo silent! w") -- Save all modified buffers silently
        end
        -- Forcefully delete the buffer
        vim.api.nvim_buf_delete(buf, { force = true })
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
      if not buf_name:match("NeogitStatus") then
        -- Save the buffer if it's modified
        if vim.api.nvim_buf_get_option(buf, "modified") then
          vim.api.nvim_command("bufdo silent! w") -- Save all modified buffers silently
        end
        -- Forcefully delete the buffer
        vim.api.nvim_buf_delete(buf, { force = true })
      end
    end
  end
end

function CloseCurrentBuffer()
  local current_buf = vim.api.nvim_get_current_buf()

  -- Check if the buffer is valid and loaded
  if vim.api.nvim_buf_is_valid(current_buf) and vim.api.nvim_buf_is_loaded(current_buf) then
    -- Check if the buffer is modified
    if vim.api.nvim_buf_get_option(current_buf, "modified") then
      -- Save the current buffer
      vim.api.nvim_buf_call(current_buf, function()
        vim.cmd("w") -- or vim.cmd('silent! w') for silent save
      end)
    end

    -- Close the current buffer
    vim.api.nvim_buf_delete(current_buf, { force = true })
  end
end

-- Keybinding for saving and closing the current buffer
vim.api.nvim_set_keymap("n", "<leader>bd", ":lua CloseCurrentBuffer()<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>bo", ":lua CloseAllBuffersExceptCurrent()<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>ba", ":lua CloseAllBuffers()<CR>", opts)
