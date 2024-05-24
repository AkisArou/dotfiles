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

-- vim.api.nvim_create_autocmd("BufWriteCmd", {
--   pattern = { "*.tsx", "*.jsx", "*.css" },
--   command = "TailwindSort",
-- })

-- highlight yanked text for 200ms using the "Visual" highlight group
vim.cmd([[
  augroup highlight_yank
  autocmd!
  au TextYankPost * silent! lua vim.highlight.on_yank({higroup="Visual", timeout=200})
  augroup END
]])

-- Auto save session
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
  callback = function()
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      -- Don't save while there's any 'nofile' buffer open.
      if vim.api.nvim_get_option_value("buftype", { buf = buf }) == "nofile" then
        return
      end
    end
    session_manager.save_current_session()
  end,
})

-- Function to close empty and unnamed buffers
function Close_empty_unnamed_buffers()
  -- Get a list of all buffers
  local buffers = vim.api.nvim_list_bufs()

  -- Iterate over each buffer
  for _, bufnr in ipairs(buffers) do
    -- Check if the buffer is empty and doesn't have a name
    if
      vim.api.nvim_buf_is_loaded(bufnr) and vim.api.nvim_buf_get_name(bufnr) == ""
      -- and vim.api.nvim_buf_get_option(bufnr, "buftype") == ""
    then
      -- -- Get all lines in the buffer
      -- local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
      --
      -- -- Initialize a variable to store the total number of characters
      -- local total_characters = 0
      --
      -- -- Iterate over each line and calculate the number of characters
      -- for _, line in ipairs(lines) do
      --   total_characters = total_characters + #line
      -- end

      -- Close the buffer if it's empty:
      -- if total_characters == 0 then
      vim.api.nvim_buf_delete(bufnr, {
        force = true,
      })
      -- end
    end
  end
end

-- Clear the mandatory, empty, unnamed buffer when a real file is opened:
vim.api.nvim_command("autocmd BufReadPost * lua Close_empty_unnamed_buffers()")

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
