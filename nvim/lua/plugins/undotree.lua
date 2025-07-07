local is_open = false

-- Undo
vim.keymap.set("n", "<leader>uu", function()
  vim.cmd.UndotreeToggle()

  is_open = not is_open

  if is_open then
    vim.cmd.UndotreeFocus()
  end
end)
