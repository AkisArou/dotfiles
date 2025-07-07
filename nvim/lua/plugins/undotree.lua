vim.api.nvim_create_autocmd("FileType", {
  pattern = "undotree",
  callback = function()
    vim.defer_fn(vim.cmd.UndotreeFocus, 0)
  end,
})

vim.keymap.set("n", "<leader>uu", vim.cmd.UndotreeToggle)
