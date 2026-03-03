require("persistence").setup()

vim.keymap.set("n", "<leader>sl", function()
  require("persistence").load()
end)

vim.api.nvim_create_autocmd("User", {
  pattern = "PersistenceSavePre",
  callback = function()
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.bo[buf].filetype == "opencode_output" or vim.bo[buf].filetype == "opencode" then
        vim.api.nvim_buf_delete(buf, { force = true })
      end
    end
  end,
})
