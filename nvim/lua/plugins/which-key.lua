require("which-key").setup({
  delay = 1000,
  triggers = {
    { "<auto>", mode = { "n", "v" } },
  },
})

vim.keymap.set("n", "<leader>?", function()
  require("which-key").show({ global = false })
end, { desc = "Buffer Local Keymaps (which-key)" })
