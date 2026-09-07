require("codediff").setup({
  highlights = {
    char_insert = "#444D2E",
  },
})

vim.keymap.set("n", "<leader>gd", "<cmd>CodeDiff<CR>", { desc = "CodeDiff" })
