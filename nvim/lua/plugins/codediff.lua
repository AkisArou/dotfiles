require("codediff").setup({
  highlights = {
    line_insert = "#373d29",
    char_insert = "#4b5632",
    line_delete = "#4b1818",
    char_delete = "#6f1313",
  },
})

vim.keymap.set("n", "<leader>gd", "<cmd>CodeDiff<CR>", { desc = "CodeDiff" })
