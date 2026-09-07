require("codediff").setup({
  highlights = {
    char_insert = "#394126",
    line_insert = "#394126",
  },
})

vim.keymap.set("n", "<leader>gd", "<cmd>CodeDiff<CR>", { desc = "CodeDiff" })
