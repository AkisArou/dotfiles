require("codediff").setup({
  -- Groups from colorscheme.lua, so the colours follow 'background'.
  highlights = {
    line_insert = "DiffLineInsert",
    char_insert = "DiffCharInsert",
    line_delete = "DiffLineDelete",
    char_delete = "DiffCharDelete",
  },
})

vim.keymap.set("n", "<leader>gd", "<cmd>CodeDiff<CR>", { desc = "CodeDiff" })
