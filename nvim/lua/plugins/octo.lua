require("octo").setup({
  picker = "fzf-lua",
})

vim.keymap.set("n", "<leader>gi", "<cmd>Octo issue list nablesolutions/nable-solutions<CR>", { desc = "Nable issues" })
