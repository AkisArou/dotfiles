return {
  "pwntester/octo.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "ibhagwan/fzf-lua",
    "nvim-tree/nvim-web-devicons",
  },
  config = function()
    require("octo").setup({
      picker = "fzf-lua",
    })

    vim.keymap.set(
      "n",
      "<leader>gi",
      "<cmd>Octo issue list nablesolutions/nable-solutions<CR>",
      { desc = "Nable issues" }
    )
  end,
}
