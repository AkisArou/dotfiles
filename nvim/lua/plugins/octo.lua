return {
  "pwntester/octo.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",
    -- OR 'ibhagwan/fzf-lua',
    "nvim-tree/nvim-web-devicons",
  },
  config = function()
    require("octo").setup()

    vim.keymap.set("n", "<leader>gi", ":Octo issue list nablesolutions/nable-solutions<CR>")
  end,
}
