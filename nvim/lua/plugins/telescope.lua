return {
  "nvim-telescope/telescope.nvim",
  version = false,
  dependencies = {
    "nvim-lua/plenary.nvim",
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
      enabled = vim.fn.executable("make") == 1,
      config = function()
        require("telescope").load_extension("fzf")
      end,
    },
    "nvim-tree/nvim-web-devicons",
  },
  config = function()
    require("telescope").setup({
      pickers = {
        find_files = {
          sorting_strategy = "ascending",
          layout_config = {
            prompt_position = "top",
            preview_width = 0.25,
          },
        },
      },
    })

    vim.keymap.set("n", "<leader>ff", "<cmd>Telescope find_files<cr>", { desc = "Fuzzy find files in cwd" })
    vim.keymap.set("n", "<leader>fr", "<cmd>Telescope oldfiles<cr>", { desc = "Fuzzy find recent files" })
    vim.keymap.set("n", "<leader>fs", "<cmd>Telescope live_grep<cr>", { desc = "Find string in cwd" })
    vim.keymap.set("n", "<leader>fc", "<cmd>Telescope grep_string<cr>", { desc = "Find string under cursor in cwd" })
    vim.keymap.set("n", "<leader>fd", "<cmd>Telescope diagnostics bufnr=0<cr>", { desc = "Document diagnostics" })
    vim.keymap.set("n", "<leader>sD", "<cmd>Telescope diagnostics<cr>", { desc = "Workspace diagnostics" })
  end,
}
