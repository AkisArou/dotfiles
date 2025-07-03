return {
  "mikavilpas/yazi.nvim",
  lazy = false,
  dependencies = {
    "dstein64/vim-startuptime",
  },
  keys = {
    {
      "<leader>e",
      "<cmd>Yazi<cr>",
      desc = "Open yazi at the current file",
    },
    -- {
    --   "<leader>cw",
    --   "<cmd>Yazi cwd<cr>",
    --   desc = "Open the file manager in nvim's working directory",
    -- },
  },
  config = function()
    require("yazi").setup({
      open_for_directories = true,
      yazi_floating_window_border = "shadow",
      floating_window_scaling_factor = 1,
      future_features = {
        process_events_live = false,
      },
    })
  end,
}
