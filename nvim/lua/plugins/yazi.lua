return {
  "mikavilpas/yazi.nvim",
  event = "VeryLazy",
  keys = {
    {
      "<leader>e",
      "<cmd>Yazi<cr>",
      desc = "Resume the last yazi session",
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
      keymaps = {
        show_help = "<f1>",
      },
    })
  end,
}
