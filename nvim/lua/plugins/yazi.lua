return {
  "mikavilpas/yazi.nvim",
  event = "VeryLazy",
  keys = {
    -- 👇 in this section, choose your own keymappings!
    {
      "<leader>e",
      "<cmd>Yazi toggle<cr>",
      desc = "Resume the last yazi session",
    },
    {
      -- Open in the current working directory
      "<leader>cw",
      "<cmd>Yazi cwd<cr>",
      desc = "Open the file manager in nvim's working directory",
    },
  },
  ---@type YaziConfig
  opts = {
    -- if you want to open yazi instead of netrw, see below for more info
    open_for_directories = false,
    yazi_floating_window_border = "shadow",
    floating_window_scaling_factor = 1,
    keymaps = {
      show_help = "<f1>",
    },
  },
}
