return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  ---@type snacks.Config
  opts = {
    bigfile = {
      -- your bigfile configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    },
    ---@class snacks.lazygit.Config: snacks.terminal.Opts
    ---@field args? string[]
    ---@field theme? snacks.lazygit.Theme
  },
  keys = {
    {
      "<leader>gs",
      function()
        Snacks.lazygit()
      end,
      desc = "Lazygit",
    },
    {
      "<leader>gf",
      function()
        Snacks.lazygit.log_file()
      end,
      desc = "Lazygit file",
    },
    {
      "<leader>bd",
      function()
        Snacks.bufdelete()
      end,
      desc = "Buffer delete current",
    },
    {
      "<leader>bo",
      function()
        Snacks.bufdelete.other()
      end,
      desc = "Buffer delete others",
    },
    {
      "<leader>ba",
      function()
        Snacks.bufdelete.all()
      end,
      desc = "Buffer delete others",
    },
  },
}
