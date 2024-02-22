return {
  {
    "folke/persistence.nvim",
    event = "BufReadPre",
    enabled = false,
    opts = { options = vim.opt.sessionoptions:get() },
  -- stylua: ignore
  keys = {
    { "<leader>qs", function() require("persistence").load() end, desc = "Restore Session" },
    { "<leader>ql", function() require("persistence").load({ last = true }) end, desc = "Restore Last Session" },
    { "<leader>qd", function() require("persistence").stop() end, desc = "Don't Save Current Session" },
    },
  },

  {
    "rmagatti/auto-session",
    enabled = false,
    config = function()
      require("auto-session").setup({
        -- pre_save_cmds = { "Neotree close" },
      })
    end,
  },
}
