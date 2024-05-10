-- tailwind-tools.lua
return {
  "luckasRanarison/tailwind-tools.nvim",
  dependencies = { "nvim-treesitter/nvim-treesitter" },
  enabled = false,
  opts = {
    conceal = {
      enabled = false, -- can be toggled by commands
      min_length = nil, -- only conceal classes exceeding the provided length
      symbol = "󱏿", -- only a single character is allowed
      highlight = { -- extmark highlight options, see :h 'highlight'
        fg = "#38BDF8",
      },
    },
  }, -- your configuration
}
