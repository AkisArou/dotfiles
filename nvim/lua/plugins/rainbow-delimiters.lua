return {
  "hiphish/rainbow-delimiters.nvim",
  enabled = false,
  config = function()
    vim.g.rainbow_delimiters = {
      query = {
        javascript = "rainbow-parens",
        tsx = "rainbow-parens",
      },
    }
  end,
}
