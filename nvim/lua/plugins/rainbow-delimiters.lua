return {
  "hiphish/rainbow-delimiters.nvim",
  config = function()
    vim.g.rainbow_delimiters = {
      query = {
        javascript = "rainbow-parens",
        tsx = "rainbow-parens",
      },
    }
  end,
}
