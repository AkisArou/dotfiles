return {
  "ray-x/lsp_signature.nvim",
  event = "VeryLazy",
  enabled = false,
  opts = {
    floating_window = false, -- show hint in a floating window, set to false for virtual text only mode
  },
  config = function(_, opts)
    require("lsp_signature").setup(opts)
  end,
}
