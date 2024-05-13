return {
  "pmizio/typescript-tools.nvim",
  enabled = false,
  dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
  lazy = true,
  opts = {},
  config = function()
    require("typescript-tools").setup({
      root_dir = require("lspconfig.util").root_pattern(".git"),
      settings = {
        tsserver_max_memory = 10000,
        tsserver_file_preferences = {
          includeInlayParameterNameHints = "literal",
          includeInlayParameterNameHintsWhenArgumentMatchesName = false,
          includeInlayFunctionParameterTypeHints = true,
          includeInlayVariableTypeHints = false,
          includeInlayPropertyDeclarationTypeHints = true,
          includeInlayFunctionLikeReturnTypeHints = true,
          includeInlayEnumMemberValueHints = true,
        },
      },
    })
  end,
}
