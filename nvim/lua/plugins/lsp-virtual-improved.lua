return {
  "luozhiya/lsp-virtual-improved.nvim",
  event = { "LspAttach" },
  config = function()
    require("lsp-virtual-improved").setup()
  end,
}
