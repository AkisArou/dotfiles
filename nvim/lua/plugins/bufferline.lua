return {
  "akinsho/bufferline.nvim",
  version = "*",
  dependencies = "nvim-tree/nvim-web-devicons",
  config = function()
    require("bufferline").setup({
      options = {
        diagnostics = "nvim_lsp",
        always_show_bufferline = true,
      },
    })

    vim.cmd("highlight! link BufferLineFill Normal")
  end,
}
