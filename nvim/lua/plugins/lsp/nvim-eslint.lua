return {
  "esmuellert/nvim-eslint",
  config = function()
    require("nvim-eslint").setup({
      settings = {
        experimental = { useFlatConfig = true },
      },
    })
  end,
}
