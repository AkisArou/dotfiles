return {
  "3rd/image.nvim",
  enabled = false,
  dependencies = { "leafo/magick" },
  config = function()
    require("image").setup({
      backend = "ueberzug",
    })
  end,
}
