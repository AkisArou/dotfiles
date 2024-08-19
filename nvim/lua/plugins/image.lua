return {
  "3rd/image.nvim",
  enabled = true,
  dependencies = { "leafo/magick" },
  config = function()
    require("image").setup({
      backend = "ueberzug",
    })
  end,
}
