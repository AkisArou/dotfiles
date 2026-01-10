-- require("mini.icons").setup()

require("mini.cursorword").setup()

require("mini.surround").setup()

require("mini.pairs").setup({
  mappings = {
    ["<"] = { action = "open", pair = "<>", neigh_pattern = ".[%(]", register = { cr = false } },
  },
})

require("mini.ai").setup({
  custom_textobjects = {
    b = false,
  },
})
