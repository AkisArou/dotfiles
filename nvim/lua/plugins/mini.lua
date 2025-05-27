return {
  {
    "echasnovski/mini.cursorword",
    version = false,
    config = function()
      require("mini.cursorword").setup()
    end,
  },
  {
    "echasnovski/mini.surround",
    version = false,
    config = function()
      require("mini.surround").setup()
    end,
  },
  {
    "echasnovski/mini.pairs",
    version = false,
    event = "VeryLazy",
    opts = {
      mappings = {
        ["<"] = { action = "open", pair = "<>", neigh_pattern = ".[%(]", register = { cr = false } },
      },
    },
  },
}
