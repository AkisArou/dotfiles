return {
  "echasnovski/mini.pairs",
  event = "VeryLazy",
  opts = {
    mappings = {
      ["<"] = { action = "open", pair = "<>", neigh_pattern = "[^\\]." },
      [">"] = { action = "close", pair = "<>", neigh_pattern = "[^\\]." },
    },
  },
}
