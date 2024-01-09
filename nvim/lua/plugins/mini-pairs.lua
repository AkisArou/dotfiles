return {
  "echasnovski/mini.pairs",
  enabled = false,
  enable = false,
  opts = {
    mappings = {
      ["<"] = { action = "open", pair = "<>", neigh_pattern = "[^\\]." },
      [">"] = { action = "close", pair = "<>", neigh_pattern = "[^\\]." },
    },
  },
}
