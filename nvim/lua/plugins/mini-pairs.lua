return {
  "echasnovski/mini.pairs",
  version = false,
  enabled = false,
  event = "VeryLazy",
  opts = {
    mappings = {
      ['"'] = { action = "closeopen", pair = '""', neigh_pattern = "[^%a\\].", register = { cr = false } },
      ["<"] = { action = "open", pair = "<>", neigh_pattern = ".[%(]", register = { cr = false } },
    },
  },
}
