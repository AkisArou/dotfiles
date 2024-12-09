local quotes_pattern = "[%s=:]%s"

return {
  "echasnovski/mini.pairs",
  version = false,
  event = "VeryLazy",
  opts = {
    mappings = {
      ["<"] = { action = "open", pair = "<>", neigh_pattern = ".[%(]", register = { cr = false } },
      ['"'] = { action = "closeopen", pair = '""', neigh_pattern = quotes_pattern, register = { cr = false } },
      ["'"] = { action = "closeopen", pair = "''", neigh_pattern = quotes_pattern, register = { cr = false } },
      ["`"] = { action = "closeopen", pair = "``", neigh_pattern = quotes_pattern, register = { cr = false } },
    },
  },
}
