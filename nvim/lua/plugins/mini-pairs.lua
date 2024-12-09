-- return {
--   "windwp/nvim-autopairs",
--   event = "InsertEnter",
--   config = true,
--   -- use opts = {} for passing setup options
--   -- this is equivalent to setup({}) function
-- }

return {
  "echasnovski/mini.pairs",
  enabled = false,
  version = false,
  event = "VeryLazy",
  opts = {
    mappings = {
      -- ['"'] = { action = "closeopen", pair = '""', neigh_pattern = "[^%a\\].", register = { cr = false } },
      -- ["<"] = { action = "open", pair = "<>", neigh_pattern = ".[%(]", register = { cr = false } },

      ['"'] = {
        action = "closeopen",
        pair = '""',
        neigh_pattern = " ", -- Only allowed when no neighbors or only spaces.
        register = { cr = false },
      },
    },
  },
}
