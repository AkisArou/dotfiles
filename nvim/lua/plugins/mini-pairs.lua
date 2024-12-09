return {
  "windwp/nvim-autopairs",
  event = "InsertEnter",
  config = function()
    require("nvim-autopairs").setup({})

    local Rule = require("nvim-autopairs.rule")
    local npairs = require("nvim-autopairs")
    local cond = require("nvim-autopairs.conds")

    npairs.add_rule(Rule("<", ">", {
      -- if you use nvim-ts-autotag, you may want to exclude these filetypes from this rule
      -- so that it doesn't conflict with nvim-ts-autotag
      "-html",
      "-javascriptreact",
      "-typescriptreact",
    }):with_pair(
      -- regex will make it so that it will auto-pair on
      -- `a<` but not `a <`
      -- The `:?:?` part makes it also
      -- work on Rust generics like `some_func::<T>()`
      cond.before_regex("%a+:?:?$", 3)
    ):with_move(function(opts)
      return opts.char == ">"
    end))

    -- npairs.add_rule(Rule('"', '"'):with_pair(cond.before_text(" ")):with_move(function(opts)
    --   return opts.char == ""
    -- end))
  end,
}
-- return {
--   "echasnovski/mini.pairs",
--   version = false,
--   event = "VeryLazy",
--   opts = {
--     mappings = {
--       ['"'] = { action = "closeopen", pair = '""', neigh_pattern = "[^%a\\].", register = { cr = false } },
--       ["<"] = { action = "open", pair = "<>", neigh_pattern = ".[%(]", register = { cr = false } },
--     },
--   },
-- }
