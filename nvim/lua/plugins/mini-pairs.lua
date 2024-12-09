-- local H = {}
-- local function escape(s)
--   return vim.api.nvim_replace_termcodes(s, true, true, true)
-- end
-- H.keys = {
--   above = escape("<C-o>O"),
--   bs = escape("<bs>"),
--   cr = escape("<cr>"),
--   del = escape("<del>"),
--   keep_undo = escape("<C-g>U"),
--   -- NOTE: use `get_arrow_key()` instead of `H.keys.left` or `H.keys.right`
--   left = escape("<left>"),
--   right = escape("<right>"),
-- }
--
-- H.get_arrow_key = function(key)
--   if vim.fn.mode() == "i" then
--     -- Using left/right keys in insert mode breaks undo sequence and, more
--     -- importantly, dot-repeat. To avoid this, use 'i_CTRL-G_U' mapping.
--     return H.keys.keep_undo .. H.keys[key]
--   else
--     return H.keys[key]
--   end
-- end
--
-- -- Function to handle inserting a duplicated pair when there are no surrounding non-visible characters
-- function HandleQuote()
--   local cursor_pos = vim.api.nvim_win_get_cursor(0) -- Get cursor position
--   local col = cursor_pos[2] -- Get the column index
--   local line = vim.api.nvim_get_current_line() -- Get the current line's text
--
--   -- Check if there are no visible characters on both sides of the cursor
--   local no_left_char = col == 0 or not line:sub(col, col):match("%S") -- No non-space to the left
--   local no_right_char = col >= #line or not line:sub(col + 1, col + 1):match("%S") -- No non-space to the right
--
--   if no_left_char and no_right_char then
--     return ("%s%s"):format('""', H.get_arrow_key("left"))
--   else
--     return '"' -- Just insert the character itself
--   end
-- end
--
--
-- -- Map the `"` key to use our custom logic
-- vim.api.nvim_set_keymap("i", '"', "v:lua.HandleQuote()", { noremap = true, expr = true, silent = true })

-- return {
--   "windwp/nvim-autopairs",
--   enabled = false,
--   event = "InsertEnter",
--   config = function()
--     require("nvim-autopairs").setup({})
--
--     local Rule = require("nvim-autopairs.rule")
--     local npairs = require("nvim-autopairs")
--     local cond = require("nvim-autopairs.conds")
--
--     npairs.add_rule(Rule("<", ">", {
--       -- if you use nvim-ts-autotag, you may want to exclude these filetypes from this rule
--       -- so that it doesn't conflict with nvim-ts-autotag
--       "-html",
--       "-javascriptreact",
--       "-typescriptreact",
--     }):with_pair(
--       -- regex will make it so that it will auto-pair on
--       -- `a<` but not `a <`
--       -- The `:?:?` part makes it also
--       -- work on Rust generics like `some_func::<T>()`
--       cond.before_regex("%a+:?:?$", 3)
--     ):with_move(function(opts)
--       return opts.char == ">"
--     end))
--
--     -- npairs.add_rule(Rule('"', '"'):with_pair(cond.before_text(" ")):with_move(function(opts)
--     --   return opts.char == ""
--     -- end))
--   end,
-- }
return {
  "echasnovski/mini.pairs",
  version = false,
  event = "VeryLazy",
  opts = {
    mappings = {
      ["<"] = { action = "open", pair = "<>", neigh_pattern = ".[%(]", register = { cr = false } },
    },
  },
}
