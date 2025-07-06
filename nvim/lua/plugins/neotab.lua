require("neotab").setup({
  -- configuration goes here
  smart_punctuators = {
    enabled = true,
    semicolon = {
      enabled = true,
      ft = { "javascript", "typescript", "javascriptreact", "typescriptreact", "rust" },
    },
    escape = {
      enabled = true,
      triggers = { ---@type table<string, ntab.trigger>
        -- [','] = {
        -- 	pairs = {
        -- 		{ open = "'", close = "'" },
        -- 		{ open = '"', close = '"' },
        -- 		{ open = '{', close = '}' },
        -- 		{ open = '[', close = ']' },
        -- 	},
        -- 	format = '%s ', -- ", "
        -- },
        ["="] = {
          pairs = {
            { open = "(", close = ")" },
          },
          ft = { "javascript", "typescript" },
          format = " %s> ", -- ` => `
          -- string.match(text_between_pairs, cond)
          cond = "^$", -- match only pairs with empty content
        },
      },
    },
  },
})
