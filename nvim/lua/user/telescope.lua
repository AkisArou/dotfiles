local M = {
  "nvim-telescope/telescope.nvim",
  commit = "40c31fdde93bcd85aeb3447bb3e2a3208395a868",
  event = "BufEnter",
  cmd = { "Telescope" },
  dependencies = {
    {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = 'make'
    },
    {
      "nvim-lua/plenary.nvim",
      commit = "9a0d3bf7b832818c042aaf30f692b081ddd58bd9",
    },
    {
      "fannheyward/telescope-coc.nvim"
    }
  }
}


-- M.opts = {
--   defaults = {
--     prompt_prefix = " ",
--     selection_caret = " ",
--     path_display = { "smart" },
--     file_ignore_patterns = { ".git/", "node_modules" },
--     mappings = {
--       i = {
--         ["<Down>"] = actions.move_selection_next,
--         ["<Up>"] = actions.move_selection_previous,
--         ["<C-j>"] = actions.move_selection_next,
--         ["<C-k>"] = actions.move_selection_previous,
--       },
--     },
--   },
--   extensions = {
--     fzf = {
--       fuzzy = true,                    -- false will only do exact matching
--       override_generic_sorter = true,  -- override the generic sorter
--       override_file_sorter = true,     -- override the file sorter
--       case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
--                                        -- the default case_mode is "smart_case"
--     },
--     coc = {
--         theme = 'ivy',
--         prefer_locations = true, -- always use Telescope locations to preview definitions/declarations/implementations etc
--     }
--   }
-- }

function M.config()
  local actions = require "telescope.actions"

  require("telescope").setup({
  defaults = {
    prompt_prefix = " ",
    selection_caret = " ",
    path_display = { "smart" },
    file_ignore_patterns = { ".git/", "node_modules" },
    mappings = {
      i = {
        ["<Down>"] = actions.move_selection_next,
        ["<Up>"] = actions.move_selection_previous,
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
      },
    },
  },
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    },
    coc = {
        theme = 'ivy',
        prefer_locations = true, -- always use Telescope locations to preview definitions/declarations/implementations etc
    }
  }
  })
	-- require('telescope').load_extension('fzf')
	require('telescope').load_extension('coc')
end


return M
