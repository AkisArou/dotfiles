local Util = require("lazyvim.util")

local file_pickers_config = {
  sorting_strategy = "ascending",
  -- layout_strategy = "flex",
  layout_config = {
    -- height = 0.95,
    prompt_position = 'top',
    -- preview_height = 0,
    preview_width = 0.2
  }
}

return {
  {
    "telescope.nvim",
    keys = {
      { "<leader>ff", Util.telescope("files", { cwd = false }), desc = "Find Files (root dir)" },
      { "<leader>fF", Util.telescope("files"),                  desc = "Find Files (cwd)" },
    },
    opts = {
      pickers = {
        git_files = file_pickers_config,
        find_files = file_pickers_config,
      },
    },
    extentions = {
      coc = {
        theme = "ivy",
        prefer_locations = true, -- always use Telescope locations to preview definitions/declarations/implementations etc
      }
    },
    dependencies = {
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        config = function()
          require("telescope").load_extension("fzf")
        end,
      },
      {
        "fannheyward/telescope-coc.nvim",
        config = function()
          require('telescope').load_extension('coc')
        end,
      }
    },
  }
}
