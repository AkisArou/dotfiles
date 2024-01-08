local Util = require("lazyvim.util")

return {
  {
    "telescope.nvim",
    keys = {
      { "<leader>ff", Util.telescope("files", { cwd = false }), desc = "Find Files (root dir)" },
      { "<leader>fF", Util.telescope("files"),                  desc = "Find Files (cwd)" },
    },
    extentions = {
      coc = {
        theme = 'ivy',
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
