local Util = require("lazyvim.util")

return {
  "telescope.nvim",
  keys = {
    { "<leader>ff", Util.telescope("files", { cwd = false }), desc = "Find Files (root dir)" },
    { "<leader>fF", Util.telescope("files"), desc = "Find Files (cwd)" },
  },
  dependencies = {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "make",
    config = function()
      require("telescope").load_extension("fzf")
    end,
  },
}
