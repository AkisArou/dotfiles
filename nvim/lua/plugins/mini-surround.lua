return {
  "echasnovski/mini.surround",
  version = false,
  event = "VeryLazy",
  keys = {
    { "gsa", desc = "Add surrounding", mode = { "n", "v" } },
    { "gsd", desc = "Delete surrounding" },
    { "gsf", desc = "Find right surrounding" },
    { "gsF", desc = "Find left surrounding" },
    { "gsh", desc = "Highlight surrounding" },
    { "gsr", desc = "Replace surrounding" },
  },
  opts = {
    mappings = {
      add = "gsa", -- Add surrounding in Normal and Visual modes
      delete = "gsd", -- Delete surrounding
      find = "gsf", -- Find surrounding (to the right)
      find_left = "gsF", -- Find surrounding (to the left)
      highlight = "gsh", -- Highlight surrounding
      replace = "gsr", -- Replace surrounding
    },
  },
}
