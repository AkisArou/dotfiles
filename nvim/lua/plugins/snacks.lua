return {
  "folke/snacks.nvim",
  ---@type snacks.Config
  opts = {
    styles = {
      input = {
        backdrop = true,
        relative = "cursor",
        border = "solid",
        title_pos = "left",
        width = 40,
        keys = {
          c_c = "cancel",
        },
      },
    },
    bigfile = { enabled = false },
    dashboard = { enabled = false },
    indent = { enabled = false },
    input = { enabled = true, icon = "" },
    notifier = { enabled = false },
    quickfile = { enabled = false },
    scroll = { enabled = false },
    statuscolumn = { enabled = false },
    words = { enabled = false },
  },
}
