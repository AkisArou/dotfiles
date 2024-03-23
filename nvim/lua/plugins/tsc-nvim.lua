return {
  "dmmulroy/tsc.nvim",
  event = "VimEnter",
  config = function()
    require("tsc").setup({
      use_trouble_qflist = true,
      auto_start_watch_mode = true,
      flags = {
        project = false,
        watch = true,
        build = true,
        noEmit = false,
      },
    })
  end,
}
