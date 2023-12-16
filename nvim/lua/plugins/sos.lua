return {
  "tmillr/sos.nvim",
  branch = "master",
  event = "VimEnter",
  config = function()
    require("sos").setup({
      enabled = true,
      timeout = 20000,
      autowrite = true,
      save_on_cmd = "some",
      save_on_bufleave = true,
      save_on_focuslost = true,
    })
  end,
}
