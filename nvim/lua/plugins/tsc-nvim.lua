return {
  "dmmulroy/tsc.nvim",
  event = "VimEnter",
  enabled = true,
  config = function()
    local cwd = vim.fn.getcwd()
    require("tsc").setup({
      use_trouble_qflist = false,
      auto_start_watch_mode = true,
      auto_close_qflist = false,
      auto_focus_qflist = false,
      -- bin_path = cwd .. " ./node-modules/.bin/tsc " .. cwd,
      raw = "--build " .. cwd .. "/tsconfig.json --watch",
      flags = {
        project = false,
        build = true,
        -- [vim.fn.getcwd() .. "tsconfig.json"] = true,
        watch = true,
        noEmit = false,
      },
    })
  end,
}
