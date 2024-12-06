return {
  "echasnovski/mini.cursorword",
  version = false,
  config = function()
    require("mini.cursorword").setup()

    vim.cmd([[
      hi! MiniCursorword guibg=#343b41 gui=NONE
    ]])
  end,
}
