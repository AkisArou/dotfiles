return {
  "echasnovski/mini.indentscope",
  version = false,
  event = "VeryLazy",
  config = function()
    require("mini.indentscope").setup({
      draw = {
        animation = require("mini.indentscope").gen_animation.none(),
      },
    })

    vim.api.nvim_set_hl(0, "MiniIndentscopeSymbol", { fg = "#3F3F3F" })

    -- vim.api.nvim_create_autocmd("FileType", {
    --   pattern = {
    --     "help",
    --     "alpha",
    --     "dashboard",
    --     "neo-tree",
    --     "NvimTree",
    --     "Trouble",
    --     "trouble",
    --     "lazy",
    --     "mason",
    --     "notify",
    --     "toggleterm",
    --     "lazyterm",
    --   },
    --   callback = function()
    --     vim.b.miniindentscope_disable = true
    --   end,
    -- })
  end,
}
