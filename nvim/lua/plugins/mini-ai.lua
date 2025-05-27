return {
  "echasnovski/mini.ai",
  dependencies = {
    "nvim-treesitter/nvim-treesitter-textobjects",
    branch = "main",
  },
  event = "VeryLazy",
  config = function()
    local ai = require("mini.ai")

    ai.setup({
      n_lines = 500,
      custom_textobjects = {
        b = false,
        f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }), -- function
        t = { "<([%p%w]-)%f[^<%w][^<>]->.-</%1>", "^<.->().*()</[^/]->$" }, -- tags
      },
    })
  end,
}
