return {
  "echasnovski/mini.ai",
  event = "VeryLazy",
  opts = function()
    local ai = require("mini.ai")
    return {
      n_lines = 500,
      custom_textobjects = {
        b = false,
        f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }), -- function
        t = { "<([%p%w]-)%f[^<%w][^<>]->.-</%1>", "^<.->().*()</[^/]->$" }, -- tags
      },
    }
  end,
  config = function(_, opts)
    require("mini.ai").setup(opts)
  end,
}
