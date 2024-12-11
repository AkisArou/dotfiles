return {
  "iguanacucumber/magazine.nvim",
  enabled = true,
  name = "nvim-cmp",
  dependencies = {
    { "iguanacucumber/mag-buffer", name = "cmp-buffer" },
    { "iguanacucumber/mag-cmdline", name = "cmp-cmdline" },
    { "https://codeberg.org/FelipeLema/cmp-async-path" },
  },
  event = { "BufEnter" },
  config = function()
    local cmp = require("cmp")

    local cmdlineMapping = {
      ["<C-e>"] = {
        c = function()
          cmp.confirm({
            select = true,
          })
        end,
      },
      ["<C-n>"] = {
        c = function()
          if cmp.visible() then
            cmp.select_next_item()
          end
        end,
      },
      ["<C-p>"] = {
        c = function()
          if cmp.visible() then
            cmp.select_prev_item()
          end
        end,
      },
    }

    -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
    cmp.setup.cmdline({ "/", "?" }, {
      sources = {
        { name = "buffer" },
      },
      mapping = cmp.mapping.preset.cmdline(cmdlineMapping),
    })

    -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
    cmp.setup.cmdline(":", {
      sources = cmp.config.sources({
        { name = "async_path" },
      }, {
        { name = "cmdline" },
      }),
      matching = { disallow_symbol_nonprefix_matching = false },
      mapping = cmp.mapping.preset.cmdline(cmdlineMapping),
    })
  end,
}
