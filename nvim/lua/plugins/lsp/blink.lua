require("lazydev").setup({
  library = {
    { path = "${3rd}/luv/library", words = { "vim%.uv" } },
  },
})

local select_count = 10

require("blink-cmp").setup({
  keymap = {
    preset = "default",
    ["<C-e>"] = { "select_and_accept" },
    ["<C-b>"] = { "scroll_documentation_up", "fallback" },
    ["<C-f>"] = { "scroll_documentation_down", "fallback" },
    ["<C-d>"] = {
      function(cmp)
        return cmp.select_next({ count = select_count })
      end,
      "fallback",
    },
    ["<C-u>"] = {
      function(cmp)
        return cmp.select_prev({ count = select_count })
      end,
      "fallback",
    },
    ["<C-k>"] = { "fallback", "show_signature", "hide_signature" },
  },

  appearance = {
    nerd_font_variant = "mono",
  },

  sources = {
    default = {
      "lsp",
      "path",
      "lazydev",
      "buffer",
      "dap",
    },

    providers = {
      -- lsp = {
      --   name = "LSP",
      --   module = "blink.cmp.sources.lsp",
      --   transform_items = function(ctx, items)
      --     if vim.bo[ctx.bufnr].filetype ~= "typescriptreact" then
      --       return items
      --     end
      --
      --     return vim.tbl_filter(function(item)
      --       if item.client_name ~= "emmet_language_server" then
      --         return true
      --       end
      --
      --       local node = vim.treesitter.get_node()
      --       while node do
      --         local t = node:type()
      --         if
      --           t == "jsx_element"
      --           or t == "jsx_self_closing_element"
      --           or t == "jsx_opening_element"
      --           or t == "jsx_fragment"
      --           or t == "jsx_expression"
      --         then
      --           return true
      --         end
      --         node = node:parent()
      --       end
      --       return false
      --     end, items)
      --   end,
      -- },

      lazydev = {
        name = "LazyDev",
        module = "lazydev.integrations.blink",
        score_offset = 100, -- show at a higher priority than lsp
        fallbacks = { "lsp" },
      },
      dap = {
        name = "Dap",
        module = "blink-cmp-dap",
        enabled = function()
          return vim.tbl_contains({ "dap-repl", "dapui_watches", "dapui_hover" }, vim.bo.filetype)
        end,
        opts = {},
      },
    },
  },

  completion = {
    menu = {
      draw = vim.g.disable_blink_treesitter and {} or { treesitter = { "lsp" } },
    },
    accept = { auto_brackets = { enabled = false } },
    documentation = {
      auto_show = true,
      auto_show_delay_ms = 200,
    },
  },

  cmdline = {
    keymap = { preset = "default", ["<C-e>"] = { "select_and_accept" } },
    completion = {
      menu = {
        auto_show = true,
      },
    },
  },

  signature = { enabled = true },
})
