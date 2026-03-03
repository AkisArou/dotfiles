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
