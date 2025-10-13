require("lazydev").setup({
  library = {
    { path = "${3rd}/luv/library", words = { "vim%.uv" } },
  },
})

require("blink-cmp").setup({
  keymap = {
    preset = "default",
    ["<C-e>"] = { "select_and_accept" },
    ["<C-b>"] = { "scroll_documentation_up", "fallback" },
    ["<C-f>"] = { "scroll_documentation_down", "fallback" },
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
      -- "ripgrep"
    },

    providers = {
      lazydev = {
        name = "LazyDev",
        module = "lazydev.integrations.blink",
        score_offset = 100, -- show at a higher priority than lsp
        fallbacks = { "lsp" },
      },
    },
  },

  completion = {
    menu = {
      draw = {
        treesitter = { "lsp" },
      },
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
