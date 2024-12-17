return {
  "saghen/blink.cmp",
  version = "*",
  lazy = false, -- lazy loading handled internally
  dependencies = {
    { "mikavilpas/blink-ripgrep.nvim" },
    {
      "folke/lazydev.nvim",
      ft = "lua", -- only load on lua files
      opts = {
        library = {
          -- See the configuration section for more details
          -- Load luvit types when the `vim.uv` word is found
          { path = "${3rd}/luv/library", words = { "vim%.uv" } },
        },
      },
    },
  },

  opts = {
    keymap = { preset = "default", ["<C-e>"] = { "select_and_accept" } },

    appearance = {
      nerd_font_variant = "mono",
    },

    sources = {
      default = { "lsp", "path", "lazydev", "buffer", "ripgrep" },

      providers = {
        lsp = { fallback_for = { "lazydev" } },

        lazydev = {
          name = "LazyDev",
          module = "lazydev.integrations.blink",
          score_offset = 100, -- show at a higher priority than lsp
        },

        ripgrep = {
          module = "blink-ripgrep",
          name = "Ripgrep",
          -- the options below are optional, some default values are shown
          ---@module "blink-ripgrep"
          ---@type blink-ripgrep.Options
          opts = {
            prefix_min_len = 3,
            context_size = 5,
            max_filesize = "1M",
          },
        },
      },
    },

    completion = {
      menu = {
        draw = {
          treesitter = { "lsp" },
        },
      },
      -- accept = { auto_brackets = { enabled = true } },
      documentation = {
        auto_show = true,
        auto_show_delay_ms = 200,
      },
    },

    signature = { enabled = true },
  },
  opts_extend = {
    "sources.completion.enabled_providers",
    "sources.default",
  },
}
