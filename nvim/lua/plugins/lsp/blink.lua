return {
  "saghen/blink.cmp",
  build = "cargo build --release",
  dependencies = {
    "mikavilpas/blink-ripgrep.nvim",
    {
      "folke/lazydev.nvim",
      ft = "lua",
      opts = {
        library = {
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
      default = (function()
        local default_sources = {
          "lsp",
          "path",
          "lazydev",
          "buffer",
          -- "ripgrep"
        }

        if vim.g.is_work then
          table.insert(default_sources, "commit")
        end

        return default_sources
      end)(),

      providers = {
        lazydev = {
          name = "LazyDev",
          module = "lazydev.integrations.blink",
          score_offset = 100, -- show at a higher priority than lsp
          fallbacks = { "lsp" },
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

        commit = vim.g.is_work and {
          name = "Commit",
          module = "custom.blink-commit",
          enabled = function()
            return vim.bo.filetype == "gitcommit"
          end,
          opts = {},
        } or nil,
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
  },
  opts_extend = {
    "sources.completion.enabled_providers",
    "sources.default",
  },
}
