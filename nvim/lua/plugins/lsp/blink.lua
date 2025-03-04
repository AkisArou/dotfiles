return {
  "saghen/blink.cmp",
  -- enabled = false,
  build = "cargo build --release",
  -- version = "*",
  -- lazy = false, -- lazy loading handled internally
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
      use_nvim_cmp_as_default = true,
      nerd_font_variant = "mono",
    },

    sources = {
      default = (function()
        local basic = {
          "lsp",
          "path",
          "lazydev",
          "buffer",
          -- "ripgrep"
        }

        if vim.g.is_work then
          table.insert(basic, "commit")
        end
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

    fuzzy = {
      sorts = {
        function(a, b)
          if (a.client_name == nil or b.client_name == nil) or (a.client_name == b.client_name) then
            return
          end

          return b.client_name == "emmet_language_server"
        end,
        "score",
        "sort_text",
      },
    },

    signature = { enabled = true },
  },
  opts_extend = {
    "sources.completion.enabled_providers",
    "sources.default",
  },
}
