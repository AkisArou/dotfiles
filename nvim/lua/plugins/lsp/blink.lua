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
      "git",
      -- "ripgrep"
    },

    per_filetype = {
      org = { "orgmode" },
    },

    providers = {
      lazydev = {
        name = "LazyDev",
        module = "lazydev.integrations.blink",
        score_offset = 100, -- show at a higher priority than lsp
        fallbacks = { "lsp" },
      },
      git = {
        module = "blink-cmp-git",
        name = "Git",
        enabled = function()
          return vim.tbl_contains({ "octo", "gitcommit" }, vim.bo.filetype)
        end,
        opts = {},
      },
      orgmode = {
        name = "Orgmode",
        module = "orgmode.org.autocompletion.blink",
        fallbacks = { "buffer" },
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
