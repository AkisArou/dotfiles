return {
  "saghen/blink.cmp",
  lazy = false, -- lazy loading handled internally
  -- optional: provides snippets for the snippet source
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

  -- use a release tag to download pre-built binaries
  version = "v0.*",
  -- OR build from source, requires nightly: https://rust-lang.github.io/rustup/concepts/channels.html#working-with-nightly-rust
  -- build = 'cargo build --release',
  -- If you use nix, you can build from source using latest nightly rust with:
  -- build = 'nix run .#build-plugin',

  ---@module 'blink.cmp'
  ---@type blink.cmp.Config
  opts = {
    -- 'default' for mappings similar to built-in completion
    -- 'super-tab' for mappings similar to vscode (tab to accept, arrow keys to navigate)
    -- 'enter' for mappings similar to 'super-tab' but with 'enter' to accept
    -- see the "default configuration" section below for full documentation on how to define
    -- your own keymap.
    keymap = { preset = "default", ["<C-e>"] = { "select_and_accept" } },

    appearance = {
      -- Sets the fallback highlight groups to nvim-cmp's highlight groups
      -- Useful for when your theme doesn't support blink.cmp
      -- will be removed in a future release
      use_nvim_cmp_as_default = true,
      -- Set to 'mono' for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
      -- Adjusts spacing to ensure icons are aligned
      nerd_font_variant = "mono",
    },

    -- default list of enabled providers defined so that you can extend it
    -- elsewhere in your config, without redefining it, via `opts_extend`
    sources = {
      completion = {
        enabled_providers = {
          "lsp",
          "path",
          "lazydev",
          "buffer",
          -- "ripgrep", -- 👈🏻 add "ripgrep" here
        },
      },
      providers = {
        -- dont show LuaLS require statements when lazydev has items
        lsp = { fallback_for = { "lazydev" } },
        lazydev = { name = "LazyDev", module = "lazydev.integrations.blink" },
        ripgrep = {
          module = "blink-ripgrep",
          name = "Ripgrep",
          -- the options below are optional, some default values are shown
          ---@module "blink-ripgrep"
          ---@type blink-ripgrep.Options
          opts = {
            -- For many options, see `rg --help` for an exact description of
            -- the values that ripgrep expects.

            -- the minimum length of the current word to start searching
            -- (if the word is shorter than this, the search will not start)
            prefix_min_len = 3,

            -- The number of lines to show around each match in the preview window
            context_size = 5,

            -- The maximum file size that ripgrep should include in its search.
            -- Useful when your project contains large files that might cause
            -- performance issues.
            -- Examples: "1024" (bytes by default), "200K", "1M", "1G"
            max_filesize = "1M",
          },
        },
      },
    },

    -- experimental auto-brackets support
    completion = {
      -- accept = { auto_brackets = { enabled = true } },
      documentation = {
        -- Controls whether the documentation window will automatically show when selecting a completion item
        auto_show = true,
        auto_show_delay_ms = 200,
      },
    },

    -- experimental signature help support
    signature = { enabled = true },
  },
  -- allows extending the enabled_providers array elsewhere in your config
  -- without having to redefine it
  opts_extend = { "sources.completion.enabled_providers" },
}
