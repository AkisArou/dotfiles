return {
  "hrsh7th/nvim-cmp",
  -- lazy = true,
  dependencies = {
    -- "luckasRanarison/tailwind-tools.nvim",
    "onsails/lspkind-nvim",
    "hrsh7th/cmp-buffer",
    "saadparwaiz1/cmp_luasnip",
    {
      "David-Kunz/cmp-npm",
      dependencies = { "nvim-lua/plenary.nvim" },
      lazy = false,
      ft = "json",
      config = function()
        require("cmp-npm").setup({
          ignore = {},
          -- only_semantic_versions = true,
          only_latest_version = true,
        })
      end,
    },
    {
      "hrsh7th/cmp-path",
      config = function()
        require("cmp").setup({
          sources = {
            { name = "path" },
          },
        })
      end,
    },
    { "davidsierradz/cmp-conventionalcommits" },
  },
  opts = function()
    vim.api.nvim_set_hl(0, "CmpGhostText", { link = "Comment", default = true })
    local cmp = require("cmp")
    local defaults = require("cmp.config.default")()

    local cmp_kinds = {
      Text = "  ",
      Method = "  ",
      Function = "  ",
      Constructor = "  ",
      Field = "  ",
      Variable = "  ",
      Class = "  ",
      Interface = "  ",
      Module = "  ",
      Property = "  ",
      Unit = "  ",
      Value = "  ",
      Enum = "  ",
      Keyword = "  ",
      Snippet = "  ",
      Color = "  ",
      File = "  ",
      Reference = "  ",
      Folder = "  ",
      EnumMember = "  ",
      Constant = "  ",
      Struct = "  ",
      Event = "  ",
      Operator = "  ",
      TypeParameter = "  ",
    }

    return {
      formatting = {
        format = function(entry, vim_item)
          vim_item.kind = (cmp_kinds[vim_item.kind] or "") .. vim_item.kind

          if entry.source.name == "npm" then
            vim_item.kind = "NPM"
          end

          return vim_item
        end,
      },
      completion = {
        completeopt = "menu,menuone,noinsert",
      },
      snippet = {
        expand = function(args)
          require("luasnip").lsp_expand(args.body)
        end,
      },
      mapping = cmp.mapping.preset.insert({
        ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
        ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<C-e>"] = cmp.mapping.abort(),
        ["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        ["<Tab>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        ["<S-CR>"] = cmp.mapping.confirm({
          behavior = cmp.ConfirmBehavior.Replace,
          select = true,
        }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        ["<C-CR>"] = function(fallback)
          cmp.abort()
          fallback()
        end,
      }),
      sources = cmp.config.sources({
        { name = "nvim_lsp" },
        { name = "path" },
        { name = "luasnip" },
        { name = "buffer" },
        { name = "conventionalcommits" },
        { name = "npm", keyword_length = 4 },
      }),
      -- experimental = {
      --   ghost_text = {
      --     hl_group = "CmpGhostText",
      --   },
      -- },
      sorting = defaults.sorting,
    }
  end,
}
