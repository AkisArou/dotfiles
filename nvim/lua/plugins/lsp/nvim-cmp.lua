return {
  "hrsh7th/nvim-cmp",
  dependencies = {
    "onsails/lspkind-nvim",
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-nvim-lsp-signature-help",
    "saadparwaiz1/cmp_luasnip",
    "hrsh7th/cmp-path",
  },
  opts = function()
    vim.api.nvim_set_hl(0, "CmpGhostText", { link = "Comment", default = true })
    local cmp = require("cmp")
    -- local defaults = require("cmp.config.default")()

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

    local sources = cmp.config.sources({
      { name = "nvim_lsp", group_index = 1 },
      { name = "nvim_lsp_signature_help", group_index = 1 },
      {
        name = "path",
        option = {
          trailing_slash = true,
        },
        group_index = 1,
      },
      {
        name = "buffer",
        option = {
          get_bufnrs = function()
            local buf = vim.api.nvim_get_current_buf()
            local byte_size = vim.api.nvim_buf_get_offset(buf, vim.api.nvim_buf_line_count(buf))
            if byte_size > 1024 * 1024 then -- 1 Megabyte max
              return {}
            end
            return { buf }
          end,
        },
        group_index = 2,
      },
      { name = "luasnip", group_index = 2 },
    })

    return {
      preselect = cmp.PreselectMode.Item,
      completion = {
        completeopt = "menu,menuone,noinsert",
      },
      formatting = {
        format = function(_, vim_item)
          vim_item.kind = (cmp_kinds[vim_item.kind] or "") .. vim_item.kind

          return vim_item
        end,
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
        ["<CR>"] = cmp.mapping.confirm({ select = true }),
        ["<Tab>"] = cmp.mapping.confirm({
          select = true,
        }),
        ["<S-CR>"] = cmp.mapping.confirm({
          behavior = cmp.ConfirmBehavior.Replace,
          select = true,
        }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        ["<C-CR>"] = function(fallback)
          cmp.abort()
          fallback()
        end,
      }),
      sources = sources,
      -- experimental = {
      --   ghost_text = {
      --     hl_group = "CmpGhostText",
      --   },
      -- },
      sorting = {
        comparators = {
          cmp.config.compare.offset,
          cmp.config.compare.exact,
          cmp.config.compare.score,
          cmp.config.compare.kind,
          cmp.config.compare.sort_text,
          cmp.config.compare.length,
          cmp.config.compare.order,
        },
      },
    }
  end,
}
