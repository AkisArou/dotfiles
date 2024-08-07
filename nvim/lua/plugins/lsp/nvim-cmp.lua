return {
  "hrsh7th/nvim-cmp",
  dependencies = {
    "onsails/lspkind-nvim",
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-cmdline",
    "saadparwaiz1/cmp_luasnip",
  },
  config = function()
    vim.api.nvim_set_hl(0, "CmpGhostText", { link = "Comment", default = true })

    local cmp = require("cmp")

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

    local default_cmp_sources = cmp.config.sources({
      { name = "nvim_lsp", group_index = 1 },
      {
        name = "path",
        option = {
          trailing_slash = true,
        },
        group_index = 1,
      },
      {
        name = "buffer",
        Keyword_length = 10,
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

    vim.api.nvim_create_augroup("buffercmp", { clear = true })

    vim.api.nvim_create_autocmd("BufEnter", {
      callback = function()
        if vim.bo.filetype == "typescriptreact" or vim.bo.filetype == "typescript" then
          local sources = default_cmp_sources
          for i = #sources, 1, -1 do
            if sources[i].name == "buffer" then
              table.remove(sources, i)
            end
          end

          cmp.setup.buffer({
            sources = sources,
          })
        else
          cmp.setup.buffer({
            sources = default_cmp_sources,
          })
        end
      end,
      group = "buffercmp",
    })

    local mapping = {
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
    }

    local preselect = cmp.PreselectMode.Item

    cmp.setup({
      preselect = preselect,
      completion = {
        completeopt = "menu,menuone,noinsert",
      },
      ---@diagnostic disable-next-line: missing-fields
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
      mapping = cmp.mapping.preset.insert(mapping),
    })

    -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
    cmp.setup.cmdline({ "/", "?" }, {
      preselect = preselect,
      sources = {
        { name = "buffer" },
      },
      mapping = cmp.mapping.preset.cmdline(mapping),
    })

    -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
    cmp.setup.cmdline(":", {
      preselect = preselect,
      sources = cmp.config.sources({
        { name = "path" },
      }, {
        { name = "cmdline" },
      }),
      ---@diagnostic disable-next-line: missing-fields
      matching = { disallow_symbol_nonprefix_matching = false },
      mapping = cmp.mapping.preset.cmdline(mapping),
    })
  end,
}
