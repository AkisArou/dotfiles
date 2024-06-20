return {
  "hrsh7th/nvim-cmp",
  lazy = true,
  dependencies = {
    "onsails/lspkind-nvim",
    "hrsh7th/cmp-buffer",
    "saadparwaiz1/cmp_luasnip",
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
    local buffer_cmp = {
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
    }

    local default_cmp_sources = cmp.config.sources({
      { name = "nvim_lsp" },
      { name = "path" },
      { name = "luasnip" },
      { name = "conventionalcommits" },
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
          local sources = default_cmp_sources
          sources[#sources + 1] = buffer_cmp
          cmp.setup.buffer({
            sources = sources,
          })
        end
      end,
      group = "buffercmp",
    })

    return {
      formatting = {
        format = function(_, vim_item)
          vim_item.kind = (cmp_kinds[vim_item.kind] or "") .. vim_item.kind

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
        ["<CR>"] = cmp.mapping.confirm({ select = true }),
        ["<Tab>"] = cmp.mapping.confirm({ select = true }),
        ["<S-CR>"] = cmp.mapping.confirm({
          behavior = cmp.ConfirmBehavior.Replace,
          select = true,
        }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        ["<C-CR>"] = function(fallback)
          cmp.abort()
          fallback()
        end,
      }),
      sources = default_cmp_sources,
      -- experimental = {
      --   ghost_text = {
      --     hl_group = "CmpGhostText",
      --   },
      -- },
      sorting = defaults.sorting,
    }
  end,
}
