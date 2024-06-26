return {
  "nvimtools/none-ls.nvim",
  lazy = true,
  event = { "BufReadPre", "BufNewFile" },
  keys = {
    {
      "<leader>cf",
      function()
        vim.lsp.buf.format()
      end,
      desc = "Format",
    },
  },
  config = function()
    local null_ls = require("null-ls")
    local null_ls_utils = require("null-ls.utils")

    local formatting = null_ls.builtins.formatting
    local diagnostics = null_ls.builtins.diagnostics

    -- local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

    null_ls.setup({
      root_dir = null_ls_utils.root_pattern(".git"),
      sources = {
        formatting.stylua,
        -- formatting.biome.with({
        --   args = {
        --     "check",
        --     "--apply",
        --     "--linter-enabled=false",
        --     "--formatter-enabled=true",
        --     "--organize-imports-enabled=true",
        --     "--skip-errors",
        --     "--stdin-file-path",
        --     "$FILENAME",
        --   },
        -- }),
        -- formatting.rustywind,
        -- formatting.shfmt,
        -- formatting.clang_format,
        formatting.prisma_format,
        diagnostics.hadolint,
        -- diagnostics.shellcheck,
        -- code_actions.shellcheck,
      },

      on_attach = function(client, bufnr)
        -- if client.supports_method("textDocument/formatting") then
        --   vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
        --   vim.api.nvim_create_autocmd("BufWritePre", {
        --     group = augroup,
        --     buffer = bufnr,
        --     callback = function()
        --       -- on 0.8, you should use vim.lsp.buf.format({ bufnr = bufnr }) instead
        --       -- on later neovim version, you should use vim.lsp.buf.format({ async = false }) instead
        --
        --       vim.lsp.buf.format({
        --         async = false,
        --         filter = function(cl)
        --           return cl.name == "null-ls"
        --         end,
        --       })
        --     end,
        --   })
        -- end
      end,
    })
  end,
}
