return {
  "nvimtools/none-ls.nvim", -- configure formatters & linters
  lazy = true,
  event = { "BufReadPre", "BufNewFile" }, -- to enable uncomment this
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

    -- for conciseness
    local formatting = null_ls.builtins.formatting -- to setup formatters
    local diagnostics = null_ls.builtins.diagnostics -- to setup linters
    -- local code_actions = null_ls.builtins.code_actions -- to setup linters

    -- to setup format on save
    local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

    -- configure null_ls
    null_ls.setup({
      -- add package.json as identifier for root (for typescript monorepos)
      root_dir = null_ls_utils.root_pattern(".git"),
      -- setup formatters & linters
      sources = {
        --  to disable file types use
        --  "formatting.prettier.with({disabled_filetypes: {}})" (see null-ls docs)
        formatting.stylua,
        formatting.biome.with({
          args = {
            "check",
            "--apply",
            "--linter-enabled=false",
            "--formatter-enabled=true",
            "--organize-imports-enabled=true",
            "--skip-errors",
            "--stdin-file-path",
            "$FILENAME",
          },
        }),
        -- formatting.rustywind,
        formatting.shfmt,
        -- formatting.codespell,
        formatting.clang_format,
        formatting.prismaFmt,
        diagnostics.hadolint,
        -- diagnostics.shellcheck,
        -- code_actions.shellcheck,
      },
      -- configure format on save

      on_attach = function(client, bufnr)
        if client.supports_method("textDocument/formatting") then
          vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
          vim.api.nvim_create_autocmd("BufWritePre", {
            group = augroup,
            buffer = bufnr,
            callback = function()
              -- on 0.8, you should use vim.lsp.buf.format({ bufnr = bufnr }) instead
              -- on later neovim version, you should use vim.lsp.buf.format({ async = false }) instead
              vim.cmd("TailwindSort")
              vim.lsp.buf.format({
                async = false,
                -- filter = function(cl)
                --   return cl.name == "null-ls"
                -- end,
              })
            end,
          })
        end
      end,
    })
  end,
}
