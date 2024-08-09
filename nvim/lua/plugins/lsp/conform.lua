return {
  "stevearc/conform.nvim",
  config = function()
    local util = require("conform.util")
    local root_file = util.root_file({ ".git" })
    local conform = require("conform")

    local format_react = function(bufnr)
      if conform.get_formatter_info("biome", bufnr).available then
        return { "biome", "rustywind" }
      else
        return { "prettier", "rustywind" }
      end
    end

    local prisma = {
      command = util.from_node_modules("prisma"),
      stdin = false,
      args = { "format", "--schema", "$FILENAME" },
    }

    conform.setup({
      notify_on_error = false,
      formatters_by_ft = {
        lua = { "stylua" },
        javascript = { "biome", "prettier", stop_after_first = true },
        typescript = { "biome", "prettier", stop_after_first = true },
        javascriptreact = format_react,
        typescriptreact = format_react,
        json = { "biome", "prettier", stop_after_first = true },
        jsonc = { "biome", "prettier", stop_after_first = true },
        html = { "prettier" },
        css = { "prettier" },
        yaml = { "prettier" },
        markdown = { "prettier" },
        ["markdown.mdx"] = { "prettier" },
        prisma = { "prisma" },
        zsh = { "shfmt", "shellcheck" },
        sh = { "shfmt", "shellcheck" },
      },
      format_on_save = {
        lsp_format = "fallback",
        timeout_ms = 1000,
      },
      formatters = {
        prisma = prisma,
        biome = {
          cwd = root_file,
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
        },
        typescript = {
          cwd = root_file,
        },
        rustywind = {
          cwd = root_file,
        },
        prettier = {
          cwd = root_file,
        },
      },
    })
  end,
}
