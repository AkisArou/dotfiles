return {
  "stevearc/conform.nvim",
  config = function()
    local root_file = require("conform.util").root_file({ ".git" })

    require("conform").setup({
      notify_on_error = false,
      formatters_by_ft = {
        lua = { "stylua" },
        javascript = { { "biome", "prettier" } },
        typescript = { { "biome", "prettier" } },
        javascriptreact = { { "biome", "prettier" }, "rustywind" },
        typescriptreact = { { "biome", "prettier" }, "rustywind" },
        json = { { "biome", "prettier" } },
        jsonc = { { "biome", "prettier" } },
        html = { "prettier" },
        css = { "prettier" },
        yaml = { "prettier" },
        markdown = { "prettier" },
        ["markdown.mdx"] = { "prettier" },
      },
      format_on_save = {
        lsp_format = "fallback",
        timeout_ms = 500,
      },
      formatters = {
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
