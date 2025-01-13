return {
  "stevearc/conform.nvim",
  config = function()
    local util = require("conform.util")
    local root_file = util.root_file({ ".git" })
    local conform = require("conform")

    local prisma = {
      command = util.from_node_modules("prisma"),
      stdin = false,
      args = { "format", "--schema", "$FILENAME" },
    }

    conform.setup({
      notify_on_error = false,
      formatters_by_ft = {
        lua = { "stylua" },
        javascript = { "prettierd" },
        typescript = { "prettierd" },
        javascriptreact = { "prettierd" },
        typescriptreact = { "prettierd" },
        json = { "prettierd" },
        jsonc = { "prettierd" },
        html = { "prettierd" },
        css = { "prettierd" },
        yaml = { "prettierd" },
        markdown = { "prettierd" },
        ["markdown.mdx"] = { "prettierd" },
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
        typescript = {
          cwd = root_file,
        },
        prettierd = {
          cwd = root_file,
        },
      },
    })
  end,
}
