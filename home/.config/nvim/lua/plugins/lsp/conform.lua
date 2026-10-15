require("conform")

local util = require("conform.util")
local root_file = util.root_file({ ".git" })
local conform = require("conform")

local prettier_compatible_formatter = (function()
  local has_oxfmt = util.root_file({
    ".oxfmtrc.json",
    ".oxfmtrc.jsonc",
  })

  if has_oxfmt then
    return {} -- use LSP only
  else
    return { "prettierd" }
  end
end)()

conform.setup({
  notify_on_error = false,
  formatters_by_ft = {
    lua = { "stylua" },
    javascript = prettier_compatible_formatter,
    typescript = prettier_compatible_formatter,
    javascriptreact = prettier_compatible_formatter,
    typescriptreact = prettier_compatible_formatter,
    json = prettier_compatible_formatter,
    jsonc = prettier_compatible_formatter,
    html = prettier_compatible_formatter,
    css = prettier_compatible_formatter,
    yaml = prettier_compatible_formatter,
    markdown = prettier_compatible_formatter,
    ["markdown.mdx"] = prettier_compatible_formatter,
    zsh = { "shfmt", "shellcheck" },
    sh = { "shfmt", "shellcheck" },
    rust = { "rustfmt", lsp_format = "fallback" },
  },
  format_on_save = {
    lsp_format = "fallback",
    timeout_ms = 1000,
  },
  formatters = {
    prettierd = {
      cwd = root_file,
    },
  },
})
