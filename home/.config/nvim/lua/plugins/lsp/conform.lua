require("conform")

local util = require("conform.util")
local root_file = util.root_file({ ".git" })
local conform = require("conform")

---@param bufnr integer
local function prettier_compatible_formatter(bufnr)
  if vim.fs.root(bufnr, { ".oxfmtrc.json", ".oxfmtrc.jsonc", "oxfmt.config.ts" }) then
    return { name = "oxfmt" } -- oxfmt formats through its LSP, without other LSP formatters
  end

  return { "prettierd" }
end

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
    rust = { "rustfmt" },
  },
  default_format_opts = {
    lsp_format = "fallback",
    timeout_ms = 1000,
  },
  formatters = {
    prettierd = {
      cwd = root_file,
    },
  },
})
