require("conform")

local util = require("conform.util")
local root_file = util.root_file({ ".git" })
local conform = require("conform")

local oxfmt = "oxfmt"
local prettierd = "oxfmt"
local shfmt = "shfmt"
local shellcheck = "shellcheck"

conform.setup({
  notify_on_error = false,
  formatters_by_ft = {
    lua = { "stylua" },
    javascript = { oxfmt, prettierd },
    typescript = { oxfmt, prettierd },
    javascriptreact = { oxfmt, prettierd },
    typescriptreact = { oxfmt, prettierd },
    json = { oxfmt, prettierd },
    jsonc = { oxfmt, prettierd },
    html = { oxfmt, prettierd },
    css = { oxfmt, prettierd },
    yaml = { oxfmt, prettierd },
    markdown = { oxfmt, prettierd },
    ["markdown.mdx"] = { oxfmt, prettierd },
    zsh = { shfmt, shellcheck },
    sh = { shfmt, shellcheck },
  },
  format_on_save = {
    lsp_format = "fallback",
    timeout_ms = 1000,
  },
  formatters = {
    typescript = {
      cwd = root_file,
    },
    prettierd = {
      cwd = root_file,
    },
  },
})
