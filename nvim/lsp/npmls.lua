--- @type vim.lsp.Config
return {
  cmd = { "npx", "npm-workspaces-language-server", "--stdio" },
  filetypes = { "json", "packagejson" },
  root_markers = { ".git", "package.json" },
}
