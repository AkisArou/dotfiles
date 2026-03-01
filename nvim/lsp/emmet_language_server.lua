---@brief
---
--- https://github.com/olrtg/emmet-language-server

---@type vim.lsp.Config
return {
  cmd = { "emmet-language-server", "--stdio" },
  filetypes = {
    "astro",
    "css",
    "html",
    "javascriptreact",
    "typescriptreact",
  },
  root_markers = { ".git" },
}
