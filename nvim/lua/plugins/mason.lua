return {
  "williamboman/mason.nvim",
  enabled = false,
  enable = false,
  opts = {
    ensure_installed = {
      "stylua",
      "shellcheck",
      "shfmt",
      "flake8",
      "codespell",
      "html-lsp",
      "emmet-language-server",
      "css-lsp",
      "cssmodules-language-server",
      "typescript-language-server",
      "prisma-language-server",
      "dockerfile-language-server",
      "docker-compose-language-service",
      "yaml-language-server",
    },
  },
}
