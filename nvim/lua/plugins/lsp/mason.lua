local mason = require("mason")
local mason_lspconfig = require("mason-lspconfig")
require("mason-update-all").setup({})

mason.setup({
  ui = {
    icons = {
      package_installed = "✓",
      package_pending = "➜",
      package_uninstalled = "✗",
    },
  },
})

mason_lspconfig.setup({
  ensure_installed = {
    "vtsls",
    "lua_ls",
    "jsonls",
    "html",
    "emmet_language_server",
    "cssls",
    "tailwindcss",
    "cssmodules_ls",
    "dockerls",
    "docker_compose_language_service",
    "yamlls",
    "clangd",
    "mdx_analyzer",
    "taplo",
    "rust_analyzer",
  },
  automatic_installation = true,
})

local extra = {
  "eslint_d",
  "shfmt",
  "stylua",
  "shellcheck",
  -- "clang-format",
  "prettierd",
}

local mr = require("mason-registry")

local function ensure_installed()
  for _, tool in ipairs(extra) do
    local p = mr.get_package(tool)
    if not p:is_installed() then
      p:install()
    end
  end
end

if mr.refresh then
  mr.refresh(ensure_installed)
else
  ensure_installed()
end
