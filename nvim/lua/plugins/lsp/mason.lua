return {
  "williamboman/mason.nvim",
  dependencies = {
    "williamboman/mason-lspconfig.nvim",
  },
  lazy = true,
  config = function()
    -- import mason
    local mason = require("mason")

    -- import mason-lspconfig
    local mason_lspconfig = require("mason-lspconfig")

    -- enable mason and configure icons
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
      -- list of servers for mason to install
      ensure_installed = {
        "vtsls",
        "lua_ls",
        "jsonls",
        "html",
        "cssls",
        "emmet_language_server",
        "tailwindcss",
        "cssmodules_ls",
        -- "tsserver",
        "prismals",
        "dockerls",
        "docker_compose_language_service",
        "yamlls",
        "biome",
        "clangd",
      },
      -- auto-install configured servers (with lspconfig)
      automatic_installation = true, -- not the same as ensure_installed
    })

    local extra = {
      "shfmt",
      "codespell",
      "rustywind",
      "hadolint",
      "stylua",
      "shellcheck",
      "clang-format",
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
  end,
}
