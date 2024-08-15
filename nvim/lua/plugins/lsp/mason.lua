return {
  "williamboman/mason.nvim",
  dependencies = {
    "williamboman/mason-lspconfig.nvim",
    {
      "jay-babu/mason-nvim-dap.nvim",
      dependencies = "mason.nvim",
      cmd = { "DapInstall", "DapUninstall" },
      opts = {
        automatic_installation = true,
        ensure_installed = {
          "js",
          "firefox",
        },
      },
    },
  },
  lazy = true,
  config = function()
    local mason = require("mason")
    local mason_lspconfig = require("mason-lspconfig")

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
        "cssls",
        "emmet_language_server",
        "tailwindcss",
        "cssmodules_ls",
        "prismals",
        "dockerls",
        "docker_compose_language_service",
        "yamlls",
        "biome",
        -- "clangd",
        "mdx_analyzer",
      },
      automatic_installation = true,
    })

    local extra = {
      "shfmt",
      "rustywind",
      "stylua",
      "shellcheck",
      -- "clang-format",
      "prettier",
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
