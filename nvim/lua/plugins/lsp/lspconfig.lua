return {
  "neovim/nvim-lspconfig",
  event = { "BufReadPre", "BufNewFile" },
  lazy = true,
  dependencies = {
    "folke/neodev.nvim",
    "hrsh7th/cmp-nvim-lsp",
    "b0o/SchemaStore.nvim",
    { "antosha417/nvim-lsp-file-operations", config = true },
    "yioneko/nvim-vtsls",
  },
  config = function()
    vim.diagnostic.config({ update_in_insert = true })

    -- IMPORTANT: make sure to setup neodev BEFORE lspconfig
    require("neodev").setup({})

    local lspconfig = require("lspconfig")

    local cmp_nvim_lsp = require("cmp_nvim_lsp")

    local keymap = vim.keymap

    local opts = { noremap = true, silent = true }
    local on_attach = function(_, bufnr)
      opts.buffer = bufnr

      opts.desc = "Show LSP references"
      keymap.set("n", "gr", "<cmd>Telescope lsp_references<CR>", opts)

      opts.desc = "Go to declaration"
      keymap.set("n", "gD", vim.lsp.buf.declaration, opts)

      opts.desc = "Show LSP definitions"
      keymap.set("n", "gd", "<cmd>Telescope lsp_definitions<CR>", opts)

      opts.desc = "Show LSP implementations"
      keymap.set("n", "gi", "<cmd>Telescope lsp_implementations<CR>", opts)

      opts.desc = "Show LSP type definitions"
      keymap.set("n", "gt", "<cmd>Telescope lsp_type_definitions<CR>", opts)

      opts.desc = "See available code actions"
      keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, opts)

      opts.desc = "Smart rename"
      keymap.set("n", "<leader>cr", vim.lsp.buf.rename, opts)

      opts.desc = "Show buffer diagnostics"
      keymap.set("n", "<leader>cb", "<cmd>Telescope diagnostics bufnr=0<CR>", opts)

      opts.desc = "Show line diagnostics"
      keymap.set("n", "<leader>cd", vim.diagnostic.open_float, opts)

      opts.desc = "Go to previous diagnostic"
      keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)

      opts.desc = "Go to next diagnostic"
      keymap.set("n", "]d", vim.diagnostic.goto_next, opts)

      opts.desc = "Show documentation for what is under cursor"
      keymap.set("n", "K", vim.lsp.buf.hover, opts)

      opts.desc = "Restart LSP"
      keymap.set("n", "<leader>cl", ":LspRestart<CR>", opts)

      opts.desc = "Remove unused imports"
      keymap.set("n", "<leader>cqi", ":VtsExec remove_unused_imports<CR>", opts)
    end

    local capabilities = cmp_nvim_lsp.default_capabilities()

    local signs = { Error = " ", Warn = " ", Hint = "󰠠 ", Info = " " }
    for type, icon in pairs(signs) do
      local hl = "DiagnosticSign" .. type
      vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
    end

    lspconfig["html"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })

    require("lspconfig.configs").vtsls = require("vtsls").lspconfig -- set default server config, optional but recommended

    lspconfig["vtsls"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      root_dir = function(...)
        return require("lspconfig.util").root_pattern(".git")(...)
      end,
      settings = {
        vtsls = {
          autoUseWorkspaceTsdk = true,
        },
        typescript = {
          tsserver = {
            maxTsServerMemory = 10000,
          },
          preferences = {
            includePackageJsonAutoImports = "on",
            importModuleSpecifier = "non-relative",
          },
        },
      },
    })

    lspconfig["cssls"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      settings = {
        css = {
          validate = true,
          lint = {
            unknownAtRules = "ignore",
          },
        },
        scss = {
          validate = true,
          lint = {
            unknownAtRules = "ignore",
          },
        },
      },
    })

    lspconfig["tailwindcss"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      root_dir = function(...)
        return require("lspconfig.util").root_pattern(".git")(...)
      end,
      settings = {
        tailwindCSS = {
          validate = true,
          classAttributes = { "class", "className", "style" },
          experimental = {
            configFile = {
              ["apps/client/assistant-prm-airport/back-office/tailwind.config.ts"] = "packages/assistant-prm-airport/**",
              ["apps/client/volunteer/back-office/tailwind.config.ts"] = "packages/assistant-volunteer/**",
              ["apps/website/nable/tailwind.config.ts"] = "apps/website/nable/**",
              ["packages/shared/react/mantine/tailwind.config.ts"] = "packages/shared/react/mantine/**",
            },
            classRegex = {
              "tw`([^`]*)",
              { "tw.style\\(([^)]*)\\)", "'([^']*)'" },
            },
          },
        },
      },
    })

    lspconfig["prismals"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })

    lspconfig["emmet_language_server"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      filetypes = { "html", "typescriptreact", "javascriptreact", "css", "sass", "scss", "svelte" },
    })

    lspconfig["lua_ls"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      settings = {
        Lua = {
          diagnostics = {
            globals = { "vim" },
          },
          workspace = {
            library = {
              [vim.fn.expand("$VIMRUNTIME/lua")] = true,
              [vim.fn.stdpath("config") .. "/lua"] = true,
            },
          },
        },
      },
    })

    lspconfig["biome"].setup({})

    lspconfig["jsonls"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      filetypes = { "json", "jsonc" },
      settings = {
        json = {
          schemas = require("schemastore").json.schemas(),
          format = {
            enable = true,
          },
          validate = { enable = true },
        },
      },
    })

    lspconfig["yamlls"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      settings = {
        yaml = {
          schemaStore = {
            -- You must disable built-in schemaStore support if you want to use
            -- this plugin and its advanced options like `ignore`.
            enable = false,
            -- Avoid TypeError: Cannot read properties of undefined (reading 'length')
            url = "",
          },
          schemas = require("schemastore").yaml.schemas(),
        },
      },
    })

    local configs = require("lspconfig.configs")

    if not configs.npm_workspaces_language_server then
      configs.npm_workspaces_language_server = {
        default_config = {
          cmd = { "npx", "npm-workspaces-language-server", "--stdio" },
          filetypes = { "json" },
          root_dir = function(fname)
            return lspconfig.util.find_git_ancestor(fname)
          end,
          autostart = true,
          settings = {},
        },
      }
    end

    lspconfig["npm_workspaces_language_server"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })

    lspconfig["clangd"].setup({
      on_attach = function(client, bufnr)
        client.server_capabilities.signatureHelpProvider = false
        on_attach(client, bufnr)
      end,
      capabilities = capabilities,
    })

    lspconfig["mdx_analyzer"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      root_dir = function(...)
        return require("lspconfig.util").root_pattern(".git")(...)
      end,
    })
  end,
}
