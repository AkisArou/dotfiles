return {
  "neovim/nvim-lspconfig",
  event = { "BufReadPre", "BufNewFile" },
  dependencies = {
    "hrsh7th/cmp-nvim-lsp",
    "b0o/SchemaStore.nvim",
    { "antosha417/nvim-lsp-file-operations", config = true },
  },
  config = function()
    vim.diagnostic.config({ update_in_insert = true })

    -- import lspconfig plugin
    local lspconfig = require("lspconfig")

    -- import cmp-nvim-lsp plugin
    local cmp_nvim_lsp = require("cmp_nvim_lsp")

    local keymap = vim.keymap -- for conciseness

    local opts = { noremap = true, silent = true }
    local on_attach = function(_, bufnr)
      opts.buffer = bufnr

      -- set keybinds
      opts.desc = "Show LSP references"
      keymap.set("n", "gr", "<cmd>Telescope lsp_references<CR>", opts) -- show definition, references

      opts.desc = "Go to declaration"
      keymap.set("n", "gD", vim.lsp.buf.declaration, opts) -- go to declaration

      opts.desc = "Show LSP definitions"
      keymap.set("n", "gd", "<cmd>Telescope lsp_definitions<CR>", opts) -- show lsp definitions

      opts.desc = "Show LSP implementations"
      keymap.set("n", "gi", "<cmd>Telescope lsp_implementations<CR>", opts) -- show lsp implementations

      opts.desc = "Show LSP type definitions"
      keymap.set("n", "gt", "<cmd>Telescope lsp_type_definitions<CR>", opts) -- show lsp type definitions

      opts.desc = "See available code actions"
      keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, opts) -- see available code actions, in visual mode will apply to selection

      opts.desc = "Smart rename"
      keymap.set("n", "<leader>cr", vim.lsp.buf.rename, opts) -- smart rename

      opts.desc = "Show buffer diagnostics"
      keymap.set("n", "<leader>cb", "<cmd>Telescope diagnostics bufnr=0<CR>", opts) -- show  diagnostics for file

      opts.desc = "Show line diagnostics"
      keymap.set("n", "<leader>cd", vim.diagnostic.open_float, opts) -- show diagnostics for line

      opts.desc = "Go to previous diagnostic"
      keymap.set("n", "[d", vim.diagnostic.goto_prev, opts) -- jump to previous diagnostic in buffer

      opts.desc = "Go to next diagnostic"
      keymap.set("n", "]d", vim.diagnostic.goto_next, opts) -- jump to next diagnostic in buffer

      opts.desc = "Show documentation for what is under cursor"
      keymap.set("n", "K", vim.lsp.buf.hover, opts) -- show documentation for what is under cursor

      opts.desc = "Restart LSP"
      keymap.set("n", "<leader>cl", ":LspRestart<CR>", opts) -- mapping to restart lsp if necessary
    end

    -- used to enable autocompletion (assign to every lsp server config)
    local capabilities = cmp_nvim_lsp.default_capabilities()

    -- Change the Diagnostic symbols in the sign column (gutter)
    -- (not in youtube nvim video)
    local signs = { Error = " ", Warn = " ", Hint = "󰠠 ", Info = " " }
    for type, icon in pairs(signs) do
      local hl = "DiagnosticSign" .. type
      vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
    end

    -- configure html server
    lspconfig["html"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })

    -- configure typescript server
    -- require("lspconfig.configs").vtsls = require("vtsls").lspconfig -- set default server config, optional but recommended

    lspconfig["vtsls"].setup({
      root_dir = function(...)
        return require("lspconfig.util").root_pattern(".git")(...)
      end,
    })

    -- configure typescript server with plugin
    -- lspconfig["tsserver"].setup({
    --   capabilities = capabilities,
    --   on_attach = on_attach,
    --   root_dir = function(...)
    --     return require("lspconfig.util").root_pattern(".git")(...)
    --   end,
    --   single_file_support = false,
    --   settings = {
    --     typescript = {
    --       tsserver = {
    --         maxTsServerMemory = 10000,
    --       },
    --       updateImportsOnFileMove = "always",
    --       enablePromptUseWorkspaceTsdk = true,
    --       preferences = {
    --         includePackageJsonAutoImports = "on",
    --       },
    --       inlayHints = {
    --         includeInlayParameterNameHints = "literal",
    --         includeInlayParameterNameHintsWhenArgumentMatchesName = false,
    --         includeInlayFunctionParameterTypeHints = true,
    --         includeInlayVariableTypeHints = false,
    --         includeInlayPropertyDeclarationTypeHints = true,
    --         includeInlayFunctionLikeReturnTypeHints = true,
    --         includeInlayEnumMemberValueHints = true,
    --       },
    --     },
    --     javascript = {
    --       inlayHints = {
    --         includeInlayParameterNameHints = "all",
    --         includeInlayParameterNameHintsWhenArgumentMatchesName = false,
    --         includeInlayFunctionParameterTypeHints = true,
    --         includeInlayVariableTypeHints = true,
    --         includeInlayPropertyDeclarationTypeHints = true,
    --         includeInlayFunctionLikeReturnTypeHints = true,
    --         includeInlayEnumMemberValueHints = true,
    --       },
    --     },
    --   },
    -- })

    -- configure css server
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

    -- configure tailwindcss server
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
              ["packages/shared/react/shad/tailwind.config.ts"] = "packages/**",
            },
            classRegex = {
              "tw`([^`]*)",
              { "tw.style\\(([^)]*)\\)", "'([^']*)'" },
            },
          },
        },
      },
    })

    -- configure prisma orm server
    lspconfig["prismals"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })

    -- configure emmet language server
    lspconfig["emmet_language_server"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      filetypes = { "html", "typescriptreact", "javascriptreact", "css", "sass", "scss", "svelte" },
    })

    -- configure lua server (with special settings)
    lspconfig["lua_ls"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      settings = { -- custom settings for lua
        Lua = {
          -- make the language server recognize "vim" global
          diagnostics = {
            globals = { "vim" },
          },
          workspace = {
            -- make language server aware of runtime files
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

    if not configs.npm_workspaces_lsp then
      configs.npm_workspaces_lsp = {
        default_config = {
          cmd = { "npx", "npm-workspaces-lsp", "--stdio" },
          filetypes = { "json" },
          root_dir = function(fname)
            return lspconfig.util.find_git_ancestor(fname)
          end,
          autostart = true,
          settings = {},
        },
      }
    end

    lspconfig["npm_workspaces_lsp"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
    })
    -- lspconfig["css_variables_language_server"].setup({})
  end,
}
