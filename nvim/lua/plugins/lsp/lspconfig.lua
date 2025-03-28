return {
  "neovim/nvim-lspconfig",
  event = { "BufReadPre", "BufNewFile" },
  lazy = true,
  dependencies = {
    "saghen/blink.cmp",
    "b0o/SchemaStore.nvim",
    "yioneko/nvim-vtsls",
    "ibhagwan/fzf-lua",
    { "Bilal2453/luvit-meta", lazy = true },
    {
      "folke/lazydev.nvim",
      ft = "lua",
      opts = {
        library = {
          { path = "${3rd}/luv/library", words = { "vim%.uv" } },
        },
      },
    },
  },
  config = function()
    vim.diagnostic.config({
      update_in_insert = true,
      virtual_text = false,
      signs = {
        text = {
          [vim.diagnostic.severity.ERROR] = "◉",
          [vim.diagnostic.severity.WARN] = "◎",
          [vim.diagnostic.severity.INFO] = "●",
          [vim.diagnostic.severity.HINT] = "○",
        },
      },
      jump = { float = true },
    })

    local lspconfig = require("lspconfig")

    local keymap = vim.keymap

    local opts = { noremap = true, silent = true }
    local on_attach = function(_, bufnr)
      opts.buffer = bufnr

      opts.desc = "Show LSP references"
      keymap.set("n", "grr", function()
        require("fzf-lua").lsp_references({
          ignore_current_line = true,
          winopts = {
            preview = {
              layout = "vertical",
            },
          },
        })
      end, opts)

      opts.desc = "Go to declaration"
      keymap.set("n", "gD", vim.lsp.buf.declaration, opts)

      opts.desc = "Show LSP definitions"
      keymap.set("n", "gd", function()
        require("fzf-lua").lsp_definitions({
          jump1 = true,
          winopts = {
            preview = {
              layout = "vertical",
            },
          },
        })
      end, opts)

      opts.desc = "See available code actions"

      local ommited_actions = {
        "Move to",
        "Extract to",
        "Change to parameter",
        "Add missing function declaration",
        "Add all missing function declarations",
        "Generate 'get'",
        "Add braces",
        "Remove unused",
        "Fix all detected spelling",
        "Change spelling to",
      }

      keymap.set({ "n", "v" }, "gra", function()
        require("fzf-lua").lsp_code_actions({
          filter = function(action)
            for _, value in ipairs(ommited_actions) do
              if string.find(action.title, value) then
                return false
              end
            end

            return true
          end,
        })
      end, opts)

      opts.desc = "Show buffer diagnostics"
      keymap.set("n", "<C-w><C-f>", ":FzfLua lsp_document_diagnostics<CR>", opts)

      opts.desc = "Restart LSP"
      keymap.set("n", "<leader>cl", "<cmd>LspRestart<CR>", opts)

      opts.desc = "Remove unused imports"
      keymap.set("n", "<leader>cqi", function()
        require("vtsls").commands.remove_unused_imports(bufnr, function()
          require("conform").format({ bufnr = bufnr, async = false })
          vim.cmd("silent! w")
        end)
      end, opts)
    end

    local capabilities = require("blink.cmp").get_lsp_capabilities()

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

    local autoImportSpecifierExcludeRegex =
      "^(assert|async_hooks|buffer|child_process|cluster|console|crypto|dgram|dns|domain|events|fs|fs/promises|http|http2|https|inspector|module|net|os|path|path/posix|perf_hooks|process|punycode|querystring|readline|repl|stream|string_decoder|timers|tls|trace_events|tty|url|util|v8|vm|worker_threads|zlib)$"

    lspconfig["vtsls"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      root_dir = function(...)
        return require("lspconfig.util").root_pattern(".git", "package.json", "tsconfig.json")(...)
      end,
      settings = {
        vtsls = {
          autoUseWorkspaceTsdk = true,
          experimental = {
            completion = {
              enableServerSideFuzzyMatch = true,
              entriesLimit = 5000,
            },
          },
        },
        typescript = {
          format = {
            enable = false,
          },
          tsserver = {
            maxTsServerMemory = 4 * 1024,
            nodePath = "~/dotfiles/nvim/lua/plugins/lsp/vscode-as-node-path",
          },
          preferences = {
            includePackageJsonAutoImports = "on",
            importModuleSpecifier = "non-relative",
            autoImportSpecifierExcludeRegexes = {
              autoImportSpecifierExcludeRegex,
            },
          },
        },
        javascript = {
          format = {
            enable = false,
          },
          preferences = {
            autoImportSpecifierExcludeRegexes = {
              autoImportSpecifierExcludeRegex,
            },
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
        return require("lspconfig.util").root_pattern(
          ".git",
          "tailwind.config.js",
          "tailwind.config.ts",
          "tailwind.config.cjs"
        )(...)
      end,
      settings = {
        tailwindCSS = {
          validate = true,
          classAttributes = { "class", "className", "style" },
          experimental = {
            configFile = (function()
              local cwd = vim.fn.getcwd()
              if cwd:match("nable%-solutions") then
                return {
                  ["apps/client/assistant-prm-airport/back-office/tailwind.config.ts"] = {
                    "apps/client/assistant-prm-airport/back-office/src/**",
                    "packages/assistant-prm-airport/**",
                  },
                  ["apps/client/assistant-prm-airport/agent/tailwind.config.ts"] = {
                    "apps/client/assistant-prm-airport/agent/**",
                  },
                  ["apps/client/volunteer/back-office/tailwind.config.ts"] = "packages/assistant-volunteer/**",
                  ["apps/website/nable/tailwind.config.ts"] = "apps/website/nable/**",
                  ["packages/shared/react/rsuite/tailwind.config.ts"] = "packages/shared/react/rsuite/**",
                  ["packages/shared/react/heroui/tailwind.config.ts"] = "packages/shared/react/heroui/**",
                }
              end

              return nil
            end)(),
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

    lspconfig.taplo.setup({})

    lspconfig["dockerls"].setup({})

    lspconfig["docker_compose_language_service"].setup({})

    local configs = require("lspconfig.configs")

    if not configs.npm_workspaces_language_server then
      configs.npm_workspaces_language_server = {
        default_config = {
          cmd = { "npx", "npm-workspaces-language-server", "--stdio" },
          filetypes = { "json", "packagejson" },
          root_dir = function(...)
            return require("lspconfig.util").root_pattern(".git", "package.json")(...)
          end,
          autostart = true,
          settings = {},
        },
      }
    end

    lspconfig["npm_workspaces_language_server"].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      filetypes = { "json", "packagejson" },
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
