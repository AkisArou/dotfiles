return {
  "neovim/nvim-lspconfig",
  opts = {
    inlay_hints = { enabled = true },
    ---@type lspconfig.options
    servers = {
      cssls = {
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
          less = {
            validate = true,
            lint = {
              unknownAtRules = "ignore",
            },
          },
        },
      },
      tailwindcss = {
        root_dir = function(...)
          return require("lspconfig.util").root_pattern(".git")(...)
        end,
        settings = {
          tailwindCSS = {
            validate = true,
            classAttributes = { "class", "className", "style" },
            experimental = {
              configFile = {
                ["packages/shared/react/react-ui-data-centric/tailwind.config.ts"] = "packages/**",
              },
              classRegex = {
                "tw`([^`]*)",
                { "tw.style\\(([^)]*)\\)", "'([^']*)'" },
              },
            },
          },
        },
      },
      tsserver = {
        root_dir = function(...)
          return require("lspconfig.util").root_pattern(".git")(...)
        end,
        single_file_support = false,
        settings = {
          typescript = {
            updateImportsOnFileMove = "always",
            enablePromptUseWorkspaceTsdk = true,
            preferences = {
              includePackageJsonAutoImports = "on",
            },
            inlayHints = {
              includeInlayParameterNameHints = "literal",
              includeInlayParameterNameHintsWhenArgumentMatchesName = false,
              includeInlayFunctionParameterTypeHints = true,
              includeInlayVariableTypeHints = false,
              includeInlayPropertyDeclarationTypeHints = true,
              includeInlayFunctionLikeReturnTypeHints = true,
              includeInlayEnumMemberValueHints = true,
            },
          },
          javascript = {
            inlayHints = {
              includeInlayParameterNameHints = "all",
              includeInlayParameterNameHintsWhenArgumentMatchesName = false,
              includeInlayFunctionParameterTypeHints = true,
              includeInlayVariableTypeHints = true,
              includeInlayPropertyDeclarationTypeHints = true,
              includeInlayFunctionLikeReturnTypeHints = true,
              includeInlayEnumMemberValueHints = true,
            },
          },
        },
      },
      html = {},
      yamlls = {
        settings = {
          yaml = {
            keyOrdering = false,
          },
        },
      },
      lua_ls = {
        -- enabled = false,
        single_file_support = true,
        settings = {
          Lua = {
            workspace = {
              checkThirdParty = false,
            },
            completion = {
              workspaceWord = true,
              callSnippet = "Both",
            },
            misc = {
              parameters = {
                -- "--log-level=trace",
              },
            },
            hint = {
              enable = true,
              setType = false,
              paramType = true,
              paramName = "Disable",
              semicolon = "Disable",
              arrayIndex = "Disable",
            },
            doc = {
              privateName = { "^_" },
            },
            type = {
              castNumberToInteger = true,
            },
            diagnostics = {
              disable = { "incomplete-signature-doc", "trailing-space" },
              -- enable = false,
              groupSeverity = {
                strong = "Warning",
                strict = "Warning",
              },
              groupFileStatus = {
                ["ambiguity"] = "Opened",
                ["await"] = "Opened",
                ["codestyle"] = "None",
                ["duplicate"] = "Opened",
                ["global"] = "Opened",
                ["luadoc"] = "Opened",
                ["redefined"] = "Opened",
                ["strict"] = "Opened",
                ["strong"] = "Opened",
                ["type-check"] = "Opened",
                ["unbalanced"] = "Opened",
                ["unused"] = "Opened",
              },
              unusedLocalExclude = { "_*" },
            },
            format = {
              enable = false,
              defaultConfig = {
                indent_style = "space",
                indent_size = "2",
                continuation_indent_size = "2",
              },
            },
          },
        },
      },
    },
    npm_workspaces_lsp = {},
    css_variables_language_server = {},
  },
  config = function()
    local lspconfig = require("lspconfig")
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

    if not configs.css_variables_language_server then
      local file_path = os.getenv("HOME")
        .. "/.asdf/installs/nodejs/20.10.0/lib/node_modules/css-variables-language-server/dist/index.js"

      configs.css_variables_language_server = {
        default_config = {
          cmd = {
            "node",
            file_path,
            "--nolazy",
            "--stdio",
          },
          filetypes = {
            "css",
            "less",
            "sass",
            "scss",
            "stylus",
          },
          root_dir = function(fname)
            return lspconfig.util.find_git_ancestor(fname)
          end,
          autostart = true,
          settings = {
            lookupFiles = { "**/*.less", "**/*.scss", "**/*.sass", "**/*.css" },
            blacklistFolders = {
              "**/.cache",
              "**/.DS_Store",
              "**/.git",
              "**/.hg",
              "**/.next",
              "**/.svn",
              "**/bower_components",
              "**/CVS",
              "**/dist",
              "**/node_modules",
              "**/tests",
              "**/tmp",
            },
          },
        },
      }
    end

    lspconfig.npm_workspaces_lsp.setup({})
    lspconfig.css_variables_language_server.setup({})
  end,
}
