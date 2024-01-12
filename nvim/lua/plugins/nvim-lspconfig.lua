local util = require("util.util")

-- vim.lsp.set_log_level("debug")

return {
  {
    "neovim/nvim-lspconfig",
    enabled = false,
    enable = false,
    tsserver = {
      enabled = false,
      autostart = false,
      enable = false,
    },
  },
  {
    "folke/neoconf.nvim",
    enabled = false,
    enable = false,
  },
  {
    "nvim-lspconfig",
    enabled = false,
    enable = false,
  },
  { "folke/neodev.nvim",                 enabled = false, enable = false },
  -- { "mason.nvim",                        enabled = false, enable = false },
  { "williamboman/mason-lspconfig.nvim", enabled = false, enable = false },
  -- { "williamboman/mason.nvim",           enabled = false, enable = false },

  { "hrsh7th/cmp-nvim-lsp",              enabled = false, enable = false },
  { "hrsh7th/cmp-buffer",                enabled = false, enable = false },
  { "hrsh7th/cmp-path",                  enabled = false, enable = false },
  { "saadparwaiz1/cmp_luasnip",          enabled = false, enable = false },
  {
    "L3MON4D3/LuaSnip",
    enabled = false,
    enable = false
  },
  {
    "nvim-cmp",
    enabled = false,
    enable = false
  },
  {
    "nvim-lint",
    enabled = false,
    enable = false
  },
  {
    "conform.nvim",
    enabled = false,
    enable = false
  }
}

-- return {
--   "neovim/nvim-lspconfig",
--   opts = {
--     inlay_hints = { enabled = true },
--     ---@type lspconfig.options
--     servers = {
--       cssls = {
--         settings = {
--           css = {
--             validate = true,
--             lint = {
--               unknownAtRules = "ignore",
--             },
--           },
--           scss = {
--             validate = true,
--             lint = {
--               unknownAtRules = "ignore",
--             },
--           },
--           less = {
--             validate = true,
--             lint = {
--               unknownAtRules = "ignore",
--             },
--           },
--         },
--       },
--       tailwindcss = {
--         root_dir = function(...)
--           return require("lspconfig.util").root_pattern(".git")(...)
--         end,
--         settings = {
--           tailwindCSS = {
--             validate = true,
--             classAttributes = { "class", "className", "style" },
--             experimental = {
--               configFile = {
--                 ["packages/shared/react/react-ui-data-centric/tailwind.config.ts"] = "packages/**",
--               },
--               classRegex = {
--                 "tw`([^`]*)",
--                 { "tw.style\\(([^)]*)\\)", "'([^']*)'" },
--               },
--             },
--           },
--         },
--       },
--       tsserver = {
--         enabled = false,
--         autostart = false,
--         enable = false,
--       },
--
--       -- tsserver = {
--       --   root_dir = function(...)
--       --     return require("lspconfig.util").root_pattern(".git")(...)
--       --   end,
--       --   single_file_support = false,
--       --   settings = {
--       --     typescript = {
--       --       tsserver = {
--       --         maxTsServerMemory = 10000,
--       --         nodePath = "/home/akisarou/.asdf/installs/nodejs/20.10.0/bin/node",
--       --       },
--       --       updateImportsOnFileMove = "always",
--       --       enablePromptUseWorkspaceTsdk = true,
--       --       preferences = {
--       --         includePackageJsonAutoImports = "on",
--       --       },
--       --       inlayHints = {
--       --         includeInlayParameterNameHints = "literal",
--       --         includeInlayParameterNameHintsWhenArgumentMatchesName = false,
--       --         includeInlayFunctionParameterTypeHints = true,
--       --         includeInlayVariableTypeHints = false,
--       --         includeInlayPropertyDeclarationTypeHints = true,
--       --         includeInlayFunctionLikeReturnTypeHints = true,
--       --         includeInlayEnumMemberValueHints = true,
--       --       },
--       --     },
--       --     javascript = {
--       --       inlayHints = {
--       --         includeInlayParameterNameHints = "all",
--       --         includeInlayParameterNameHintsWhenArgumentMatchesName = false,
--       --         includeInlayFunctionParameterTypeHints = true,
--       --         includeInlayVariableTypeHints = true,
--       --         includeInlayPropertyDeclarationTypeHints = true,
--       --         includeInlayFunctionLikeReturnTypeHints = true,
--       --         includeInlayEnumMemberValueHints = true,
--       --       },
--       --     },
--       --   },
--       -- },
--       html = {},
--       yamlls = {
--         settings = {
--           yaml = {
--             keyOrdering = false,
--           },
--         },
--       },
--       lua_ls = {
--         -- enabled = false,
--         single_file_support = true,
--         settings = {
--           Lua = {
--             workspace = {
--               checkThirdParty = false,
--             },
--             completion = {
--               workspaceWord = true,
--               callSnippet = "Both",
--             },
--             misc = {
--               parameters = {
--                 -- "--log-level=trace",
--               },
--             },
--             hint = {
--               enable = true,
--               setType = false,
--               paramType = true,
--               paramName = "Disable",
--               semicolon = "Disable",
--               arrayIndex = "Disable",
--             },
--             doc = {
--               privateName = { "^_" },
--             },
--             type = {
--               castNumberToInteger = true,
--             },
--             diagnostics = {
--               disable = { "incomplete-signature-doc", "trailing-space" },
--               -- enable = false,
--               groupSeverity = {
--                 strong = "Warning",
--                 strict = "Warning",
--               },
--               groupFileStatus = {
--                 ["ambiguity"] = "Opened",
--                 ["await"] = "Opened",
--                 ["codestyle"] = "None",
--                 ["duplicate"] = "Opened",
--                 ["global"] = "Opened",
--                 ["luadoc"] = "Opened",
--                 ["redefined"] = "Opened",
--                 ["strict"] = "Opened",
--                 ["strong"] = "Opened",
--                 ["type-check"] = "Opened",
--                 ["unbalanced"] = "Opened",
--                 ["unused"] = "Opened",
--               },
--               unusedLocalExclude = { "_*" },
--             },
--             format = {
--               enable = false,
--               defaultConfig = {
--                 indent_style = "space",
--                 indent_size = "2",
--                 continuation_indent_size = "2",
--               },
--             },
--           },
--         },
--       },
--     },
--     npm_workspaces_lsp = {},
--     css_variables_language_server = {},
--   },
--   init = function()
--     local lspconfig = require("lspconfig")
--     local configs = require("lspconfig.configs")
--
--     if not configs.npm_workspaces_lsp then
--       configs.npm_workspaces_lsp = {
--         default_config = {
--           cmd = { "npx", "npm-workspaces-lsp", "--stdio" },
--           filetypes = { "json" },
--           root_dir = function(fname)
--             return lspconfig.util.find_git_ancestor(fname)
--           end,
--           autostart = true,
--           settings = {},
--         },
--       }
--     end
--
--     if not configs.css_variables_language_server then
--       local file_path = util.get_css_variables_language_server_path()
--
--       configs.css_variables_language_server = {
--         default_config = {
--           cmd = {
--             "node",
--             file_path,
--             "--nolazy",
--             "--stdio",
--           },
--           filetypes = {
--             "css",
--             "less",
--             "sass",
--             "scss",
--             "stylus",
--           },
--           root_dir = function(fname)
--             return lspconfig.util.find_git_ancestor(fname)
--           end,
--           autostart = true,
--           settings = {
--             lookupFiles = { "**/*.less", "**/*.scss", "**/*.sass", "**/*.css" },
--             blacklistFolders = {
--               "**/.cache",
--               "**/.DS_Store",
--               "**/.git",
--               "**/.hg",
--               "**/.next",
--               "**/.svn",
--               "**/bower_components",
--               "**/CVS",
--               "**/dist",
--               "**/node_modules",
--               "**/tests",
--               "**/tmp",
--             },
--           },
--         },
--       }
--     end
--
--     lspconfig.npm_workspaces_lsp.setup({})
--     lspconfig.css_variables_language_server.setup({
--       capabilities = require("cmp_nvim_lsp").default_capabilities(),
--     })
--   end,
-- }
