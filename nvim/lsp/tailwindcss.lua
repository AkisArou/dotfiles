--- @type vim.lsp.Config
return {
  cmd = { "tailwindcss-language-server", "--stdio" },
  -- filetypes copied and adjusted from tailwindcss-intellisense
  filetypes = {
    -- html
    "astro",
    "astro-markdown",
    "html",
    "htmlangular",
    "markdown",
    "mdx",
    -- css
    "css",
    "postcss",
    "sass",
    "scss",
    "stylus",
    -- js
    "javascript",
    "javascriptreact",
    "typescript",
    "typescriptreact",
    -- mixed
    "vue",
    "svelte",
  },
  root_markers = {
    ".git",
    "tailwind.config.ts",
    "tailwind.config.js",
    "tailwind.config.cjs",
    "tailwind.config.mjs",
  },
  settings = {
    tailwindCSS = {
      validate = true,
      lint = {
        cssConflict = "warning",
        invalidApply = "error",
        invalidScreen = "error",
        invalidVariant = "error",
        invalidConfigPath = "error",
        invalidTailwindDirective = "error",
        recommendedVariantOrder = "warning",
      },

      includeLanguages = {
        htmlangular = "html",
      },

      classAttributes = { "class", "className", "style", "class:list", "classList", "ngClass" },
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
}
