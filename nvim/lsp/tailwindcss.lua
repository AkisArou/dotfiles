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
  root_dir = vim.fn.getcwd(),
  workspace_required = true,
  before_init = function(_, config)
    if not config.settings then
      config.settings = {}
    end
    if not config.settings.editor then
      config.settings.editor = {}
    end
    if not config.settings.editor.tabSize then
      ---@diagnostic disable-next-line: inject-field
      config.settings.editor.tabSize = vim.lsp.util.get_effective_tabstop()
    end
  end,
  settings = {
    tailwindCSS = {
      validate = true,
      lint = {
        cssConflict = "error",
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
      classAttributes = { "class", "className", "style", "classList" },
      classFunctions = { "cn", "clsx", "tw", "tw.color", "tw.style" },
    },
  },
}
