return {
  "esmuellert/nvim-eslint",
  config = function()
    local nvim_eslint = require("nvim-eslint")

    nvim_eslint.setup({
      filetypes = {
        "javascript",
        "javascriptreact",
        "javascript.jsx",
        "typescript",
        "typescriptreact",
        "typescript.tsx",
        "vue",
        "svelte",
        "astro",
        "json",
      },
      settings = {
        useFlatConfig = true,
        workspaceFolder = function(bufnr)
          local root_dir = nvim_eslint.resolve_git_dir(bufnr) or vim.fn.getcwd()
          return {
            uri = vim.uri_from_fname(root_dir),
            name = vim.fn.fnamemodify(root_dir, ":t"),
          }
        end,
      },
    })
  end,
}
