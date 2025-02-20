return {
  "esmuellert/nvim-eslint",
  config = function()
    require("nvim-eslint").setup({
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
        experimental = { useFlatConfig = true },
      },
    })
  end,
}
