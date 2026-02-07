require("nvim-treesitter").install({
  "bash",
  "c",
  "css",
  "diff",
  "dockerfile",
  "editorconfig",
  "git_config",
  "gitignore",
  "html",
  "javascript",
  "jsdoc",
  "json",
  "json5",
  "lua",
  "luadoc",
  "luap",
  "markdown",
  "markdown_inline",
  "python",
  "query",
  "regex",
  "scss",
  "ssh_config",
  "tmux",
  "toml",
  "tsx",
  "typescript",
  "vim",
  "vimdoc",
  "yaml",
})

require("treesitter-context").setup({ mode = "cursor", max_lines = 3 })

require("nvim-ts-autotag").setup()

require("ts-comments").setup()

vim.keymap.set({ "x", "o" }, "af", function()
  require("nvim-treesitter-textobjects.select").select_textobject("@function.outer", "textobjects")
end)
vim.keymap.set({ "x", "o" }, "if", function()
  require("nvim-treesitter-textobjects.select").select_textobject("@function.inner", "textobjects")
end)
