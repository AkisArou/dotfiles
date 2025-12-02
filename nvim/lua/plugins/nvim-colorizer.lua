require("colorizer").setup({
  filetypes = {
    "*", -- Highlight all files, but customize some others.
    "!notmuch-search", -- Exclude vim from highlighting.
  },
  user_default_options = {
    tailwind = true,
    mode = "background",
  },
})
