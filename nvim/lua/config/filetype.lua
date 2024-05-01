vim.filetype.add({
  extension = {
    conf = "conf",
    env = "dotenv",
    tiltfile = "tiltfile",
    Tiltfile = "tiltfile",
  },
  filename = {
    [".env"] = "dotenv",
    ["tsconfig.json"] = "jsonc",
    [".yamlfmt"] = "yaml",
  },
  pattern = {
    ["%.env%.[%w_.-]+"] = "dotenv",
  },
})

vim.filetype.add({
  extension = {
    mdx = "mdx",
  },
})

vim.filetype.add({
  extension = {
    ["mdx"] = "markdown.mdx",
  },
})
