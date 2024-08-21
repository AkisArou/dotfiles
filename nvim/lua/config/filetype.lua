vim.filetype.add({
  extension = {
    conf = "conf",
    env = "dotenv",
    tiltfile = "tiltfile",
    Tiltfile = "tiltfile",
    ["mdx"] = "markdown.mdx",
  },
  filename = {
    [".env"] = "dotenv",
    ["tsconfig.json"] = "jsonc",
    [".yamlfmt"] = "yaml",
    ["librewolf.overrides.cfg"] = "javascript",
  },
  pattern = {
    ["%.env%.[%w_.-]+"] = "dotenv",
  },
})
