vim.filetype.add({
  extension = { rasi = "rasi", rofi = "rasi", wofi = "rasi", ["mdx"] = "markdown.mdx", tmTheme = "xml" },
  filename = {
    ["vifmrc"] = "vim",
    ["tsconfig.json"] = "jsonc",
    [".yamlfmt"] = "yaml",
    ["librewolf.overrides.cfg"] = "javascript",
  },
  pattern = {
    [".*/waybar/config"] = "jsonc",
    [".*/mako/config"] = "dosini",
    [".*/kitty/.+%.conf"] = "kitty",
    [".*/hypr/.+%.conf"] = "hyprlang",
    ["%.env%.[%w_.-]+"] = "sh",
  },
})
