vim.filetype.add({
  extension = { rasi = "rasi", rofi = "rasi", wofi = "rasi", ["mdx"] = "markdown.mdx", tmTheme = "xml" },
  filename = {
    ["vifmrc"] = "vim",
    ["exports"] = "zsh",
    ["tsconfig.json"] = "jsonc",
    [".yamlfmt"] = "yaml",
    ["librewolf.overrides.cfg"] = "javascript",
  },
  pattern = {
    [".*/waybar/config"] = "jsonc",
    [".*/mako/config"] = "dosini",
    [".*/kitty/.+%.conf"] = "kitty",
    [".*/hypr/.+%.conf"] = "hyprlang",
    [".*/git/config.*"] = "gitconfig",
    [".*/ssh/config"] = "sshconfig",
    [".*/.+%.tmux"] = "tmux",
    ["%.env%.[%w_.-]+"] = "sh",
  },
})
