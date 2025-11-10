vim.filetype.add({
  extension = { rasi = "rasi", rofi = "rasi", wofi = "rasi", ["mdx"] = "markdown.mdx", tmTheme = "xml" },
  filename = {
    ["vifmrc"] = "vim",
    ["exports"] = "zsh",
    ["blerc"] = "bash",
    ["tsconfig.json"] = "jsonc",
    ["launch.json"] = "jsonc",
    ["tasks.json"] = "jsonc",
    [".yamlfmt"] = "yaml",
    ["librewolf.overrides.cfg"] = "javascript",
  },
  pattern = {
    [".*/.vscode/.*.json"] = "jsonc",
    [".*/waybar/config"] = "jsonc",
    [".*/mako/config"] = "dosini",
    [".*/kitty/.+%.conf"] = "kitty",
    [".*/hypr/.+%.conf"] = "hyprlang",
    [".*/git/config.*"] = "gitconfig",
    [".*/ssh/config"] = "sshconfig",
    [".*/.+%.tmux"] = "tmux",
    ["%.env%.[%w_.-]+"] = "dosini",
  },
})
