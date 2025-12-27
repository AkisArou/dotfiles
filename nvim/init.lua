vim.g.os_theme = os.getenv("THEME") or "tokyonight"

vim.g.is_difftool = vim.iter(vim.v.argv):any(function(a)
  return a:find("DiffTool")
end)

require("config")
require("custom.temp-worktree")
