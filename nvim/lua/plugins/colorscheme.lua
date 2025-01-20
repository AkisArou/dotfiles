local themes = {
  tokyonight = {
    name = "tokyonight",
    repo = "folke/tokyonight.nvim",
    config = function()
      vim.opt.background = "dark"
    end,
  },
  vscode = {
    name = "vscode",
    repo = "Mofiqul/vscode.nvim",
    branch = "main",
    config = function()
      vim.o.background = "dark"
      require("vscode").setup()
    end,
  },
  darcula = {
    name = "darcula-solid",
    repo = "briones-gabriel/darcula-solid.nvim",
    dependencies = { "rktjmp/lush.nvim" },
  },
  gruvbox = {
    name = "gruvbox-material",
    repo = "sainnhe/gruvbox-material",
  },
  onedark = {
    name = "onedark",
    repo = "navarasu/onedark.nvim",
  },
}

local selectedTheme = themes[os.getenv("THEME") or "vscode"] or themes.vscode

local M = {
  selectedTheme.repo,
  dependencies = selectedTheme.dependencies,
  branch = selectedTheme.branch,
  event = "VimEnter",
  opts = selectedTheme.opts,
  lazy = false,
  priority = 1000,
}

function M.config()
  if selectedTheme.config ~= nil then
    selectedTheme.config()
  end

  local status_ok, _ = pcall(vim.cmd.colorscheme, selectedTheme.name)
  if not status_ok then
    return
  end
end

return M
