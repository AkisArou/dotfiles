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
}

local selectedTheme = themes.vscode

local M = {
  selectedTheme.repo,
  name = selectedTheme.name,
  event = "VimEnter",
  opts = selectedTheme.opts,
  lazy = false,
  priority = 1000,
}

function M.config()
  if selectedTheme.config ~= nil then
    selectedTheme.config()
  end

  local status_ok, _ = pcall(vim.cmd.colorscheme, M.name)
  if not status_ok then
    return
  end
end

return M
