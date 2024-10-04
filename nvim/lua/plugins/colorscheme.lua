local themes = {
  tokyonight = {
    name = "tokyonight",
    repo = "folke/tokyonight.nvim",
    commit = "e52c41314e83232840d6970e6b072f9fba242eb9",
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
      require("vscode").load()
    end,
  },
}

local selectedTheme = themes.vscode

if vim.fn.has("termguicolors") == 1 then
  vim.o.termguicolors = true
end

local M = {
  selectedTheme.repo,
  commit = selectedTheme.commit,
  event = "VimEnter",
  opts = selectedTheme.opts,
  lazy = false, -- make sure we load this during startup if it is your main colorscheme
  priority = 1000, -- make sure to load this before all the other start plugins
}

M.name = selectedTheme.name

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
