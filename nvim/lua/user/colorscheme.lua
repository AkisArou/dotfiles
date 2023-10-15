local themes = {
  gruvbox = {
    name = "gruvbox-material",
    repo = "sainnhe/gruvbox-material",
    branch = "master",
    config = function()
      vim.g.gruvbox_material_background = 'hard'
      vim.g.gruvbox_material_better_performance = 1
      vim.g.gruvbox_material_foreground = "original"
    end
  },
  tokyonight = {
    name = "tokyonight",
    repo = "folke/tokyonight.nvim",
    commit = "e52c41314e83232840d6970e6b072f9fba242eb9",
    config = function()
      vim.opt.background = "dark"
      require("tokyonight").setup({
        style = "storm",
        light_style = "day"
      })
    end
  },
  catpuccin = {
    name = "catppuccin",
    repo = "catppuccin/nvim",
    branch = "main",
    config = function()
      require("catppuccin").setup({
        flavour = "latte",
        background = { -- :h background
          light = "latte",
          dark = "mocha",
        },
      })
    end
  },
  vscode = {
    name = "vscode",
    repo = "Mofiqul/vscode.nvim",
    branch = "main",
    config = function()
      -- For dark theme (neovim's default)
      vim.o.background = 'dark'
      -- For light theme
      -- vim.o.background = 'light'

      local c = require('vscode.colors').get_colors()
      require('vscode').setup()
      require('vscode').load()
    end
  },
  papercolor = {
    name = "papercolor",
    repo = "NLKNguyen/papercolor-theme",
    branch = "master",
    config = function()
    end
  },
  onenord = {
    name = "onenord",
    repo = "rmehri01/onenord.nvim",
    branch = "main",
    config = function()
      local colors = require("onenord.colors").load()

      require('onenord').setup({
        theme = "dark",
        custom_highlights = {
          ["@parameter"] = { fg = colors.cyan }
        }
      })
    end
  },
  nordic = {
    name = "nordic",
    repo = "AlexvZyl/nordic.nvim",
    branch = "main",
    config = function()
      require("nordic").load()
    end
  },
  dracula = {
    name = "dracula",
    repo = "Mofiqul/dracula.nvim",
    branch = "main",
    config = function()
      require("dracula").setup()
    end
  },
  nightfly = {
    name = "nightfly",
    repo = "bluz71/vim-nightfly-guicolors",
    priority = 1000, -- make sure to load this before all the other start plugins
    config = function()
    end,
  },
  monokai_nightasty = {
    name = "monokai-nightasty",
    repo = "polirritmico/monokai-nightasty.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      vim.opt.background = "light"
    end,
  }
}

local selectedTheme = themes.tokyonight

if vim.fn.has('termguicolors') == 1 then
  vim.o.termguicolors = true
end

-- vim.o.background = "dark"

local M = {
  selectedTheme.repo,
  commit = selectedTheme.commit,
  event = "VimEnter",
  lazy = false,    -- make sure we load this during startup if it is your main colorscheme
  priority = 1000, -- make sure to load this before all the other start plugins
}

M.name = selectedTheme.name

function M.config()
  local status_ok, _ = pcall(vim.cmd.colorscheme, M.name)
  if not status_ok then
    return
  end

  selectedTheme.config()
end

return M
