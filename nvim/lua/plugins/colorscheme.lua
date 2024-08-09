local themes = {
  gruvbox = {
    name = "gruvbox-material",
    repo = "sainnhe/gruvbox-material",
    branch = "master",
    config = function()
      vim.g.gruvbox_material_background = "hard"
      vim.g.gruvbox_material_better_performance = 1
      vim.g.gruvbox_material_foreground = "original"
    end,
  },
  tokyonight = {
    name = "tokyonight",
    repo = "folke/tokyonight.nvim",
    commit = "e52c41314e83232840d6970e6b072f9fba242eb9",
    config = function()
      vim.opt.background = "dark"
      -- require("tokyonight").setup({
      --   style = "night",
      --   light_style = "day",
      -- })

      -- vim.cmd([[
      --   colorscheme tokyonight-night
      -- ]])
    end,
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
    end,
  },
  vscode = {
    name = "vscode",
    repo = "Mofiqul/vscode.nvim",
    branch = "main",
    config = function()
      -- For dark theme (neovim's default)
      vim.o.background = "dark"
      -- For light theme
      -- vim.o.background = 'light'

      local c = require("vscode.colors").get_colors()
      require("vscode").setup()
      require("vscode").load()
    end,
  },
  papercolor = {
    name = "papercolor",
    repo = "NLKNguyen/papercolor-theme",
    branch = "master",
    config = function() end,
  },
  onenord = {
    name = "onenord",
    repo = "rmehri01/onenord.nvim",
    branch = "main",
    config = function()
      local colors = require("onenord.colors").load()

      require("onenord").setup({
        theme = "dark",
        custom_highlights = {
          ["@parameter"] = { fg = colors.cyan },
        },
      })
    end,
  },
  nordic = {
    name = "nordic",
    repo = "AlexvZyl/nordic.nvim",
    branch = "main",
    config = function()
      require("nordic").load()
    end,
  },
  dracula = {
    name = "dracula",
    repo = "Mofiqul/dracula.nvim",
    branch = "main",
    config = function()
      require("dracula").setup()
    end,
  },
  nightfly = {
    name = "nightfly",
    repo = "bluz71/vim-nightfly-guicolors",
    priority = 1000, -- make sure to load this before all the other start plugins
    config = function() end,
  },
  monokai_nightasty = {
    name = "monokai-nightasty",
    repo = "polirritmico/monokai-nightasty.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      vim.opt.background = "light"
    end,
  },
  doomone = {
    name = "doom-one",
    repo = "NTBBloodbath/doom-one.nvim",
    config = function()
      -- Add color to cursor
      vim.g.doom_one_cursor_coloring = false
      -- Set :terminal colors
      vim.g.doom_one_terminal_colors = true
      -- Enable italic comments
      vim.g.doom_one_italic_comments = false
      -- Enable TS support
      vim.g.doom_one_enable_treesitter = true
      -- Color whole diagnostic text or only underline
      vim.g.doom_one_diagnostics_text_color = false
      -- Enable transparent background
      vim.g.doom_one_transparent_background = false

      -- Pumblend transparency
      vim.g.doom_one_pumblend_enable = false
      vim.g.doom_one_pumblend_transparency = 20

      -- Plugins integration
      vim.g.doom_one_plugin_neorg = true
      vim.g.doom_one_plugin_barbar = false
      vim.g.doom_one_plugin_telescope = false
      vim.g.doom_one_plugin_neogit = true
      vim.g.doom_one_plugin_nvim_tree = true
      vim.g.doom_one_plugin_dashboard = true
      vim.g.doom_one_plugin_startify = true
      vim.g.doom_one_plugin_whichkey = true
      vim.g.doom_one_plugin_indent_blankline = true
      vim.g.doom_one_plugin_vim_illuminate = true
      vim.g.doom_one_plugin_lspsaga = false
    end,
  },
}

local selectedTheme = themes.tokyonight

if vim.fn.has("termguicolors") == 1 then
  vim.o.termguicolors = true
end

-- vim.o.background = "dark"

local M = {
  selectedTheme.repo,
  commit = selectedTheme.commit,
  event = "VimEnter",
  lazy = false, -- make sure we load this during startup if it is your main colorscheme
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
