local themes = {
  tokyonight = {
    name = "tokyonight",
    colorscheme_name = "tokyonight-night",
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
    config = function()
      local colors = require("onedark.palette").darker

      require("onedark").setup({
        style = "darker", -- Default theme style. Choose between 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer' and 'light'
        highlights = {
          ["@keyword.import"] = { fg = colors.blue },
          ["@keyword"] = { fg = colors.purple },
          ["@type"] = { fg = colors.fg },
          ["@variable.member"] = { fg = colors.fg },
          ["@variable.parameter"] = { fg = colors.fg },
          ["@lsp.type.parameter"] = { fg = colors.fg },
          ["@lsp.type.type"] = { fg = colors.fg },
          ["@lsp.type.interface"] = { fg = colors.cyan },
          ["@lsp.type.enum"] = { fg = colors.fg },
          ["@lsp.type.namespace"] = { fg = colors.fg },
          ["@lsp.type.property"] = { fg = colors.fg },
          ["@lsp.type.enumMember"] = { fg = colors.cyan },
          ["@type.builtin.typescript"] = { fg = colors.cyan },
          ["@tag.attribute"] = { fg = colors.fg },
          ["@keyword.modifier.typescript"] = { fg = colors.light_grey },
        },
      })

      vim.defer_fn(function()
        vim.cmd(string.format("hi LazySpecial guifg=%s", colors.cyan))
        vim.cmd(string.format("hi LazyReasonPlugin guifg=%s", colors.blue))
        vim.cmd(string.format("hi FzfLuaHeaderText guifg=%s", colors.fg))
      end, 0)
    end,
  },
}

local selectedTheme = themes[vim.g.os_theme] or themes.onedark

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

  local status_ok, _ = pcall(vim.cmd.colorscheme, selectedTheme.colorscheme_name or selectedTheme.name)
  if not status_ok then
    return
  end
end

return M
