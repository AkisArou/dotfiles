local themes = {
  tokyonight = {
    name = "tokyonight",
    repo = "folke/tokyonight.nvim",
    config = function()
      require("tokyonight").setup({
        style = "night",
      })

      local colors = require("tokyonight.colors.storm")

      vim.defer_fn(function()
        vim.cmd(string.format("hi @tag.tsx guifg=%s", colors.magenta))
        vim.cmd(string.format("hi @variable.parameter guifg=%s", colors.fg_dark))
        vim.cmd(string.format("hi @variable.parameter guifg=%s", colors.fg_dark))
        vim.cmd(string.format("hi @variable.builtin.javascript guifg=%s", colors.teal))
        vim.cmd(string.format("hi @variable.builtin.typescript guifg=%s", colors.teal))
        vim.cmd(string.format("hi @variable.builtin.tsx guifg=%s", colors.teal))

        vim.cmd([[
          hi DiagnosticSignError guifg=#6e2a33 ctermfg=red
          hi DiagnosticSignWarn guifg=#7a3f22 ctermfg=yellow
          hi DiagnosticSignInfo guifg=#2a4373 ctermfg=blue
          hi DiagnosticSignHint guifg=#364a23 ctermfg=green
          hi DiagnosticUnderlineError guisp=#6e2a33 gui=undercurl
          hi DiagnosticVirtualTextError guifg=#3b4050
          hi CursorLineNr guifg=#737aa2
          hi DiagnosticUnnecessary guifg=#6F7E99
        ]])
      end, 0)
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

        vim.cmd([[
          hi DiagnosticSignError guifg=#5C0000 ctermfg=red
          hi DiagnosticSignWarn guifg=#8B4500 ctermfg=yellow
          hi DiagnosticSignInfo guifg=#0A1D42 ctermfg=blue
          hi DiagnosticSignHint guifg=#004d00 ctermfg=green
          hi DiagnosticUnderlineError guisp=#70222a gui=undercurl
          hi DiagnosticVirtualTextError guifg=#808080
          hi! MiniCursorword guibg=#343b41 gui=NONE
        ]])
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

  local status_ok, _ = pcall(vim.cmd.colorscheme, selectedTheme.name)
  if not status_ok then
    return
  end
end

return M
