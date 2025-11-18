local themes = {
  tokyonight = function()
    require("tokyonight").setup({
      style = "night",
      on_highlights = function(highlights, colors)
        highlights["@tag.tsx"] = { fg = colors.magenta }
        highlights["@variable.parameter"] = { fg = colors.fg_dark }
        highlights["@variable.builtin.javascript"] = { fg = colors.teal }
        highlights["@variable.builtin.typescript"] = { fg = colors.teal }
        highlights["@variable.builtin.tsx"] = { fg = colors.teal }
        highlights["@lsp.typemod.variable.defaultLibrary.typescriptreact"] = { fg = colors.teal }
        highlights["Search"] = { bg = "#5e428f" }
        highlights["IncSearch"] = { bg = colors.magenta, fg = colors.bg_dark }
        highlights["Title"] = { fg = "#c0caf5" }
        highlights["DiagnosticSignError"] = { fg = "#4a1b22" }
        highlights["DiagnosticSignWarn"] = { fg = "#7a3f22" }
        highlights["DiagnosticSignInfo"] = { fg = "#2a4373" }
        highlights["DiagnosticSignHint"] = { fg = "#364a23" }
        highlights["DiagnosticVirtualTextError"] = { fg = "#3b4050" }
        highlights["CursorLineNr"] = { fg = "#737aa2" }
        highlights["DiagnosticUnnecessary"] = { fg = "#6F7E99" }
        highlights["FloatBorder"] = { fg = "#737aa2" }
      end,
    })

    vim.defer_fn(function()
      vim.cmd([[ 
      hi HighlightUrl guifg=#5f8ae9 gui=underline
      hi DiagnosticUnderlineError guisp=#7a3844 gui=undercurl
    ]])
    end, 0)
  end,
  vscode = function()
    vim.o.background = "dark"
    require("vscode").setup()
  end,
  onedark = function()
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
}

local selectedTheme = vim.g.os_theme or "tokyonight"
themes[selectedTheme]()

pcall(vim.cmd.colorscheme, selectedTheme)

---@class Palette
local ret = {
  bg = "#24283b",
  bg_dark = "#1f2335",
  bg_dark1 = "#1b1e2d",
  bg_highlight = "#292e42",
  blue = "#7aa2f7",
  blue0 = "#3d59a1",
  blue1 = "#2ac3de",
  blue2 = "#0db9d7",
  blue5 = "#89ddff",
  blue6 = "#b4f9f8",
  blue7 = "#394b70",
  comment = "#565f89",
  cyan = "#7dcfff",
  dark3 = "#545c7e",
  dark5 = "#737aa2",
  fg = "#c0caf5",
  fg_dark = "#a9b1d6",
  fg_gutter = "#3b4261",
  green = "#9ece6a",
  green1 = "#73daca",
  green2 = "#41a6b5",
  magenta = "#bb9af7",
  magenta2 = "#ff007c",
  orange = "#ff9e64",
  purple = "#9d7cd8",
  red = "#f7768e",
  red1 = "#db4b4b",
  teal = "#1abc9c",
  terminal_black = "#414868",
  yellow = "#e0af68",
  git = {
    add = "#449dab",
    change = "#6183bb",
    delete = "#914c54",
  },
}
return ret
