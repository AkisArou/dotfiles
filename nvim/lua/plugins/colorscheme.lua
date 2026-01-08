local themes = {
  tokyonight = function()
    require("tokyonight").setup({
      style = "night",
      on_highlights = function(highlights, colors)
        highlights["@tag.tsx"] = { fg = colors.magenta }
        highlights["@variable.parameter"] = { fg = colors.fg_dark }
        highlights["@variable.builtin.javascript"] = { fg = colors.teal }
        highlights["@variable.builtin.typescript"] = { fg = colors.teal }
        highlights["@variable.builtin.tsx"] = { fg = colors.blue }
        highlights["@function.method.call"] = { fg = colors.blue }
        highlights["@function.method.call.tsx"] = { fg = colors.blue }
        highlights["@lsp.type.enum"] = { fg = colors.teal }
        highlights["@lsp.typemod.variable.defaultLibrary.typescriptreact"] = { fg = colors.blue }
        highlights["Special"] = { fg = colors.blue }
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
        highlights["MatchParen"] = { fg = "#7aa2f7", bg = "#394b70", bold = true }
        highlights["Type"] = { fg = colors.fg_dark }
        highlights["@variable.member"] = { fg = colors.fg_dark }
        highlights["@variable.member.tsx"] = { fg = colors.fg_dark }
        highlights["@property"] = { fg = colors.fg_dark }
        highlights["@keyword.import"] = { fg = colors.magenta }
      end,
    })

    vim.cmd([[
      hi manOptionDesc guifg=#41a6b5
    ]])

    vim.defer_fn(function()
      vim.cmd([[ 
        hi HighlightUrl guifg=#5f8ae9 gui=underline
        hi DiagnosticUnderlineError guisp=#7a3844 gui=undercurl
        hi FzfLuaHeaderText guifg=#434a65
        hi FzfLuaHeaderBind guifg=#434a65
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
