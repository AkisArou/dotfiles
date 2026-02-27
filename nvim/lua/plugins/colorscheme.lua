local themes = {
  vscode = function()
    local bgColor = "#18191A"

    vim.o.background = "dark"
    vim.g.disable_blink_treesitter = true

    require("vscode").setup({
      color_overrides = {
        vscBack = bgColor,
        vscOrange = "#C48081",
      },
      group_overrides = {
        Search = { fg = "NONE", bg = "#2a4856" },
        IncSearch = { fg = "NONE", bg = "#242526" },
        CurSearch = { fg = "NONE", bg = "#17262d" },
        MiniCursorword = { bg = "#1a2d38" },
        MiniCursorwordCurrent = { bg = "#17262d" },
        PmenuSel = { bg = "#343B41" },
        DiagnosticError = { bg = bgColor, fg = "#F44747" },
        FzfLuaBorder = { fg = "#444444" },
        FzfLuaHeaderText = { fg = "#343B41" },
        FzfLuaHeaderBind = { fg = "#343B41" },
      },
    })

    -- Create custom lualine theme with overridden backgrounds
    local vscode_lualine = require("lualine.themes.vscode")

    local lualineFgColor = "#AEAFAD"
    vscode_lualine.normal.b.bg = bgColor
    vscode_lualine.normal.b.fg = lualineFgColor
    vscode_lualine.normal.c.bg = bgColor
    vscode_lualine.normal.c.fg = lualineFgColor
    vscode_lualine.insert.b.bg = bgColor
    vscode_lualine.insert.b.fg = lualineFgColor
    vscode_lualine.insert.c.bg = bgColor
    vscode_lualine.insert.c.fg = lualineFgColor
    vscode_lualine.visual.b.bg = bgColor
    vscode_lualine.visual.b.fg = lualineFgColor
    vscode_lualine.replace.b.bg = bgColor
    vscode_lualine.replace.b.fg = lualineFgColor
    vscode_lualine.replace.c.bg = bgColor
    vscode_lualine.replace.c.fg = lualineFgColor
    vscode_lualine.command.b.bg = bgColor
    vscode_lualine.command.b.fg = lualineFgColor
    vscode_lualine.command.c.bg = bgColor
    vscode_lualine.command.c.fg = lualineFgColor
    vscode_lualine.inactive.b.bg = bgColor
    vscode_lualine.inactive.b.fg = lualineFgColor
    vscode_lualine.inactive.c.bg = bgColor
    vscode_lualine.inactive.c.fg = lualineFgColor

    -- Store globally so lualine.lua can use it
    vim.g.lualine_theme_override = vscode_lualine
  end,
  tokyonight = function()
    require("tokyonight").setup({
      style = "night",
      -- bg = "#24283b",
      -- bg_dark = "#1f2335",
      -- bg_dark1 = "#1b1e2d",
      -- bg_highlight = "#292e42",
      -- blue = "#7aa2f7",
      -- blue0 = "#3d59a1",
      -- blue1 = "#2ac3de",
      -- blue2 = "#0db9d7",
      -- blue5 = "#89ddff",
      -- blue6 = "#b4f9f8",
      -- blue7 = "#394b70",
      -- comment = "#565f89",
      -- cyan = "#7dcfff",
      -- dark3 = "#545c7e",
      -- dark5 = "#737aa2",
      -- fg = "#c0caf5",
      -- fg_dark = "#a9b1d6",
      -- fg_gutter = "#3b4261",
      -- green = "#9ece6a",
      -- green1 = "#73daca",
      -- green2 = "#41a6b5",
      -- magenta = "#bb9af7",
      -- magenta2 = "#ff007c",
      -- orange = "#ff9e64",
      -- purple = "#9d7cd8",
      -- red = "#f7768e",
      -- red1 = "#db4b4b",
      -- teal = "#1abc9c",
      -- terminal_black = "#414868",
      -- yellow = "#e0af68",
      -- git = {
      --   add = "#449dab",
      --   change = "#6183bb",
      --   delete = "#914c54",
      -- }
      on_highlights = function(highlights, colors)
        highlights["Visual"] = { bg = "#24283b" }
        highlights["CursorLine"] = { bg = "#24283b" }
        highlights["MiniCursorword"] = { bg = "#292e42" }
        highlights["MiniCursorwordCurrent"] = { bg = "#292e42" }
        highlights["@tag.tsx"] = { fg = colors.magenta }
        highlights["@variable.parameter"] = { fg = colors.fg_dark }
        highlights["@variable.builtin.javascript"] = { fg = colors.teal }
        highlights["@variable.builtin.typescript"] = { fg = colors.teal }
        highlights["@variable.builtin.tsx"] = { fg = colors.blue }
        highlights["@function.method.call"] = { fg = colors.blue }
        highlights["@function.method.call.tsx"] = { fg = colors.blue }
        highlights["@lsp.type.enum"] = { fg = colors.teal }
        highlights["@lsp.typemod.variable.defaultLibrary.typescriptreact"] = { fg = colors.blue }
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
        highlights["MatchParen"] = { fg = "#737aa2", bg = "#282c3f", bold = true }
        highlights["Type"] = { fg = colors.fg_dark }
        highlights["@variable.member"] = { fg = colors.fg_dark }
        highlights["@variable.member.tsx"] = { fg = colors.fg_dark }
        highlights["@property"] = { fg = colors.fg_dark }
        highlights["@keyword.import"] = { fg = colors.magenta }
        highlights["TabLineSel"] = { fg = colors.fg, bg = colors.bg_dark1 }
        highlights["@tag.builtin.tsx"] = { fg = colors.blue }
        highlights["SnacksInputBorder"] = { fg = colors.bg_highlight }
        highlights["SnacksInputTitle"] = { fg = colors.comment }
      end,
    })

    vim.cmd([[
      hi manOptionDesc guifg=#41a6b5
    ]])

    vim.defer_fn(function()
      vim.cmd([[ 
        hi HighlightUrl guifg=#5f8ae9 gui=underline
        hi DiagnosticUnderlineError guisp=#7a3844 gui=undercurl
        hi FzfLuaBorder guifg=#1f1f1f
        hi FzfLuaHeaderText guifg=#434a65
        hi FzfLuaHeaderBind guifg=#434a65
        hi MiniIconsAzure guifg=#7aa2f7
        hi MiniIconsGreen guifg=#7aa2f7
        hi MiniIconsYellow guifg=#7aa2f7
        hi MiniIconsGray guifg=#7aa2f7
        hi MiniIconsOrange guifg=#7aa2f7
        hi MiniIconsPurple guifg=#7aa2f7
        hi MiniIconsRed guifg=#7aa2f7
      ]])
    end, 0)
  end,
}

local selectedTheme = vim.g.os_theme or "vscode"

if themes[selectedTheme] then
  themes[selectedTheme]()
end

pcall(vim.cmd.colorscheme, selectedTheme)
