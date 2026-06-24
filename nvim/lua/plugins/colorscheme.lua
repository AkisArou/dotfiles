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
        Visual = { bg = "#313131" },
        Search = { fg = "NONE", bg = "#2a4856" },
        IncSearch = { fg = "NONE", bg = "#444444" },
        CurSearch = { fg = "NONE", bg = "#17262d" },
        SnippetTabStop = { bg = "#17262d" },
        SnippetTabStopActive = { bg = "#17262d" },
        MiniCursorword = { bg = "#1a2d38" },
        MiniCursorwordCurrent = { bg = "#17262d" },
        PmenuSel = { bg = "#343B41" },
        DiagnosticError = { bg = bgColor, fg = "#E4676B" },
        DiagnosticUnderlineError = { fg = "NONE", bg = "NONE", undercurl = true, sp = "#A0484B" },
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
}

local selectedTheme = vim.g.os_theme or "vscode"

if themes[selectedTheme] then
  themes[selectedTheme]()
end

pcall(vim.cmd.colorscheme, selectedTheme)
