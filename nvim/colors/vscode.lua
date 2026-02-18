-- vscode.lua
-- VS Code Dark+ colorscheme for Neovim

vim.opt.termguicolors = true
vim.g.colors_name = "vscode"

local c = {
  -- UI colors
  front = "#D4D4D4",
  back = "#121314",

  tab_current = "#1F1F1F",
  tab_other = "#2D2D2D",
  tab_outside = "#252526",

  left_dark = "#252526",
  left_mid = "#373737",
  left_light = "#636369",

  popup_front = "#BBBBBB",
  popup_back = "#202020",
  popup_highlight_blue = "#04395E",
  popup_highlight_gray = "#343B41",

  split_light = "#898989",
  split_dark = "#444444",
  split_thumb = "#424242",

  cursor_dark_dark = "#222222",
  cursor_dark = "#51504F",
  cursor_light = "#AEAFAD",
  selection = "#264F78",
  line_number = "#5A5A5A",

  diff_red_dark = "#4B1818",
  diff_red_light = "#6F1313",
  diff_red_light_light = "#FB0101",
  diff_green_dark = "#373D29",
  diff_green_light = "#4B5632",
  search_current = "#515c6a",
  search = "#613315",

  git_added = "#81b88b",
  git_modified = "#e2c08d",
  git_deleted = "#c74e39",
  git_renamed = "#73c991",
  git_untracked = "#73c991",
  git_ignored = "#8c8c8c",
  git_stage_modified = "#e2c08d",
  git_stage_deleted = "#c74e39",
  git_conflicting = "#e4676b",
  git_submodule = "#8db9e2",

  context = "#404040",
  context_current = "#707070",

  fold_background = "#202d39",

  suggestion = "#6A6A6A",

  -- Syntax colors
  gray = "#808080",
  violet = "#646695",
  blue = "#569CD6",
  accent_blue = "#4FC1FF",
  dark_blue = "#223E55",
  medium_blue = "#18a2fe",
  disabled_blue = "#729DB3",
  light_blue = "#9CDCFE",
  green = "#6A9955",
  blue_green = "#4EC9B0",
  light_green = "#B5CEA8",
  red = "#F44747",
  orange = "#CE9178",
  light_red = "#D16969",
  yellow_orange = "#D7BA7D",
  yellow = "#DCDCAA",
  dark_yellow = "#FFD602",
  pink = "#C586C0",

  dim_highlight = "#51504F",
}

---@param group string
---@param opts vim.api.keyset.highlight
local function hi(group, opts)
  vim.api.nvim_set_hl(0, group, opts)
end

-- Reset
vim.cmd("highlight clear")
vim.cmd("syntax reset")

--------------------------------------------------
-- Core UI
--------------------------------------------------
hi("Normal", { fg = c.front, bg = c.back })
hi("Cursor", { fg = c.back, bg = c.front })
hi("CursorLine", { bg = c.cursor_dark_dark })
hi("LineNr", { fg = c.line_number })
hi("CursorLineNr", { fg = c.front })
hi("WinSeparator", { fg = c.split_dark })

hi("StatusLine", { fg = c.front, bg = c.left_dark })
hi("StatusLineNC", { fg = c.left_light, bg = c.left_dark })

hi("Visual", { bg = c.selection })
hi("Search", { fg = c.front, bg = c.search })
hi("CurSearch", { fg = c.front, bg = c.search_current })
hi("IncSearch", { fg = c.front, bg = c.search_current })
hi("Special", { fg = c.yellow_orange })

hi("Pmenu", { fg = c.popup_front, bg = c.popup_back })
hi("PmenuSel", { bg = c.popup_highlight_blue })
hi("PmenuSbar", { bg = c.popup_back })
hi("PmenuThumb", { bg = c.split_thumb })

hi("TabLine", { fg = c.front, bg = c.tab_other })
hi("TabLineSel", { fg = c.front, bg = c.tab_current })
hi("TabLineFill", { bg = c.tab_outside })

hi("VertSplit", { fg = c.split_dark })
hi("ColorColumn", { bg = c.cursor_dark_dark })
hi("SignColumn", { bg = c.back })
hi("FoldColumn", { fg = c.line_number, bg = c.back })
hi("Folded", { fg = c.left_light, bg = c.fold_background })

hi("MatchParen", { bg = c.cursor_dark })
hi("NonText", { fg = c.left_mid })
hi("SpecialKey", { fg = c.left_mid })
hi("Whitespace", { fg = c.left_mid })

hi("Directory", { fg = c.blue })
hi("Title", { fg = c.blue, bold = true })
hi("ErrorMsg", { fg = c.red })
hi("WarningMsg", { fg = c.orange })
hi("MoreMsg", { fg = c.green })
hi("Question", { fg = c.blue })

--------------------------------------------------
-- Basic syntax (fallback, non-TS)
--------------------------------------------------
hi("Comment", { fg = c.green, italic = true })
hi("String", { fg = c.orange })
hi("Character", { fg = c.orange })
hi("Number", { fg = c.light_green })
hi("Boolean", { fg = c.blue })
hi("Float", { fg = c.light_green })

hi("Keyword", { fg = c.blue })
hi("Conditional", { fg = c.pink })
hi("Repeat", { fg = c.pink })
hi("Statement", { fg = c.pink })
hi("Label", { fg = c.pink })
hi("Exception", { fg = c.pink })

hi("Type", { fg = c.blue_green })
hi("StorageClass", { fg = c.blue })
hi("Structure", { fg = c.blue_green })
hi("Typedef", { fg = c.blue_green })

hi("Function", { fg = c.yellow })
hi("Identifier", { fg = c.light_blue })

hi("Operator", { fg = c.front })
hi("Delimiter", { fg = c.front })

hi("PreProc", { fg = c.pink })
hi("Include", { fg = c.pink })
hi("Define", { fg = c.pink })
hi("Macro", { fg = c.pink })
hi("PreCondit", { fg = c.pink })

hi("Constant", { fg = c.blue })
hi("SpecialChar", { fg = c.yellow_orange })
hi("Tag", { fg = c.blue })
hi("Debug", { fg = c.pink })
hi("Underlined", { fg = c.accent_blue, underline = true })
hi("Ignore", { fg = c.left_mid })
hi("Error", { fg = c.red })
hi("Todo", { fg = c.back, bg = c.yellow, bold = true })

--------------------------------------------------
-- Diff
--------------------------------------------------
hi("DiffAdd", { bg = c.diff_green_dark })
hi("DiffChange", { bg = c.diff_green_dark })
hi("DiffDelete", { bg = c.diff_red_dark })
hi("DiffText", { bg = c.diff_green_light })
hi("Added", { fg = c.git_added })
hi("Changed", { fg = c.git_modified })
hi("Removed", { fg = c.git_deleted })

--------------------------------------------------
-- Diagnostics
--------------------------------------------------
hi("DiagnosticError", { fg = c.red })
hi("DiagnosticWarn", { fg = c.git_modified })
hi("DiagnosticInfo", { fg = c.blue })
hi("DiagnosticHint", { fg = c.blue_green })
hi("DiagnosticUnderlineError", { sp = c.red, undercurl = true })
hi("DiagnosticUnderlineWarn", { sp = c.git_modified, undercurl = true })
hi("DiagnosticUnderlineInfo", { sp = c.blue, undercurl = true })
hi("DiagnosticUnderlineHint", { sp = c.blue_green, undercurl = true })

--------------------------------------------------
-- Tree-sitter
--------------------------------------------------
hi("TreesitterContext", { bg = c.context })

hi("@comment", { link = "Comment" })

hi("@string", { fg = c.orange })
hi("@string.escape", { fg = c.yellow_orange })
hi("@string.regex", { fg = c.light_red })

hi("@number", { fg = c.light_green })
hi("@number.float", { fg = c.light_green })
hi("@boolean", { fg = c.blue })
hi("@constant", { fg = c.blue })
hi("@constant.builtin", { fg = c.blue })

hi("@keyword", { fg = c.blue })
hi("@keyword.conditional", { fg = c.pink })
hi("@keyword.return", { fg = c.pink })
hi("@keyword.repeat", { fg = c.pink })
hi("@keyword.function", { fg = c.blue })
hi("@keyword.operator", { fg = c.blue })
hi("@keyword.import", { fg = c.pink })

hi("@type", { fg = c.blue_green })
hi("@type.builtin", { fg = c.blue })

hi("@function", { fg = c.yellow })
hi("@function.call", { fg = c.yellow })
hi("@function.method", { fg = c.yellow })
hi("@function.method.call", { fg = c.yellow })
hi("@function.builtin", { fg = c.yellow })

hi("@variable", { fg = c.light_blue })
hi("@variable.builtin", { fg = c.blue })
hi("@variable.parameter", { fg = c.light_blue })
hi("@variable.member", { fg = c.light_blue })

hi("@property", { fg = c.light_blue })
hi("@attribute", { fg = c.blue_green })

hi("@punctuation", { fg = c.front })
hi("@punctuation.bracket", { fg = c.front })
hi("@punctuation.delimiter", { fg = c.front })

hi("@operator", { fg = c.front })

hi("@tag", { fg = c.blue })
hi("@tag.attribute", { fg = c.light_blue })
hi("@tag.delimiter", { fg = c.gray })
hi("@tag.tsx", { fg = c.blue })
hi("@tag.builtin.tsx", { fg = c.blue })

hi("@constructor", { fg = c.blue_green })

hi("@namespace", { fg = c.blue_green })
hi("@module", { fg = c.blue_green })

--------------------------------------------------
-- LSP semantic tokens
--------------------------------------------------
hi("@lsp.type.function", { fg = c.yellow })
hi("@lsp.type.method", { fg = c.yellow })
hi("@lsp.type.variable", { fg = c.light_blue })
hi("@lsp.type.parameter", { fg = c.light_blue })
hi("@lsp.type.property", { fg = c.light_blue })
hi("@lsp.type.keyword", { fg = c.blue })
hi("@lsp.type.type", { fg = c.blue_green })
hi("@lsp.type.class", { fg = c.blue_green })
hi("@lsp.type.interface", { fg = c.blue_green })
hi("@lsp.type.enum", { fg = c.blue_green })
hi("@lsp.type.enumMember", { fg = c.blue })
hi("@lsp.type.namespace", { fg = c.blue_green })
hi("@lsp.type.struct", { fg = c.blue_green })

--------------------------------------------------
-- Git signs
--------------------------------------------------
hi("GitSignsAdd", { fg = c.git_added })
hi("GitSignsChange", { fg = c.git_modified })
hi("GitSignsDelete", { fg = c.git_deleted })

--------------------------------------------------
-- Blink.cmp / MiniCursorword
--------------------------------------------------
hi("MiniCursorwordCurrent", { bg = c.cursor_dark_dark })
hi("MiniCursorword", { bg = c.context })

--------------------------------------------------
-- Blink.cmp
--------------------------------------------------
hi("BlinkCmpMenu", { bg = c.popup_back })
hi("BlinkCmpMenuSelection", { bg = c.popup_highlight_blue })

--------------------------------------------------
-- Lualine
--------------------------------------------------
hi("lualine_c_normal", { bg = c.left_dark })
hi("lualine_x_normal", { bg = c.left_dark })

--------------------------------------------------
-- FZF
--------------------------------------------------
hi("FzfLuaBorder", { fg = c.split_dark })
hi("FzfLuaHeaderText", { fg = c.left_light })
hi("FzfLuaHeaderBind", { fg = c.left_light })
hi("FzfLuaLivePrompt", { fg = c.front })

--------------------------------------------------
-- Oil
--------------------------------------------------
hi("OilDir", { fg = c.blue })

--------------------------------------------------
-- highlightUrl
--------------------------------------------------
hi("highlightUrl", { fg = c.accent_blue })

--------------------------------------------------
-- render-markdown
--------------------------------------------------
hi("RenderMarkdownCode", { fg = c.orange, bg = c.back })
hi("RenderMarkdownH1Bg", { fg = c.blue })
hi("RenderMarkdownH2Bg", { fg = c.blue_green })

--------------------------------------------------
-- WhichKey
--------------------------------------------------
hi("WhichKeyNormal", { bg = c.popup_back })

--------------------------------------------------
-- Neogit
--------------------------------------------------
hi("NeogitRemote", { fg = c.pink })

--------------------------------------------------
-- Mason
--------------------------------------------------
hi("MasonNormal", { bg = c.back, fg = c.front })
hi("MasonHeader", { bg = c.blue, fg = c.back })
hi("MasonHighlight", { fg = c.blue_green })
hi("MasonHighlightBlockBold", { bg = c.blue_green, fg = c.back })
hi("MasonMutedBlock", { bg = c.left_mid })
