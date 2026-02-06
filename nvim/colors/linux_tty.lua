-- linux_tty.lua
-- Minimal Linux virtual console–inspired colorscheme
-- Designed for low visual noise and high signal

vim.opt.termguicolors = true
vim.g.colors_name = "linux_tty"

local c = {
  accent_subtle = "#B8C8F7",
  black = "#111111",
  red = "#f7768e",
  green = "#009966",
  yellow = "#aaaa00",
  orange = "#ff9e64",
  blue = "#7aa2f7",
  magenta = "#9d7cd8",
  cyan = "#7dcfff",
  light_gray = "#bbbbbb",
  dark_gray = "#555555",
  white = "#ffffff",
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
hi("Normal", { fg = c.light_gray, bg = c.black })
hi("Cursor", { fg = c.black, bg = c.light_gray })
hi("CursorLine", { bg = "#111111" })
hi("LineNr", { fg = c.dark_gray })
hi("CursorLineNr", { fg = c.light_gray })
hi("WinSeparator", { fg = "#111111" })

hi("StatusLine", { fg = c.light_gray, bg = "#111111" })
hi("StatusLineNC", { fg = c.dark_gray, bg = "#111111" })

hi("Visual", { bg = "#313131" })
hi("Search", { fg = c.light_gray, bg = "#1a2d5f" })
hi("CurSearch", { fg = c.light_gray, bg = "#0f1a38" })
hi("Special", { fg = c.light_gray })

--------------------------------------------------
-- Basic syntax (fallback, non-TS)
--------------------------------------------------
hi("Comment", { fg = c.dark_gray, italic = true })
hi("String", { fg = c.green })
hi("Character", { fg = c.green })
hi("Number", { fg = c.magenta })
hi("Boolean", { fg = c.orange })
hi("Float", { fg = c.magenta })

hi("Keyword", { fg = c.blue })
hi("Conditional", { fg = c.magenta })
hi("Repeat", { fg = c.magenta })

hi("Type", { fg = c.cyan })
hi("StorageClass", { fg = c.cyan })

hi("Function", { fg = c.light_gray })
hi("Identifier", { fg = c.light_gray })

hi("Operator", { fg = c.light_gray })
hi("Delimiter", { fg = c.light_gray })

--------------------------------------------------
-- Diagnostics (subtle)
--------------------------------------------------
local diagnosticColor = {
  error = "#7a3844",
  warn = c.orange,
  info = c.cyan,
  hint = c.dark_gray,
}

hi("DiagnosticError", { fg = diagnosticColor.error, underline = false })
hi("DiagnosticWarn", { fg = diagnosticColor.warn, underline = false })
hi("DiagnosticInfo", { fg = diagnosticColor.info, underline = false })
hi("DiagnosticHint", { fg = diagnosticColor.hint })
hi("DiagnosticUnderlineError", { sp = diagnosticColor.error, underline = false, undercurl = true })
hi("DiagnosticUnderlineWarn", { sp = diagnosticColor.warn, underline = false, undercurl = true })
hi("DiagnosticUnderlineInfo", { sp = diagnosticColor.info, underline = false, undercurl = true })
hi("DiagnosticUnderlineHint", { sp = diagnosticColor.hint, underline = false, undercurl = true })

--------------------------------------------------
-- Tree-sitter (INTENTIONALLY MINIMAL)
--------------------------------------------------
-- Most nodes inherit Normal unless explicitly useful

hi("@comment", { link = "Comment" })

hi("@string", { fg = c.green })
hi("@string.escape", { fg = c.cyan })

hi("@number", { fg = c.magenta })
hi("@boolean", { fg = c.orange })
hi("@constant", { fg = c.magenta })

hi("@keyword", { link = "Keyword" })
hi("@keyword.conditional", { fg = c.magenta })
hi("@keyword.return", { fg = c.magenta })
hi("@keyword.repeat", { fg = c.magenta })

hi("@type", { fg = c.light_gray })
hi("@type.builtin", { fg = c.cyan })

-- Functions are *not* highlighted loudly
hi("@function", { fg = c.accent_subtle })
hi("@function.call", { fg = c.accent_subtle })

-- Variables intentionally uncolored
hi("@variable", { fg = c.light_gray })
hi("@variable.builtin", { fg = c.light_gray })

-- Punctuation stays neutral
hi("@punctuation", { fg = c.light_gray })

-- Tag
hi("@tag.tsx", { fg = c.accent_subtle })
hi("@tag.builtin.tsx", { fg = c.accent_subtle })

--------------------------------------------------
-- LSP semantic tokens (keep restrained)
--------------------------------------------------
hi("@lsp.type.function", { link = "@function" })
hi("@lsp.type.variable", { link = "@variable" })
hi("@lsp.type.keyword", { link = "@keyword" })
hi("@lsp.type.type", { link = "@type" })
hi("@lsp.type.property", { fg = "#aaaaaa" })

--------------------------------------------------
-- Blink.cmp
--------------------------------------------------
hi("BlinkCmpMenu", { bg = "#000000" })
hi("BlinkCmpMenuSelection", { bg = "#222222" })

--------------------------------------------------
-- Lualine
--------------------------------------------------
hi("lualine_c_normal", { bg = c.black })
hi("lualine_x_normal", { bg = c.black })

--------------------------------------------------
-- FZF
--------------------------------------------------
hi("FzfLuaBorder", { fg = c.dark_gray })
hi("FzfLuaHeaderText", { fg = c.dark_gray })
hi("FzfLuaHeaderBind", { fg = c.dark_gray })
hi("FzfLuaLivePrompt", { fg = c.light_gray })

--------------------------------------------------
-- Oil
--------------------------------------------------
hi("OilDir", { fg = "#7aa2f7" })

--------------------------------------------------
-- highlightUrl
--------------------------------------------------
hi("highlightUrl", { fg = "#0092b8" })

--------------------------------------------------
-- Avante
--------------------------------------------------
hi("AvanteTitle", { fg = c.green, bg = c.black })
hi("AvanteReversedTitle", { fg = c.black })
hi("AvanteSubtitle", { fg = c.light_gray, bg = c.black })
hi("AvanteReversedSubtitle", { fg = c.black })
hi("AvanteThirdTitle", { fg = c.light_gray, bg = c.black })
hi("AvanteReversedThirdTitle", { fg = c.black })

hi("AvanteSuggestion", { link = "Comment" })
hi("AvanteAnnotation", { link = "Comment" })
hi("AvantePopupHint", { link = "NormalFloat" })
hi("AvanteInlineHint", { link = "Keyword" })

hi("AvanteToBeDeleted", { bg = "#7a3844", strikethrough = true })
hi("AvanteToBeDeletedWOStrikethrough", { bg = "#551a1e" })

hi("AvanteConfirmTitle", { fg = c.black, bg = c.red })

hi("AvanteButtonDefault", { fg = c.black, bg = c.light_gray })
hi("AvanteButtonDefaultHover", { fg = c.black, bg = c.green })
hi("AvanteButtonPrimary", { fg = c.black, bg = c.light_gray })
hi("AvanteButtonPrimaryHover", { fg = c.black, bg = c.cyan })
hi("AvanteButtonDanger", { fg = c.black, bg = c.light_gray })
hi("AvanteButtonDangerHover", { fg = c.black, bg = c.red })

hi("AvantePromptInput", {})
hi("AvantePromptInputBorder", { link = "NormalFloat" })

hi("AvanteSidebarWinSeparator", { fg = "#222222", bg = c.black })
hi("AvanteSidebarWinHorizontalSeparator", { fg = "#555555", bg = c.black })
hi("AvanteSidebarNormal", { link = "Normal" })
hi("AvanteCommentFg", { fg = c.dark_gray })
hi("AvanteReversedNormal", { fg = c.black, bg = c.light_gray })

-- Spinners
hi("AvanteStateSpinnerGenerating", { fg = c.black, bg = "#9d7cd8" })
hi("AvanteStateSpinnerToolCalling", { fg = c.black, bg = c.cyan })
hi("AvanteStateSpinnerFailed", { fg = c.black, bg = c.red })
hi("AvanteStateSpinnerSucceeded", { fg = c.green })
hi("AvanteStateSpinnerSearching", { fg = c.black, bg = "#9d7cd8" })
hi("AvanteStateSpinnerThinking", { fg = c.black, bg = "#9d7cd8" })
hi("AvanteStateSpinnerCompacting", { fg = c.black, bg = "#9d7cd8" })

-- Tasks
hi("AvanteTaskRunning", { fg = "#9d7cd8", bg = c.black })
hi("AvanteTaskCompleted", { fg = c.green, bg = c.black })
hi("AvanteTaskFailed", { fg = c.red, bg = c.black })
hi("AvanteThinking", { fg = "#9d7cd8", bg = c.black })

-- Conflict
hi("AvanteConflictCurrent", { bg = "#551a1e", bold = true, fg = c.light_gray })
hi("AvanteConflictCurrentLabel", { bg = "#551a1e", bold = true, fg = c.light_gray })

hi("AvanteConflictIncoming", { bg = "#313131", fg = c.light_gray })
hi("AvanteConflictIncomingLabel", { bg = "#313131", fg = c.light_gray })

--------------------------------------------------
-- render-markdown
--------------------------------------------------
hi("RenderMarkdownCode", { fg = "#9d7cd8", bg = c.black })
