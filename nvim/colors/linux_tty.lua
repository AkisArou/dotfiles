-- linux_tty.lua
-- Minimal Linux virtual console–inspired colorscheme
-- Designed for low visual noise and high signal

vim.opt.termguicolors = true
vim.g.colors_name = "linux_tty"

local c = {
  accent = "#9d7cd8",
  black = "#0c0c0c",
  red = "#ff4444",
  green = "#009966",
  yellow = "#aaaa00",
  orange = "#e17100",
  blue = "#6666ee",
  magenta = "#cc33cc",
  cyan = "#00aaaa",
  light_gray = "#999999",
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

hi("Visual", { bg = "#1a1a1a" })
hi("Search", { fg = c.black, bg = c.orange })
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

hi("Keyword", { fg = c.accent })
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
  error = "#aa0000",
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
hi("@keyword.return", { fg = c.magenta })

hi("@type", { fg = c.cyan })
hi("@type.builtin", { fg = c.cyan })

-- Functions are *not* highlighted loudly
hi("@function", { fg = c.light_gray })
hi("@function.call", { fg = c.light_gray })

-- Variables intentionally uncolored
hi("@variable", { fg = c.light_gray })
hi("@variable.builtin", { fg = c.light_gray })

-- Punctuation stays neutral
hi("@punctuation", { fg = c.light_gray })

--------------------------------------------------
-- LSP semantic tokens (keep restrained)
--------------------------------------------------
hi("@lsp.type.function", { link = "@function" })
hi("@lsp.type.variable", { link = "@variable" })
hi("@lsp.type.keyword", { link = "@keyword" })
hi("@lsp.type.type", { link = "@type" })
hi("@lsp.type.property", { fg = "#777777" })

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

--------------------------------------------------
-- Oil
--------------------------------------------------
hi("OilDir", { fg = c.blue })
