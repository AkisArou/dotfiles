-- linux_tty.lua
-- Minimal Linux virtual console–inspired colorscheme
-- Designed for low visual noise and high signal

vim.opt.termguicolors = true
vim.g.colors_name = "linux_tty"

local c = {
  black = "#000000",
  red = "#aa0000",
  green = "#00aa00",
  yellow = "#aaaa00",
  orange = "#aa5500",
  blue = "#6666ee",
  magenta = "#cc33cc",
  cyan = "#00aaaa",
  light_gray = "#999999",
  dark_gray = "#555555",
  white = "#ffffff",
}

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

hi("StatusLine", { fg = c.light_gray, bg = "#111111" })
hi("StatusLineNC", { fg = c.dark_gray, bg = "#111111" })

hi("Visual", { bg = "#222222" })
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

hi("Keyword", { fg = c.magenta })
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
hi("DiagnosticError", { fg = c.red })
hi("DiagnosticWarn", { fg = c.orange })
hi("DiagnosticInfo", { fg = c.cyan })
hi("DiagnosticHint", { fg = c.dark_gray })

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

hi("@keyword", { fg = c.magenta })
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

--------------------------------------------------
-- Blink.cmp
--------------------------------------------------
hi("BlinkCmpMenu", { bg = c.black })
hi("BlinkCmpMenuSelection", { bg = "#212121" })

--------------------------------------------------
-- Lualine
--------------------------------------------------
hi("lualine_c_normal", { bg = c.black })
hi("lualine_x_normal", { bg = c.black })
