-- nevermore.lua
-- Designed for low visual noise and high signal

vim.opt.termguicolors = true
vim.g.colors_name = "nevermore"

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
hi("CursorLine", { bg = "#0d0d0d" })
hi("LineNr", { fg = c.dark_gray })
hi("CursorLineNr", { fg = c.light_gray })
hi("WinSeparator", { fg = c.black })

hi("StatusLine", { fg = c.light_gray, bg = c.black })
hi("StatusLineNC", { fg = c.dark_gray, bg = c.black })

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

hi("TreesitterContext", { bg = "#0d0d0d" })
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

-- Markdown
hi("@markup.heading.3.markdown", { guibg = #222222, cterm = nil, gui = nil })
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
hi("MiniCursorwordCurrent", { bg = "#222222" })
hi("MiniCursorword", { bg = "#333333" })

--------------------------------------------------
-- Blink.cmp
--------------------------------------------------
hi("BlinkCmpMenu", { bg = "#000000" })
hi("BlinkCmpMenuSelection", { bg = "#222222" })

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
-- render-markdown
--------------------------------------------------
hi("RenderMarkdownCode", { fg = c.magenta, bg = c.black })
hi("RenderMarkdownH1Bg", { fg = c.magenta })
hi("RenderMarkdownH2Bg", { fg = c.cyan })
hi("RenderMarkdownH3Bg", { bg = c.dark_gray })
--------------------------------------------------
-- WhichKey
--------------------------------------------------
hi("WhichKeyNormal", { bg = "#0d0d0d" })

--------------------------------------------------
-- Mason
--------------------------------------------------
hi("MasonNormal", { bg = c.black, fg = c.light_gray })
hi("MasonHeader", { bg = c.magenta, fg = c.black })
hi("MasonHighlight", { fg = c.cyan })
hi("MasonHighlightBlockBold", { bg = c.cyan, fg = c.black })
hi("MasonMutedBlock", { bg = "#333333" })

--------------------------------------------------
-- Lualine
--------------------------------------------------
local function set_lualine_highlights()
  hi("lualine_c_normal", { bg = c.black, fg = c.light_gray })
  hi("lualine_x_normal", { bg = c.black, fg = c.light_gray })
  hi("lualine_c_insert", { bg = c.black, fg = c.light_gray })
  hi("lualine_x_insert", { bg = c.black, fg = c.light_gray })
  hi("lualine_c_command", { bg = c.black, fg = c.light_gray })
  hi("lualine_x_command", { bg = c.black, fg = c.light_gray })
  hi("lualine_c_visual", { bg = c.black, fg = c.light_gray })
  hi("lualine_x_visual", { bg = c.black, fg = c.light_gray })
  hi("lualine_c_replace", { bg = c.black, fg = c.light_gray })
  hi("lualine_x_replace", { bg = c.black, fg = c.light_gray })
  hi("lualine_c_inactive", { bg = c.black, fg = c.dark_gray })
  hi("lualine_x_inactive", { bg = c.black, fg = c.dark_gray })
end

--------------------------------------------------
-- Intro screen (magenta until first buffer)
--------------------------------------------------
set_lualine_highlights()

hi("Normal", { fg = c.magenta, bg = c.black })
vim.api.nvim_create_autocmd("UIEnter", {
  once = true,
  callback = function()
    vim.defer_fn(function()
      vim.api.nvim_create_autocmd({ "BufReadPost", "BufNewFile", "InsertEnter" }, {
        once = true,
        callback = function()
          hi("Normal", { fg = c.light_gray, bg = c.black })
          set_lualine_highlights()
        end,
      })
    end, 0)
  end,
})
