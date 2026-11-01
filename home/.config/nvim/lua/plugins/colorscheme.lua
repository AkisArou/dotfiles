-- 'background' is left unset: Neovim takes it from the terminal, foot's
-- colors-dark or colors-light, and follows a live switch by reloading the
-- colorscheme. The overrides below are picked again on every load.
local themes = {
  dark = {
    bg = "#18191A",
    lualine_fg = "#AEAFAD",
    color_overrides = {
      vscBack = "#18191A",
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
      DiagnosticError = { bg = "#18191A", fg = "#E4676B" },
      DiagnosticUnderlineError = { fg = "NONE", bg = "NONE", undercurl = true, sp = "#A0484B" },
      FzfLuaBorder = { fg = "#444444" },
      FzfLuaHeaderText = { fg = "#343B41" },
      FzfLuaHeaderBind = { fg = "#343B41" },
      -- codediff.lua
      DiffLineInsert = { bg = "#373d29" },
      DiffCharInsert = { bg = "#4b5632" },
      DiffLineDelete = { bg = "#4b1818" },
      DiffCharDelete = { bg = "#6f1313" },
    },
  },
  -- The same choices on Light+: vscode.nvim's light palette, with the dark
  -- overrides' blue tints and greys mirrored onto white.
  light = {
    bg = "#FFFFFF",
    lualine_fg = "#616161",
    color_overrides = {},
    group_overrides = {
      Search = { fg = "NONE", bg = "#b8d7ee" },
      IncSearch = { fg = "NONE", bg = "#c1c1c1" },
      CurSearch = { fg = "NONE", bg = "#e8f2f9" },
      SnippetTabStop = { bg = "#e8f2f9" },
      SnippetTabStopActive = { bg = "#e8f2f9" },
      MiniCursorword = { bg = "#e1eef8" },
      MiniCursorwordCurrent = { bg = "#e8f2f9" },
      DiagnosticError = { bg = "#FFFFFF", fg = "#CD3131" },
      DiagnosticUnderlineError = { fg = "NONE", bg = "NONE", undercurl = true, sp = "#DC6F6F" },
      FzfLuaBorder = { fg = "#dddddd" },
      FzfLuaHeaderText = { fg = "#cdd0d4" },
      FzfLuaHeaderBind = { fg = "#cdd0d4" },
      -- codediff.lua
      DiffLineInsert = { bg = "#ebf1dd" },
      DiffCharInsert = { bg = "#dbe6c2" },
      DiffLineDelete = { bg = "#ffcccc" },
      DiffCharDelete = { bg = "#ffa3a3" },
    },
  },
}

local function current()
  return themes[vim.o.background] or themes.dark
end

vim.api.nvim_create_autocmd("ColorSchemePre", {
  pattern = "vscode",
  callback = function()
    local theme = current()
    require("vscode").setup({
      color_overrides = theme.color_overrides,
      group_overrides = theme.group_overrides,
    })
  end,
})

local M = {}

-- vscode's lualine theme with the background of the editor. lualine calls
-- this again when the colorscheme or 'background' changes.
function M.lualine_theme()
  package.loaded["lualine.themes.vscode"] = nil
  local vscode_lualine = require("lualine.themes.vscode")
  local theme = current()

  for _, mode in ipairs({ "normal", "insert", "visual", "replace", "command", "inactive" }) do
    for _, section in ipairs({ "b", "c" }) do
      local colors = vscode_lualine[mode][section]
      if colors then
        colors.bg = theme.bg
        colors.fg = theme.lualine_fg
      end
    end
  end

  return vscode_lualine
end

vim.cmd.colorscheme("vscode")

return M
