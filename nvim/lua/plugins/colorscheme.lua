local themes = {
  tokyonight = {
    name = "tokyonight",
    repo = "folke/tokyonight.nvim",
    config = function()
      vim.opt.background = "dark"
    end,
  },
  vscode = {
    name = "vscode",
    repo = "Mofiqul/vscode.nvim",
    branch = "main",
    config = function()
      vim.o.background = "dark"
      require("vscode").setup()
      -- local c = require("vscode.colors")
      --
      -- local hl = vim.api.nvim_set_hl
      -- hl(0, "BlinkCmpKindVariable", { fg = c.vscLightBlue, bg = "NONE" })
      -- hl(0, "BlinkCmpKindnterface", { fg = c.vscLightBlue, bg = "NONE" })
      -- hl(0, "BlinkCmpKindText", { fg = c.vscLightBlue, bg = "NONE" })
      -- hl(0, "BlinkCmpKindFunction", { fg = c.vscPink, bg = "NONE" })
      -- hl(0, "BlinkCmpKindMethod", { fg = c.vscPink, bg = "NONE" })
      -- hl(0, "BlinkCmpKindKeyword", { fg = c.vscFront, bg = "NONE" })
      -- hl(0, "BlinkCmpKindProperty", { fg = c.vscFront, bg = "NONE" })
      -- hl(0, "BlinkCmpKindUnit", { fg = c.vscFront, bg = "NONE" })
      -- hl(0, "BlinkCmpKindConstructor", { fg = c.vscUiOrange, bg = "NONE" })
      -- hl(0, "BlinkCmpMenu", { fg = c.vscPopupFront, bg = "NONE" })
      -- hl(0, "BlinkCmpLabelDeprecated", { fg = c.vscCursorDark, bg = c.vscPopupBack, strikethrough = true })
      -- hl(0, "BlinkCmpLabelMatch", { fg = c.vscMediumBlue, bg = "NONE", bold = true })
      -- more...
    end,
  },
  darcula = {
    name = "darcula-solid",
    repo = "briones-gabriel/darcula-solid.nvim",
    dependencies = { "rktjmp/lush.nvim" },
  },
  gruvbox = {
    name = "gruvbox-material",
    repo = "sainnhe/gruvbox-material",
    config = function()
      vim.g.gruvbox_material_background = "hard"
    end,
  },
}

-- local selectedTheme = themes[os.getenv("THEME") or "vscode"]
local selectedTheme = themes.gruvbox

local M = {
  selectedTheme.repo,
  dependencies = selectedTheme.dependencies,
  branch = selectedTheme.branch,
  event = "VimEnter",
  opts = selectedTheme.opts,
  lazy = false,
  priority = 1000,
}

function M.config()
  if selectedTheme.config ~= nil then
    selectedTheme.config()
  end

  local status_ok, _ = pcall(vim.cmd.colorscheme, selectedTheme.name)
  if not status_ok then
    return
  end
end

return M
