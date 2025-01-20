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
  },
  onedark = {
    name = "onedark",
    repo = "navarasu/onedark.nvim",
    config = function()
      local colors = require("onedark.palette").dark

      -- black = "#181a1f",
      -- bg0 = "#282c34",
      -- bg1 = "#31353f",
      -- bg2 = "#393f4a",
      -- bg3 = "#3b3f4c",
      -- bg_d = "#21252b",
      -- bg_blue = "#73b8f1",
      -- bg_yellow = "#ebd09c",
      -- fg = "#abb2bf",
      -- purple = "#c678dd",
      -- green = "#98c379",
      -- orange = "#d19a66",
      -- blue = "#61afef",
      -- yellow = "#e5c07b",
      -- cyan = "#56b6c2",
      -- red = "#e86671",
      -- grey = "#5c6370",
      -- light_grey = "#848b98",
      -- dark_cyan = "#2b6f77",
      -- dark_red = "#993939",
      -- dark_yellow = "#93691d",
      -- dark_purple = "#8a3fa0",
      -- diff_add = "#31392b",
      -- diff_delete = "#382b2c",
      -- diff_change = "#1c3448",
      -- diff_text = "#2c5372",

      require("onedark").setup({
        highlights = {
          ["@keyword.import"] = { fg = colors.blue },
          ["@keyword"] = { fg = colors.purple },
          ["@type"] = { fg = colors.fg },
          ["@variable.member"] = { fg = colors.fg },
          ["@variable.parameter"] = { fg = colors.fg },
          ["@lsp.type.parameter"] = { fg = colors.fg },
          ["@lsp.type.type"] = { fg = colors.fg },
          ["@lsp.type.interface"] = { fg = colors.blue },
          ["@lsp.type.enum"] = { fg = colors.fg },
          ["@lsp.type.namespace"] = { fg = colors.fg },
          ["@lsp.type.property"] = { fg = colors.fg },
          ["@lsp.type.enumMember"] = { fg = colors.cyan },
          ["@type.builtin.typescript"] = { fg = colors.cyan },
        },
      })

      -- highlight Identifier guifg=#c678dd
      -- highlight GitGutterAdd guibg=NONE guifg=NONE
      -- highlight GitGutterChange guibg=NONE guifg=NONE
      -- highlight GitGutterDelete guibg=NONE guifg=NONE
      -- highlight Type guifg=#56b6c2
      -- highlight typescriptBlock guifg=#56b6c2
      -- highlight typescriptFuncCallArg guifg=#56b6c2
      -- highlight typescriptCall guifg=#abb2bf
      -- highlight typescriptDestructureVariable guifg=#abb2bf
      -- highlight typescriptMember guifg=#abb2bf
      -- highlight typescriptAliasDeclaration guifg=#abb2bf
      -- highlight typescriptObjectLabel guifg=#abb2bf
    end,
  },
}

local selectedTheme = themes[os.getenv("THEME") or "vscode"] or themes.vscode

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
