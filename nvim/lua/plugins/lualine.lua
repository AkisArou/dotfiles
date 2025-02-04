return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    local colors = {
      blue = "#61afef",
      green = "#98c379",
      purple = "#c678dd",
      cyan = "#56b6c2",
      red1 = "#e06c75",
      red2 = "#be5046",
      yellow = "#e5c07b",
      fg = "#abb2bf",
      bg = "#282c34",
      gray1 = "#828997",
      gray2 = "#2c323c",
      gray3 = "#3e4452",
    }

    local one_dark = {
      normal = {
        a = { fg = colors.bg, bg = colors.green, gui = "bold" },
        b = { fg = colors.fg, bg = colors.gray3 },
        c = { fg = colors.fg, bg = colors.gray2 },
      },
      command = { a = { fg = colors.bg, bg = colors.yellow, gui = "bold" } },
      insert = { a = { fg = colors.bg, bg = colors.blue, gui = "bold" } },
      visual = { a = { fg = colors.bg, bg = colors.purple, gui = "bold" } },
      terminal = { a = { fg = colors.bg, bg = colors.cyan, gui = "bold" } },
      replace = { a = { fg = colors.bg, bg = colors.red1, gui = "bold" } },
      inactive = {
        a = { fg = colors.gray1, bg = colors.bg, gui = "bold" },
        b = { fg = colors.gray1, bg = colors.bg },
        c = { fg = colors.gray1, bg = colors.gray2 },
      },
    }

    require("lualine").setup({
      options = {
        icons_enabled = true,
        theme = one_dark,
        component_separators = { left = "", right = "" },
        section_separators = { left = "", right = "" },
        disabled_filetypes = {
          statusline = {},
          winbar = {},
        },
        ignore_focus = {},
        always_divide_middle = true,
        always_show_tabline = true,
        globalstatus = false,
        refresh = {
          statusline = 100,
          tabline = 100,
          winbar = 100,
        },
      },
      sections = {
        lualine_a = { "mode" },
        lualine_b = {
          "branch",
          "diff",
          {
            "diagnostics",
            symbols = { error = " ", warn = " ", info = " " },
            update_in_insert = true,
          },
        },
        lualine_c = { "filename" },
        lualine_x = { "overseer", "nvim-dap-ui", "encoding", "fileformat", "filetype" },
        lualine_y = { "progress" },
        lualine_z = { "location" },
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = { "filename" },
        lualine_x = { "location" },
        lualine_y = {},
        lualine_z = {},
      },
      tabline = {},
      winbar = {},
      inactive_winbar = {},
      extensions = { "overseer", "nvim-dap-ui" },
    })
  end,

  -- config = function()
  --   -- Eviline config for lualine
  --   -- Author: shadmansaleh
  --   -- Credit: glepnir
  --   local lualine = require("lualine")
  --
  --   -- Color table for highlights
  --   -- stylua: ignore
  --   local colors = {
  --     bg       = '#1f1f1f',
  --     fg       = '#bbc2cf',
  --     yellow   = '#ECBE7B',
  --     cyan     = '#008080',
  --     darkblue = '#081633',
  --     green    = '#98be65',
  --     orange   = '#FF8800',
  --     violet   = '#a9a1e1',
  --     magenta  = '#c678dd',
  --     blue     = '#51afef',
  --     red      = '#ec5f67',
  --   }
  --
  --   local conditions = {
  --     buffer_not_empty = function()
  --       return vim.fn.empty(vim.fn.expand("%:t")) ~= 1
  --     end,
  --     hide_in_width = function()
  --       return vim.fn.winwidth(0) > 80
  --     end,
  --     check_git_workspace = function()
  --       local filepath = vim.fn.expand("%:p:h")
  --       local gitdir = vim.fn.finddir(".git", filepath .. ";")
  --       return gitdir and #gitdir > 0 and #gitdir < #filepath
  --     end,
  --   }
  --
  --   -- Config
  --   local config = {
  --     options = {
  --       -- Disable sections and component separators
  --       component_separators = "",
  --       section_separators = "",
  --       theme = {
  --         -- We are going to use lualine_c an lualine_x as left and
  --         -- right section. Both are highlighted by c theme .  So we
  --         -- are just setting default looks o statusline
  --         normal = {
  --           c = {
  --             fg = colors.fg,
  --             -- bg = colors.bg
  --           },
  --         },
  --         inactive = { c = { fg = colors.fg, bg = colors.bg } },
  --       },
  --     },
  --     sections = {
  --       -- these are to remove the defaults
  --       lualine_a = { "mode" },
  --       lualine_b = {},
  --       lualine_y = {},
  --       lualine_z = {},
  --       -- These will be filled later
  --       lualine_c = {},
  --       lualine_x = {},
  --     },
  --     inactive_sections = {
  --       -- these are to remove the defaults
  --       lualine_a = {},
  --       lualine_b = {},
  --       lualine_y = {},
  --       lualine_z = {},
  --       lualine_c = {},
  --       lualine_x = {},
  --     },
  --   }
  --
  --   -- Inserts a component in lualine_c at left section
  --   local function ins_left(component)
  --     table.insert(config.sections.lualine_c, component)
  --   end
  --
  --   -- Inserts a component in lualine_x at right section
  --   local function ins_right(component)
  --     table.insert(config.sections.lualine_x, component)
  --   end
  --
  --   -- ins_left({
  --   --   function()
  --   --     return "▊"
  --   --   end,
  --   --   color = { fg = colors.blue }, -- Sets highlighting of component
  --   --   padding = { left = 0, right = 1 }, -- We don't need space before this
  --   -- })
  --
  --   -- ins_left({
  --   --   -- mode component
  --   --   function()
  --   --     return ""
  --   --   end,
  --   --   color = function()
  --   --     -- auto change color according to neovims mode
  --   --     local mode_color = {
  --   --       n = colors.red,
  --   --       i = colors.green,
  --   --       v = colors.blue,
  --   --       [""] = colors.blue,
  --   --       V = colors.blue,
  --   --       c = colors.magenta,
  --   --       no = colors.red,
  --   --       s = colors.orange,
  --   --       S = colors.orange,
  --   --       [""] = colors.orange,
  --   --       ic = colors.yellow,
  --   --       R = colors.violet,
  --   --       Rv = colors.violet,
  --   --       cv = colors.red,
  --   --       ce = colors.red,
  --   --       r = colors.cyan,
  --   --       rm = colors.cyan,
  --   --       ["r?"] = colors.cyan,
  --   --       ["!"] = colors.red,
  --   --       t = colors.red,
  --   --     }
  --   --     return { fg = mode_color[vim.fn.mode()] }
  --   --   end,
  --   --   padding = { left = 1, right = 1 },
  --   -- })
  --
  --   ins_left({
  --     "filename",
  --     cond = conditions.buffer_not_empty,
  --     color = { fg = colors.magenta, gui = "bold" },
  --   })
  --
  --   ins_left({ "location" })
  --
  --   ins_left({ "progress", color = { fg = colors.fg, gui = "bold" } })
  --
  --   ins_left({
  --     "diagnostics",
  --     sources = { "nvim_diagnostic" },
  --     symbols = { error = " ", warn = " ", info = " " },
  --     diagnostics_color = {
  --       color_error = { fg = colors.red },
  --       color_warn = { fg = colors.yellow },
  --       color_info = { fg = colors.cyan },
  --     },
  --   })
  --
  --   ins_right("oversheer")
  --
  --   -- ins_right({
  --   --   function()
  --   --     if TSC_ERRORS_COUNT == 0 or TSC_ERRORS_COUNT == nil then
  --   --       return ""
  --   --     end
  --   --
  --   --     return "Project errors: " .. tostring(TSC_ERRORS_COUNT)
  --   --   end,
  --   --   color = { fg = colors.red, gui = "bold" },
  --   -- })
  --
  --   ins_right({
  --     -- Lsp server name .
  --     function()
  --       local msg = "No Active Lsp"
  --       local buf_ft = vim.api.nvim_buf_get_option(0, "filetype")
  --       local clients = vim.lsp.get_clients()
  --       if next(clients) == nil then
  --         return msg
  --       end
  --       for _, client in ipairs(clients) do
  --         local filetypes = client.config.filetypes
  --         if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
  --           return client.name
  --         end
  --       end
  --       return msg
  --     end,
  --     color = { fg = "#ffffff", gui = "bold" },
  --   })
  --
  --   -- -- Add components to right sections
  --   -- ins_right({
  --   --   "o:encoding", -- option component same as &encoding in viml
  --   --   fmt = string.upper, -- I'm not sure why it's upper case either ;)
  --   --   cond = conditions.hide_in_width,
  --   --   color = { fg = colors.green, gui = "bold" },
  --   -- })
  --   --
  --   -- ins_right({
  --   --   "fileformat",
  --   --   fmt = string.upper,
  --   --   icons_enabled = false, -- I think icons are cool but Eviline doesn't have them. sigh
  --   --   color = { fg = colors.green, gui = "bold" },
  --   -- })
  --
  --   ins_right({
  --     "branch",
  --     icon = "",
  --     color = { fg = colors.violet, gui = "bold" },
  --   })
  --
  --   ins_right({
  --     "diff",
  --     -- Is it me or the symbol for modified us really weird
  --     symbols = { added = " ", modified = "󰝤 ", removed = " " },
  --     diff_color = {
  --       added = { fg = colors.green },
  --       modified = { fg = colors.orange },
  --       removed = { fg = colors.red },
  --     },
  --     cond = conditions.hide_in_width,
  --   })
  --
  --   lualine.setup(config)
  -- end,
}
