return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons", "arkav/lualine-lsp-progress" },
  config = function()
    local tsc = function()
      local total_errors = require("custom.tsc").total_errors

      if total_errors == 0 or total_errors == nil then
        return ""
      end

      return "Project errors: " .. tostring(total_errors)
    end

    require("lualine").setup({
      options = {
        icons_enabled = true,
        theme = vim.g.os_theme,
        disabled_filetypes = {
          "man",
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
        },
        lualine_c = {
          "filename",
          {
            "diagnostics",
            symbols = { error = " ", warn = " ", info = " " },
            update_in_insert = true,
          },
        },
        lualine_x = {
          "lsp_progress",
          { tsc, icon = "", color = "DiagnosticError" },
          "nvim-dap-ui",
          "encoding",
          "fileformat",
          "filetype",
        },
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
      extensions = { "nvim-dap-ui" },
    })
  end,
}
