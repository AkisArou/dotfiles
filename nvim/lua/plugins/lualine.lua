local tsc = function()
  local total_errors = #require("overseer").list_tasks({ recent_first = true })[1].result.diagnostics
  return total_errors == 0 and "" or "Project errors: " .. tostring(total_errors)
end

require("lualine").setup({
  options = {
    theme = vim.g.os_theme,
    disabled_filetypes = { "man" },
    component_separators = "",
    icons_enabled = true,
  },
  sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {
      {
        "filetype",
        colored = true,
        icon_only = true,
        icon = { align = "right" },
      },
      { "filename" },
      { "diagnostics", symbols = { error = " ", warn = " ", info = " " }, update_in_insert = true },
    },
    lualine_x = {
      { tsc, icon = "", color = "DiagnosticError" },
      {
        "lsp_status",
        ignore_lsp = { "cssmodules_ls", "tailwindcss" },
      },
      "nvim-dap-ui",
      "branch",
      "diff",
      "progress",
      "location",
    },
    lualine_y = {},
    lualine_z = {},
  },
  tabline = {},
  winbar = {},
  inactive_winbar = {},
  extensions = { "nvim-dap-ui" },
})
