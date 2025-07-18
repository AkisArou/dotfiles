local tsc = function()
  local total_errors = #require("overseer").list_tasks({ recent_first = true })[1].result.diagnostics

  if total_errors == 0 then
    return ""
  end

  return "Project errors: " .. tostring(total_errors)
end

local filename = function()
  local get_fg = function(hl)
    local fg_hl = vim.api.nvim_get_hl(0, { name = hl }).fg

    if fg_hl == nil then
      return { fg = "none" }
    end

    return { fg = string.format("#%06x", fg_hl), gui = "bold" }
  end

  return {
    "filename",
    color = function()
      local diagnostics =
        vim.diagnostic.count(0, { severity = { vim.diagnostic.severity.ERROR, vim.diagnostic.severity.WARN } })

      local errors = diagnostics[vim.diagnostic.severity.ERROR] or 0
      local warnings = diagnostics[vim.diagnostic.severity.WARN] or 0

      local color = {}

      if errors > 0 then
        color = get_fg("DiagnosticError")
      elseif warnings > 0 then
        color = get_fg("DiagnosticWarning")
      end

      return color
    end,
  }
end

require("lualine").setup({
  options = {
    theme = vim.g.os_theme,
    disabled_filetypes = { "man" },
    component_separators = "",
  },
  sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {
      filename(),
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
