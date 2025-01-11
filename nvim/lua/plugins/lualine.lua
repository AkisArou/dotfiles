return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    require("lualine").setup()
    -- ins_right({
    --   function()
    --     if TSC_ERRORS_COUNT == 0 or TSC_ERRORS_COUNT == nil then
    --       return ""
    --     end
    --
    --     return "Project errors: " .. tostring(TSC_ERRORS_COUNT)
    --   end,
    -- })
  end,
}
