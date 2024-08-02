return {
  "echasnovski/mini.bufremove",
  event = "VeryLazy",
  keys = {
    {
      "<leader>bd",
      function()
        local bd = require("mini.bufremove").delete
        vim.cmd.write()
        bd(0)
      end,
      desc = "Delete Buffer",
    },
    -- stylua: ignore
    { "<leader>bD", function()
      vim.cmd("wa")
    require("mini.bufremove").delete(0, true)
    end, desc = "Delete Buffer (Force)" },
  },
}
