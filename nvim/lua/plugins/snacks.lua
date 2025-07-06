local util = require("custom.util")

require("snacks").setup({
  styles = {
    input = {
      backdrop = true,
      relative = "cursor",
      b = {
        completion = true, -- blink completions in input
      },
    },
  },
  input = {},
  bigfile = {
    -- your bigfile configuration comes here
    -- or leave it empty to use the default settings
    -- refer to the configuration section below
  },
})

local keys = {
  {
    "<leader>gl",
    function()
      Snacks.lazygit()
    end,
    desc = "Lazygit",
  },
  -- {
  --   "<leader>gf",
  --   function()
  --     Snacks.lazygit.log_file()
  --   end,
  --   desc = "Lazygit file",
  -- },
  {
    "<leader>bd",
    function()
      util.write_format()
      Snacks.bufdelete()
    end,
    desc = "Buffer delete current",
  },
  {
    "<leader>bo",
    function()
      Snacks.bufdelete.other()
    end,
    desc = "Buffer delete others",
  },
  {
    "<leader>ba",
    function()
      util.write_format()
      Snacks.bufdelete.all()
    end,
    desc = "Buffer delete all",
  },
}

for _, key in pairs(keys) do
  vim.keymap.set("n", key[1], key[2], { desc = key.desc })
end
