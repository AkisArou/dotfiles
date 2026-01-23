local util = require("custom.util")

require("snacks").setup({
  styles = {
    input = {
      width = 40,
      border = false,
      backdrop = true,
      relative = "cursor",
      b = {
        completion = true,
      },
      keys = {
        n_ctrlc = { "<C-c>", { "cancel" }, mode = "n", expr = true },
      },
    },
  },
  input = {
    icon_pos = false,
    prompt_pos = "left",
  },
  bigfile = {},
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
      util.write_format_all()
      Snacks.bufdelete.all()
    end,
    desc = "Buffer delete all",
  },
}

for _, key in pairs(keys) do
  vim.keymap.set("n", key[1], key[2], { desc = key.desc })
end
