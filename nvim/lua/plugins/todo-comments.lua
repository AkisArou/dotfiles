require("todo-comments").setup()

local keys = {
  {
    "]t",
    function()
      require("todo-comments").jump_next()
    end,
    desc = "Next todo comment",
  },
  {
    "[t",
    function()
      require("todo-comments").jump_prev()
    end,
    desc = "Previous todo comment",
  },
  {
    "<leader>fx",
    "<cmd>:lua require('fzf-lua').grep({search='TODO|PERF|NOTE|FIX:', no_esc=true})<cr>",
    desc = "Todo/Fix/Fixme (fzf-lua)",
  },
}

for _, key in pairs(keys) do
  vim.keymap.set("n", key[1], key[2], { desc = key.desc })
end
