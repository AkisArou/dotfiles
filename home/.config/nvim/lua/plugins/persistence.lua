require("persistence").setup()

vim.keymap.set("n", "<leader>sl", function()
  require("persistence").load()
end)
