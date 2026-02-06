require("sidekick").setup({
  {
    cli = {
      mux = {
        backend = "tmux",
        enabled = true,
      },
    },
  },
})

vim.keymap.set({ "n", "v" }, "<leader>aa", ":Sidekick cli toggle name=opencode<CR>")

vim.keymap.set({ "x", "n" }, "<leader>at", function()
  require("sidekick.cli").send({ msg = "{this}" })
end, { desc = "send this" })

vim.keymap.set("n", "<leader>af", function()
  require("sidekick.cli").send({ msg = "{file}" })
end, { desc = "send file" })

vim.keymap.set("x", "<leader>av", function()
  require("sidekick.cli").send({ msg = "{selection}" })
end, { desc = "send visual selection" })

vim.keymap.set({ "n", "x" }, "<leader>ap", function()
  require("sidekick.cli").prompt()
end, { desc = "sidekick select prompt" })
