local agentic = require("agentic")

agentic.setup({
  provider = "opencode-acp",
})

vim.keymap.set({ "n" }, "<leader>aa", function()
  agentic.toggle()
end, { desc = "Toggle Agentic Chat" })

vim.keymap.set({ "n", "v" }, "<leader>af", function()
  agentic.add_selection_or_file_to_context()
end, { desc = "Add file or selection to Agentic to Context" })

vim.keymap.set({ "n", "v", "i" }, "<leader>an", function()
  agentic.new_session()
end, { desc = "New Agentic Session" })

vim.keymap.set({ "n", "v", "i" }, "<leader>ar", function()
  agentic.restore_session()
end, { desc = "Agentic Restore session", silent = true })
