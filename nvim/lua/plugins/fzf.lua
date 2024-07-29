return {
  "ibhagwan/fzf-lua",
  -- optional for icon support
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    local fzf_lua = require("fzf-lua")
    fzf_lua.setup({})

    vim.keymap.set("n", "<leader>ff", fzf_lua.files, { desc = "Fuzzy find files in cwd" })
    vim.keymap.set("n", "<leader>fr", fzf_lua.oldfiles, { desc = "Fuzzy find recent files" })
    vim.keymap.set("n", "<leader>fs", fzf_lua.live_grep, { desc = "Find string in cwd" })
    vim.keymap.set("n", "<leader>fg", fzf_lua.grep_cword, { desc = "Find string under cursor in cwd" })
    vim.keymap.set("n", "<leader>fc", fzf_lua.git_commits, { desc = "Git commits" })
    vim.keymap.set("n", "<leader>fd", fzf_lua.diagnostics_document, { desc = "Document diagnostics" })
    vim.keymap.set("n", "<leader>fq", fzf_lua.quickfix, { desc = "Quickfix" })
  end,
}
