return {
  "ibhagwan/fzf-lua",
  -- optional for icon support
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    local fzf_lua = require("fzf-lua")
    fzf_lua.setup({
      fzf_opts = { ["--cycle"] = true },
    })

    vim.keymap.set("n", "<leader>ff", function()
      fzf_lua.files({
        cmd = "fd --type f --exclude node_modules",
        winopts = {
          preview = {
            layout = "vertical",
          },
        },
      })
    end, { desc = "Fuzzy find files in cwd" })
    vim.keymap.set("n", "<leader>fr", fzf_lua.oldfiles, { desc = "Fuzzy find recent files" })
    vim.keymap.set("n", "<leader>fs", fzf_lua.live_grep, { desc = "Grep in cwd" })
    vim.keymap.set("n", "<leader>fl", fzf_lua.live_grep_resume, { desc = "Resume grep" })
    vim.keymap.set("n", "<leader>fg", fzf_lua.grep_cword, { desc = "Find string under cursor in cwd" })
    vim.keymap.set("n", "<leader>fc", fzf_lua.git_commits, { desc = "Git commits" })
    vim.keymap.set("n", "<leader>fd", fzf_lua.diagnostics_document, { desc = "Document diagnostics" })
    vim.keymap.set("n", "<leader>fq", fzf_lua.quickfix, { desc = "Quickfix" })
    vim.keymap.set("n", "<leader>df", fzf_lua.dap_configurations, { desc = "Configurations" })
  end,
}
