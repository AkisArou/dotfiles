return {
  "ibhagwan/fzf-lua",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    local actions = require("fzf-lua.actions")
    local fzf_lua = require("fzf-lua")
    fzf_lua.setup({
      -- "fzf-native",
      winopts = {
        border = "none",
        preview = {
          layout = "vertical",
        },
      },
      fzf_opts = { ["--cycle"] = true },
      keymap = { fzf = { ["ctrl-e"] = "accept", ["ctrl-q"] = "select-all+accept" } },
      files = {
        fd_opts = [[--color=never --type f --hidden --follow --exclude .git --exclude node_modules]],
        actions = {
          -- inherits from 'actions.files', here we can override
          -- or set bind to 'false' to disable a default action
          -- action to toggle `--no-ignore`, requires fd or rg installed
          ["ctrl-g"] = { actions.toggle_ignore },
          ["ctrl-h"] = { actions.toggle_hidden },
        },
      },
      buffers = {
        file_icons = false,
        path_shorten = 1,
      },
    })

    vim.api.nvim_set_hl(0, "FzfLuaBorder", { fg = "#1f1f1f" })

    vim.keymap.set("n", "<leader>ff", fzf_lua.files, { desc = "Fuzzy find files in cwd" })

    vim.keymap.set("n", "<leader>fr", fzf_lua.oldfiles, { desc = "Fuzzy find recent files" })

    vim.keymap.set("n", "<leader>fs", fzf_lua.live_grep_native, { desc = "Grep in cwd" })

    vim.keymap.set("n", "<leader>fl", fzf_lua.resume, { desc = "Fzf resume" })

    vim.keymap.set("n", "<leader>fw", fzf_lua.grep_cword, { desc = "Find word under cursor in cwd" })

    vim.keymap.set("n", "<leader>fW", fzf_lua.grep_cWORD, { desc = "Find WORD under cursor in cwd" })

    vim.keymap.set("n", "<leader>fb", fzf_lua.lgrep_curbuf, { desc = "Grep in currunt buffer" })

    vim.keymap.set("n", "<leader>fd", fzf_lua.diagnostics_document, { desc = "Document diagnostics" })

    vim.keymap.set("n", "<leader>fq", fzf_lua.quickfix, { desc = "Quickfix" })

    vim.keymap.set("n", "<leader>fm", fzf_lua.manpages, { desc = "Manpages" })

    vim.keymap.set("n", "<leader>gb", fzf_lua.git_branches, { desc = "Git branches" })

    vim.keymap.set("n", "<leader>gc", function()
      fzf_lua.git_commits({
        winopts = {
          preview = {
            layout = "flex",
          },
        },
      })
    end, { desc = "Git commits" })

    vim.keymap.set("n", "<leader>gf", function()
      fzf_lua.git_bcommits({
        winopts = {
          preview = {
            layout = "flex",
          },
        },
        actions = { ["ctrl-o"] = actions.git_checkout },
      })
    end, { desc = "Buffer commit history" })

    vim.keymap.set("n", "<leader>fe", function()
      fzf_lua.buffers({
        fzf_opts = {
          ["--delimiter"] = " ",
          ["--with-nth"] = "-1",
        },
        winopts = {
          height = 0.35,
          width = 0.35,
          preview = {
            hidden = "hidden",
          },
        },
      })
    end, { desc = "Buffers" })
  end,
}
