return {
  "ibhagwan/fzf-lua",
  -- optional for icon support
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    local actions = require("fzf-lua.actions")
    local fzf_lua = require("fzf-lua")
    fzf_lua.setup({
      -- "fzf-native",
      winopts = {
        preview = {
          layout = "vertical",
        },
      },
      fzf_opts = { ["--cycle"] = true },
      previewers = {
        bat = { cmd = "bat --theme 'Visual Studio Dark+'" },
      },
      files = {
        -- VSCode like file search
        -- formatter = "path.filename_first",
        fd_opts = [[--color=never --type f --hidden --follow --exclude .git --exclude node_modules]],
        actions = {
          -- inherits from 'actions.files', here we can override
          -- or set bind to 'false' to disable a default action
          -- action to toggle `--no-ignore`, requires fd or rg installed
          ["ctrl-g"] = { actions.toggle_ignore },
          ["ctrl-h"] = { actions.toggle_hidden },
          -- Bind Ctrl+e to open the selected file in a normal buffer
          ["ctrl-e"] = function(selected)
            -- Ensure a selection was made
            if selected and #selected > 0 then
              -- Get the selected file
              local file = selected[1]

              -- Remove letters before the first emoji and the emoji itself
              local cleaned_filename = file:gsub(".*[\194-\244][\128-\191]*", "") -- Remove everything up to and including the first emoji

              -- Trim any leading or trailing spaces
              cleaned_filename = cleaned_filename:match("^%s*(.-)%s*$") -- Trim spaces

              -- Open the selected file in a normal buffer if the cleaned filename is valid
              if cleaned_filename and #cleaned_filename > 0 then
                vim.cmd("edit " .. vim.fn.fnameescape(cleaned_filename))
              end
            end
          end,
        },
      },
    })

    vim.api.nvim_set_hl(0, "FzfLuaBorder", { fg = "#3f3f3f" })

    vim.keymap.set("n", "<leader>ff", fzf_lua.files, { desc = "Fuzzy find files in cwd" })
    vim.keymap.set("n", "<leader>fr", fzf_lua.oldfiles, { desc = "Fuzzy find recent files" })
    vim.keymap.set("n", "<leader>fs", fzf_lua.live_grep_native, { desc = "Grep in cwd" })
    vim.keymap.set("n", "<leader>fl", fzf_lua.live_grep_resume, { desc = "Resume grep" })
    vim.keymap.set("n", "<leader>fw", fzf_lua.grep_cword, { desc = "Find word under cursor in cwd" })
    vim.keymap.set("n", "<leader>fW", fzf_lua.grep_cWORD, { desc = "Find WORD under cursor in cwd" })
    -- vim.keymap.set("n", "<leader>fb", fzf_lua.lgrep_curbuf, { desc = "Grep in currunt buffer" })
    vim.keymap.set("n", "<leader>fe", fzf_lua.buffers, { desc = "Buffers" })
    vim.keymap.set("n", "<leader>fc", fzf_lua.git_commits, { desc = "Git commits" })
    vim.keymap.set("n", "<leader>fd", fzf_lua.diagnostics_document, { desc = "Document diagnostics" })
    vim.keymap.set("n", "<leader>fq", fzf_lua.quickfix, { desc = "Quickfix" })
  end,
}
