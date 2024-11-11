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
      keymap = { fzf = { ["ctrl-e"] = "accept" } },
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
        },
      },
      buffers = {
        file_icons = false,
        path_shorten = 1,
        -- actions = false,
      },
    })

    vim.api.nvim_set_hl(0, "FzfLuaBorder", { fg = "#3f3f3f" })

    vim.keymap.set("n", "<leader>ff", function()
      fzf_lua.files({
        winopts = {
          preview = {
            hidden = "hidden",
          },
        },
      })
    end, { desc = "Fuzzy find files in cwd" })

    vim.keymap.set("n", "<leader>fr", fzf_lua.oldfiles, { desc = "Fuzzy find recent files" })

    vim.keymap.set("n", "<leader>fs", fzf_lua.live_grep_native, { desc = "Grep in cwd" })

    vim.keymap.set("n", "<leader>fl", fzf_lua.live_grep_resume, { desc = "Resume grep" })

    vim.keymap.set("n", "<leader>fw", fzf_lua.grep_cword, { desc = "Find word under cursor in cwd" })

    vim.keymap.set("n", "<leader>fW", fzf_lua.grep_cWORD, { desc = "Find WORD under cursor in cwd" })

    vim.keymap.set("n", "<leader>fb", fzf_lua.lgrep_curbuf, { desc = "Grep in currunt buffer" })

    vim.keymap.set("n", "<leader>fc", fzf_lua.git_commits, { desc = "Git commits" })

    vim.keymap.set("n", "<leader>fd", fzf_lua.diagnostics_document, { desc = "Document diagnostics" })

    vim.keymap.set("n", "<leader>fq", fzf_lua.quickfix, { desc = "Quickfix" })

    -- vim.keymap.set("n", "<leader>fe", function()
    --   fzf_lua.buffers({
    --     fzf_opts = {
    --       ["--delimiter"] = " ",
    --       ["--with-nth"] = "-1",
    --     },
    --     winopts = {
    --       height = 0.35,
    --       width = 0.35,
    --       preview = {
    --         hidden = "hidden",
    --       },
    --     },
    --   })
    -- end, { desc = "Buffers" })

    function Buffer_picker()
      fzf_lua.fzf_exec(function(fzf_cb)
        local cwd = vim.loop.cwd()

        for _, buf in ipairs(vim.api.nvim_list_bufs()) do
          if vim.api.nvim_buf_is_loaded(buf) and vim.api.nvim_buf_get_option(buf, "buflisted") then
            local buf_name = vim.api.nvim_buf_get_name(buf)
            local filename = vim.fn.fnamemodify(buf_name, ":t") -- Get filename without path
            local full_path = vim.fn.fnamemodify(buf_name, ":p") -- Get full path

            -- Make the full path relative to the cwd
            if full_path:sub(1, #cwd) == cwd then
              full_path = full_path:sub(#cwd + 2)
            end
            local display_line = string.format(
              "%s %s %d",
              filename,
              require("fzf-lua").utils.ansi_codes.grey("(" .. full_path .. ")"),
              buf
            )

            -- Send formatted line to fzf
            fzf_cb(display_line)
          end
        end
        -- Close fzf pipe to signal end of data
        fzf_cb()
      end, {
        prompt = "Buffers> ",
        winopts = {
          height = 0.25,
          width = 0.35,
          preview = {
            hidden = "hidden",
          },
        },
        actions = {
          -- Default action: switch to the selected buffer
          ["default"] = function(selected)
            local buf_id = tonumber(selected[1]:match("(%d+)%s*$"))

            if buf_id then
              vim.cmd("buffer " .. buf_id)
            end
          end,

          -- Ctrl+x: Delete selected buffer and reload the list
          ["ctrl-x"] = {
            function(selected)
              local buf_id = tonumber(selected[1]:match("(%d+)%s*$"))

              if buf_id then
                pcall(vim.api.nvim_buf_delete, buf_id, { force = true })
              end
            end,

            require("fzf-lua").actions.resume,
          },
        },
      })
    end

    vim.keymap.set("n", "<leader>fe", ":lua Buffer_picker()<CR>", { noremap = true, silent = true, desc = "Buffers" })
  end,
}
