local actions = require("fzf-lua.actions")
local fzf_lua = require("fzf-lua")

local img_previewer = { "ueberzugpp" }

fzf_lua.setup({
  "hide",
  winopts = {
    border = "none",
    preview = {
      layout = "vertical",
    },
    -- Make float bottom
    -- row = 1,
    -- col = 0,
    -- height = 0.65,
    -- width = 1,
  },
  previewers = {
    builtin = {
      extensions = {
        ["png"] = img_previewer,
        ["jpg"] = img_previewer,
        ["jpeg"] = img_previewer,
        ["gif"] = img_previewer,
      },
      ueberzug_scaler = "fit_contain",
    },
  },
  fzf_opts = { ["--cycle"] = true },
  grep = {
    RIPGREP_CONFIG_PATH = vim.env.RIPGREP_CONFIG_PATH,
  },
  keymap = {
    fzf = { ["ctrl-q"] = "select-all+accept" },
    builtin = {
      ["<C-c>"] = "hide", -- hide fzf-lua, `:FzfLua resume` to continue
    },
  },
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
  lsp = {
    async_or_timeout = 10000,
  },
})

fzf_lua.register_ui_select()

vim.keymap.set("n", "<leader>fa", fzf_lua.builtin, { desc = "Builtin" })

vim.keymap.set("n", "<leader>ff", fzf_lua.files, { desc = "Fuzzy find files in cwd" })

vim.keymap.set("n", "<leader>fr", fzf_lua.oldfiles, { desc = "Fuzzy find recent files" })

vim.keymap.set("n", "<leader>fs", fzf_lua.live_grep_native, { desc = "Grep in cwd" })

vim.keymap.set("n", "<leader>fl", fzf_lua.resume, { desc = "Fzf resume" })

vim.keymap.set("n", "<leader>fw", fzf_lua.grep_cword, { desc = "Find word under cursor in cwd" })

vim.keymap.set("n", "<leader>fW", fzf_lua.grep_cWORD, { desc = "Find WORD under cursor in cwd" })

vim.keymap.set("v", "<leader>fw", fzf_lua.grep_visual, { desc = "Find visual selection in cwd" })

vim.keymap.set("n", "<leader>fb", fzf_lua.lgrep_curbuf, { desc = "Grep in currunt buffer" })

vim.keymap.set("n", "<leader>fd", fzf_lua.diagnostics_document, { desc = "Document diagnostics" })

vim.keymap.set("n", "<leader>fq", fzf_lua.quickfix, { desc = "Quickfix" })

vim.keymap.set("n", "<leader>fm", fzf_lua.manpages, { desc = "Manpages" })

vim.keymap.set("n", "<leader>gb", fzf_lua.git_branches, { desc = "Git branches" })

vim.keymap.set("n", "<leader>ft", fzf_lua.treesitter, { desc = "Treesitter" })

local get_commit_hash = function(selected)
  -- Defensive check
  if not selected or selected == false then
    return
  end

  -- Normalize to string
  local line
  if type(selected) == "table" and #selected > 0 then
    line = selected[1]
  elseif type(selected) == "string" then
    line = selected
  else
    -- Unknown type, ignore
    return
  end

  -- Extract commit hash
  local commit_hash = line:match("^(%w+)")

  if commit_hash == nil then
    vim.notify("Failed to extract commit hash from line", vim.log.levels.WARN)
  end

  return commit_hash
end

vim.keymap.set("n", "<leader>gc", function()
  fzf_lua.git_commits({
    winopts = { preview = { layout = "flex" } },
    actions = {
      ["ctrl-w"] = function(selected)
        local commit_hash = get_commit_hash(selected)

        if commit_hash == nil then
          return
        end

        local success, err = pcall(function()
          require("custom.temp-worktree").open_git_worktree(commit_hash)
        end)
        if not success then
          vim.notify("Failed to open worktree: " .. tostring(err), vim.log.levels.ERROR)
        end
      end,
    },
  })
end, { desc = "Git commits → GitView" })

vim.keymap.set("n", "<leader>gf", function()
  fzf_lua.git_bcommits({
    winopts = {
      preview = {
        layout = "flex",
      },
    },
    actions = {
      ["ctrl-o"] = actions.git_checkout,
      ["ctrl-g"] = function(selected)
        local commit_hash = get_commit_hash(selected)

        if commit_hash == nil then
          return
        end

        vim.cmd("CodeDiff file HEAD " .. commit_hash)
      end,
    },
  })
end, { desc = "Buffer commit history" })

vim.keymap.set("n", "<leader>fe", function()
  fzf_lua.buffers({
    file_icons = false,
    path_shorten = 1,
    color_icons = false,
    fzf_opts = {
      ["--delimiter"] = " ",
      ["--with-nth"] = "-1",
    },
    winopts = {
      -- height = 0.35,
      -- width = 0.35,
      preview = {
        hidden = "hidden",
      },
    },
  })
end, { desc = "Buffers" })
