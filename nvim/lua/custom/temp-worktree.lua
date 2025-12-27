-- GitView temporary worktree manager
local gitview_state = {}

-- Safely cleanup temporary worktree
local function cleanup_temp_worktree()
  if gitview_state.tmp and gitview_state.orig_cwd then
    vim.notify("Cleaning up temporary worktree: " .. gitview_state.tmp)
    vim.fn.system({ "git", "worktree", "remove", "--force", gitview_state.tmp })
    vim.fn.delete(gitview_state.tmp, "rf")
    gitview_state.tmp = nil
    gitview_state.orig_cwd = nil
  end
end

-- Open a temporary Git worktree and switch cwd
local function open_git_worktree(ref)
  ref = ref or "HEAD"

  -- store original cwd
  local orig_cwd = vim.fn.getcwd()
  gitview_state.orig_cwd = orig_cwd

  -- create temp dir
  local tmp = vim.fn.systemlist("mktemp -d")[1]
  if not tmp or tmp == "" then
    vim.notify("Failed to create temp dir", vim.log.levels.ERROR)
    return
  end
  gitview_state.tmp = tmp

  -- add detached worktree
  local add = vim.fn.system({ "git", "worktree", "add", "--detach", tmp, ref })
  if vim.v.shell_error ~= 0 then
    vim.notify(add, vim.log.levels.ERROR)
    return
  end

  -- switch cwd to worktree
  vim.cmd.cd(tmp)

  -- open worktree in current buffer
  vim.cmd.edit(tmp)

  -- failsafe cleanup on exit
  vim.api.nvim_create_autocmd("VimLeavePre", {
    once = true,
    callback = function()
      if gitview_state.tmp and gitview_state.orig_cwd then
        vim.cmd.cd(gitview_state.orig_cwd)
        cleanup_temp_worktree()
      end
    end,
  })
end

-- User command to open temp worktree
vim.api.nvim_create_user_command("GitView", function(opts)
  open_git_worktree(opts.args ~= "" and opts.args or "HEAD")
end, { nargs = "?" })

-- User command to restore cwd and remove temp worktree immediately
vim.api.nvim_create_user_command("GitViewRestore", function()
  if gitview_state.tmp and gitview_state.orig_cwd then
    vim.cmd.cd(gitview_state.orig_cwd)
    cleanup_temp_worktree()
  else
    vim.notify("No temporary worktree to restore/remove", vim.log.levels.WARN)
  end
end, {})

-- Function to copy selected Oil file to real cwd
local function copy_to_real_cwd()
  local oil = require("oil")
  local entry = oil.get_cursor_entry()
  local dir = oil.get_current_dir()

  if not entry or not dir then
    vim.notify("No entry under cursor in Oil", vim.log.levels.WARN)
    return
  end

  local oil_path = dir .. entry.name

  if not gitview_state.orig_cwd or not gitview_state.tmp then
    vim.notify("Original cwd not set!", vim.log.levels.ERROR)
    return
  end

  -- Determine relative path in temp worktree
  local rel_path = oil_path:gsub("^" .. gitview_state.tmp .. "/", "")
  local target = gitview_state.orig_cwd .. "/" .. rel_path

  -- Ensure target directory exists
  vim.fn.mkdir(vim.fn.fnamemodify(target, ":h"), "p")

  -- Copy the file or directory
  local ok = vim.fn.system(string.format("cp -r %q %q", oil_path, target))
  if vim.v.shell_error == 0 then
    vim.notify("Copied " .. rel_path .. " → real cwd")
  else
    vim.notify("Copy failed: " .. ok, vim.log.levels.ERROR)
  end
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = "oil",
  callback = function()
    vim.keymap.set("n", "<leader>r", copy_to_real_cwd, {
      buffer = 0,
      noremap = true,
      silent = true,
    })
  end,
})

-- Keymap for manual restore
vim.api.nvim_set_keymap("n", "<leader>gvr", ":GitViewRestore<CR>", { noremap = true, silent = true })

return {
  open_git_worktree = open_git_worktree,
}
