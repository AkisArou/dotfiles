-- Declare a global function to retrieve the current directory
---@diagnostic disable-next-line: duplicate-set-field
function _G.get_oil_winbar()
  local bufnr = vim.api.nvim_win_get_buf(vim.g.statusline_winid)
  local dir = require("oil").get_current_dir(bufnr)
  if dir then
    return vim.fn.fnamemodify(dir, ":~")
  else
    -- If there is no current directory (e.g. over ssh), just show the buffer name
    return vim.api.nvim_buf_get_name(0)
  end
end

require("oil").setup({
  skip_confirm_for_simple_edits = true,

  win_options = {
    signcolumn = "yes:2",
    winbar = "%!v:lua.get_oil_winbar()",
  },

  keymaps = {
    ["<C-e>"] = "actions.select",
    ["<C-f>"] = { "actions.parent", mode = "n" },
    ["<Leader>yp"] = "actions.copy_entry_path",
    ["<Leader>yf"] = "actions.copy_entry_filename",
  },

  -- view_options = {
  --   is_hidden_file = function(name, bufnr)
  --     local dir = require("oil").get_current_dir(bufnr)
  --     local is_dotfile = vim.startswith(name, ".") and name ~= ".."
  --     -- if no local directory (e.g. for ssh connections), just hide dotfiles
  --     if not dir then
  --       return is_dotfile
  --     end
  --     -- dotfiles are considered hidden unless tracked
  --     if is_dotfile then
  --       return not git_status[dir].tracked[name]
  --     else
  --       -- Check if file is gitignored
  --       return git_status[dir].ignored[name]
  --     end
  --   end,
  -- },

  lsp_file_methods = {
    enabled = true,
    timeout_ms = 10000,
    autosave_changes = true,
  },
})

require("oil-lsp-diagnostics").setup()
require("oil-git-status").setup()

vim.keymap.set("n", "<leader>e", "<CMD>Oil<CR>", { desc = "Open parent directory" })
