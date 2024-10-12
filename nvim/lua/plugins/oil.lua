return {
  "stevearc/oil.nvim",
  opts = {},
  -- dependencies = { "nvim-tree/nvim-web-devicons" }, -- use if prefer nvim-web-devicons
  config = function()
    -- Declare a global function to retrieve the current directory
    function _G.get_oil_winbar()
      local dir = require("oil").get_current_dir()
      if dir then
        return vim.fn.fnamemodify(dir, ":~")
      else
        -- If there is no current directory (e.g. over ssh), just show the buffer name
        return vim.api.nvim_buf_get_name(0)
      end
    end

    require("oil").setup({
      win_options = {
        winbar = "%!v:lua.get_oil_winbar()",
      },
      columns = {},
      skip_confirm_for_simple_edits = true,
      prompt_save_on_select_new_entry = false,
      lsp_file_methods = {
        enabled = true,
        timeout_ms = 10000,
        autosave_changes = true,
      },
      keymaps = {
        ["g?"] = "actions.show_help",
        ["q"] = "actions.close",
        ["<leader>e"] = "actions.close",
        ["l"] = "actions.select",
        ["<C-s>"] = { "actions.select", opts = { vertical = true }, desc = "Open the entry in a vertical split" },
        ["<C-h>"] = { "actions.select", opts = { horizontal = true }, desc = "Open the entry in a horizontal split" },
        ["<C-t>"] = { "actions.select", opts = { tab = true }, desc = "Open the entry in new tab" },
        ["<C-p>"] = "actions.preview",
        ["<C-l>"] = "actions.refresh",
        ["h"] = "actions.parent",
        ["#"] = "actions.open_cwd",
        ["`"] = "actions.cd",
        -- ["~"] = { "actions.cd", opts = { scope = "tab" }, desc = ":tcd to the current oil directory", mode = "n" },
        ["gs"] = "actions.change_sort",
        ["gx"] = "actions.open_external",
        ["g."] = "actions.toggle_hidden",
        ["g\\"] = "actions.toggle_trash",
      },
    })

    vim.keymap.set("n", "<leader>e", "<CMD>Oil<CR>", { desc = "Open parent directory" })
  end,
}
