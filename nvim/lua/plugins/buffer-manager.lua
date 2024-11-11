return {
  "j-morano/buffer_manager.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  config = function()
    local opts = { noremap = true }
    local map = vim.keymap.set
    -- Setup
    require("buffer_manager").setup({
      select_menu_item_commands = {
        enter = {
          key = "<C-e>",
          command = "e",
        },
        v = {
          key = "<C-v>",
          command = "vsplit",
        },
        h = {
          key = "<C-h>",
          command = "split",
        },
      },
      focus_alternate_buffer = true,
      short_file_names = true,
      short_term_names = true,
      loop_nav = true,
      highlight = "Normal:BufferManagerBorder",
      win_extra_options = {
        winhighlight = "Normal:BufferManagerNormal",
      },
    })
    -- Navigate buffers bypassing the menu
    local bmui = require("buffer_manager.ui")

    -- Just the menu
    map({ "t", "n" }, "<leader>bs", bmui.toggle_quick_menu, opts)

    map("n", "<S-h>", bmui.nav_next, opts)
    map("n", "<S-l>", bmui.nav_prev, opts)

    vim.api.nvim_command([[
      autocmd FileType buffer_manager vnoremap J :m '>+1<CR>gv=gv
      autocmd FileType buffer_manager vnoremap K :m '<-2<CR>gv=gv
    ]])

    vim.api.nvim_command([[
      autocmd! FileType buffer_manager nnoremap <buffer> <C-c> :lua require('buffer_manager.ui').toggle_quick_menu()<CR>
    ]])
  end,
}
