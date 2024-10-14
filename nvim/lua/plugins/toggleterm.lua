return {
  "akinsho/toggleterm.nvim",
  event = "VeryLazy",
  enabled = false,
  version = "*",
  opts = function()
    local Terminal = require("toggleterm.terminal").Terminal

    local plain = Terminal:new({
      hidden = true,
      direction = "float",
      on_open = function(term)
        vim.cmd("startinsert!")
        vim.api.nvim_buf_set_keymap(term.bufnr, "n", "<esc>", "<cmd>close<CR>", { noremap = true, silent = true })
      end,
      on_close = function()
        vim.cmd("startinsert!")
      end,
    })

    function TogglePlainTerm()
      plain:toggle()
    end

    vim.api.nvim_set_keymap("n", "<leader>tt", "<cmd>lua TogglePlainTerm()<CR>", { noremap = true, silent = true })

    function _G.set_terminal_keymaps()
      local opts = { buffer = 0 }
      vim.keymap.set("t", "<esc>", [[<C-\><C-n>]], opts)
    end

    -- if you only want these mappings for toggle term use term://*toggleterm#* instead
    vim.cmd("autocmd! TermOpen term://* lua set_terminal_keymaps()")
  end,
}
