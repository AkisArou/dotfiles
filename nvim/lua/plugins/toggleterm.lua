return {
  "akinsho/toggleterm.nvim",
  event = "VeryLazy",
  version = "*",
  opts = function()
    local Terminal = require("toggleterm.terminal").Terminal

    local tod0 = Terminal:new({ cmd = "tod0", hidden = true, direction = "float" })

    function Tod0Toggle()
      tod0:toggle()
    end

    local plain = Terminal:new({ hidden = true, direction = "float" })

    function TogglePlainTerm()
      plain:toggle()
    end

    vim.api.nvim_set_keymap("n", "<leader>td", "<cmd>lua Tod0Toggle()<CR>", { noremap = true, silent = true })
    vim.api.nvim_set_keymap("n", "<leader>tt", "<cmd>lua TogglePlainTerm()<CR>", { noremap = true, silent = true })
    vim.api.nvim_set_keymap("t", "<Esc><Esc>", "<cmd>lua TogglePlainTerm()<CR>", { desc = "Exit terminal mode" })
  end,
}
