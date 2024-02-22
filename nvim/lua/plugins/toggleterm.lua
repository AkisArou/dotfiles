return {
  "akinsho/toggleterm.nvim",
  version = "*",
  opts = function()
    local Terminal = require("toggleterm.terminal").Terminal

    local gitui = Terminal:new({ cmd = "gitui", hidden = true, direction = "float" })

    function GitUIToggle()
      gitui:toggle()
    end

    local tod0 = Terminal:new({ cmd = "tod0", hidden = true, direction = "float" })

    function Tod0Toggle()
      tod0:toggle()
    end

    local plain = Terminal:new({ hidden = true, direction = "float" })

    function TogglePlainTerm()
      plain:toggle()
    end

    vim.api.nvim_set_keymap("n", "<leader>gg", "<cmd>lua _gitui_toggle()<CR>", { noremap = true, silent = true })
    vim.api.nvim_set_keymap("n", "<leader>td", "<cmd>lua _tod0_toggle()<CR>", { noremap = true, silent = true })
    vim.api.nvim_set_keymap("n", "<leader>tt", "<cmd>lua _plain_term()<CR>", { noremap = true, silent = true })
  end,
}
