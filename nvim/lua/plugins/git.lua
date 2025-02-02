return {
  {
    "akinsho/toggleterm.nvim",
    version = "*",
    dependencies = {
      "samjwill/nvim-unception",
    },
    config = function()
      local Terminal = require("toggleterm.terminal").Terminal

      local lazygit = Terminal:new({
        cmd = "lazygit",
        dir = "git_dir",
        direction = "float",
        float_opts = {
          border = "double",
          width = 9999,
          height = 9999,
        },
        -- function to run on opening the terminal
        on_open = function(term)
          vim.cmd("startinsert!")
          vim.api.nvim_buf_set_keymap(term.bufnr, "n", "q", "<cmd>close<CR>", { noremap = true, silent = true })
        end,
        -- function to run on closing the terminal
        on_close = function()
          vim.cmd("startinsert!")
        end,
      })

      function Lazygit_toggle()
        lazygit:toggle()
      end

      vim.api.nvim_set_keymap("n", "<leader>gs", "<cmd>lua Lazygit_toggle()<CR>", { noremap = true, silent = true })

      vim.api.nvim_create_autocmd("User", {
        pattern = "UnceptionEditRequestReceived",
        callback = function()
          lazygit:close()
        end,
      })
    end,
  },
