return {
  {
    "akinsho/toggleterm.nvim",
    version = "*",
    dependencies = {
      "samjwill/nvim-unception",
    },
    config = function()
      local Terminal = require("toggleterm.terminal").Terminal

      function Make_lazygit_term(cmd)
        return Terminal:new({
          cmd = cmd,
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
      end

      local base_lazygit_cmd = vim.env.HOME .. "/dotfiles/lazygit/launch-lazygit.sh"

      local lazygit = Make_lazygit_term(base_lazygit_cmd)
      local lazygitFile = nil

      function Lazygit_toggle()
        lazygit:toggle()
      end

      function LazygitFile_toggle()
        if not lazygitFile then
          lazygitFile = Make_lazygit_term("lazygit --filter " .. vim.fn.expand("%:p"))
          lazygitFile:open()
          lazygitFile = nil
        else
          lazygitFile:close()
        end
      end

      vim.api.nvim_set_keymap("n", "<leader>gs", "<cmd>lua Lazygit_toggle()<CR>", { noremap = true, silent = true })
      vim.api.nvim_set_keymap("n", "<leader>gf", "<cmd>lua LazygitFile_toggle()<CR>", { noremap = true, silent = true })

      vim.api.nvim_create_autocmd("User", {
        pattern = "UnceptionEditRequestReceived",
        callback = function()
          lazygit:close()

          if lazygitFile then
            lazygitFile:close()
            lazygitFile = nil
          end
        end,
      })
    end,
  },
}
