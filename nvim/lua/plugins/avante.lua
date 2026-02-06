require("avante").setup({
  provider = "opencode",
  acp_providers = {
    ["opencode"] = {
      command = "opencode",
      args = { "acp" },
      timeout = 30000, -- Timeout in milliseconds
      disable_tools = true, -- disable tools!
    },
  },

  behaviour = {
    auto_set_highlight_group = false,
  },

  mappings = {
    sidebar = {
      switch_windows = "<C-j>",
      reverse_switch_windows = "<C-k>",
    },
  },
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "AvanteInput",
  callback = function(ev)
    vim.opt_local.digraph = false

    local opts = {
      buffer = ev.buf,
      noremap = true,
      silent = true,
    }

    vim.keymap.set("i", "<C-k>", "<Esc><C-w>k", opts)

    vim.keymap.set("i", "<C-h>", "<Esc><C-w>h", opts)
  end,
})
