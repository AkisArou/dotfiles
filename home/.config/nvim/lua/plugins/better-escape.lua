require("better_escape").setup({
  timeout = vim.o.timeoutlen,
  default_mappings = false,
  mappings = {
    i = {
      j = {
        k = "<Esc>",
        j = "<Esc>",
      },
    },
    c = {
      j = {
        k = "<C-c>",
        j = "<C-c>",
      },
    },
    t = {
      j = {
        k = "<C-\\><C-n>",
      },
    },
    s = {
      j = {
        k = "<Esc>",
      },
    },
  },
})
