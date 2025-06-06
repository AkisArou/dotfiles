return {
  "gbprod/yanky.nvim",
  lazy = true,
  opts = {
    highlight = {
      on_put = false,
      on_yank = true,
      timer = 150,
    },
  },
  keys = {
    {
      "<leader>p",
      function()
        vim.cmd([[YankyRingHistory]])
      end,
      mode = { "n", "x" },
      desc = "Open Yank History",
    },
    { "p", "<Plug>(YankyPutAfter)", mode = { "n", "x" }, desc = "Put yanked text after cursor" },
    { "P", "<Plug>(YankyPutBefore)", mode = { "n", "x" }, desc = "Put yanked text before cursor" },
    { "=p", "<Plug>(YankyPutAfterLinewise)", desc = "Put yanked text in line below" },
    { "=P", "<Plug>(YankyPutBeforeLinewise)", desc = "Put yanked text in line above" },
    { "[y", "<Plug>(YankyCycleForward)", desc = "Cycle forward through yank history" },
    { "]y", "<Plug>(YankyCycleBackward)", desc = "Cycle backward through yank history" },
    { "y", "<Plug>(YankyYank)", mode = { "n", "x" }, desc = "Yanky yank" },
  },
}
