local keys = {
  { "<c-h>", "<cmd>TmuxNavigateLeft<cr>" },
  -- { "<c-j>", "<cmd>TmuxNavigateDown<cr>" },
  -- { "<c-k>", "<cmd>TmuxNavigateUp<cr>" },
  { "<c-l>", "<cmd>TmuxNavigateRight<cr>" },
  { "<c-\\>", "<cmd>TmuxNavigatePrevious<cr>" },
}

for _, key in pairs(keys) do
  vim.keymap.set("n", key[1], key[2])
end
