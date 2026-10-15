vim.api.nvim_create_autocmd("FileType", {
  desc = "Disable URL highlights",
  pattern = {
    "fzf",
    "lazyterm",
  },
  command = "call highlighturl#disable_local()",
})
