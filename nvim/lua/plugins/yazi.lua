require("yazi").setup({
  open_for_directories = true,
  yazi_floating_window_border = "shadow",
  floating_window_scaling_factor = 1,
  future_features = {
    process_events_live = false,
  },
})

vim.keymap.set("n", "<leader>e", "<cmd>Yazi<cr>", { desc = "Open yazi at the current file" })
