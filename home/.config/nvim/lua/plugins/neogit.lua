local neogit = require("neogit")
neogit.setup({
  prompt_force_push = false,
  integrations = {
    fzf_lua = true,
    codediff = true,
  },
  ignored_settings = {
    "NeogitPushPopup--force-with-lease",
    "NeogitPushPopup--force",
    "NeogitPullPopup--rebase",
    "NeogitCommitPopup--allow-empty",
  },
})

vim.keymap.set("n", "<leader>gs", vim.cmd.Neogit)
