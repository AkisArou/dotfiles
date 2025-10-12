local neogit = require("neogit")
neogit.setup({
  integrations = {
    -- If enabled, use telescope for menu selection rather than vim.ui.select.
    -- Allows multi-select and some things that vim.ui.select doesn't.
    telescope = false,
    -- Neogit only provides inline diffs. If you want a more traditional way to look at diffs, you can use `diffview`.
    -- The diffview integration enables the diff popup.
    --
    -- Requires you to have `sindrets/diffview.nvim` installed.
    diffview = false,

    -- If enabled, uses fzf-lua for menu selection. If the telescope integration
    -- is also selected then telescope is used instead
    -- Requires you to have `ibhagwan/fzf-lua` installed.
    fzf_lua = true,
  },
  ignored_settings = {
    "NeogitPushPopup--force-with-lease",
    "NeogitPushPopup--force",
    "NeogitPullPopup--rebase",
    "NeogitCommitPopup--allow-empty",
  },
})

vim.keymap.set("n", "<leader>gs", vim.cmd.Neogit)
