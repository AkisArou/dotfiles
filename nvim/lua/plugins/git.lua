return {
    "kdheepak/lazygit.nvim",
    lazy = true,
    cmd = {
        "LazyGit",
        "LazyGitCurrentFile",
        "LazyGitFilter",
        "LazyGitFilterCurrentFile",
    },
    -- optional for floating window border decoration
    dependencies = {
        "nvim-lua/plenary.nvim",
    },
    -- setting the keybinding for LazyGit with 'keys' is recommended in
    -- order to load the plugin when the command is run for the first time
    keys = {
        { "<leader>gs", "<cmd>LazyGit<cr>", desc = "LazyGit" }
    }
}

-- return {
--   "NeogitOrg/neogit",
--   -- branch = "nightly",
--   event = "VeryLazy",
--   dependencies = {
--     "nvim-lua/plenary.nvim", -- required
--     "sindrets/diffview.nvim", -- optional - Diff integration
--     "ibhagwan/fzf-lua", -- optional
--   },
--   config = function()
--     local neogit = require("neogit")
--     neogit.setup({
--       integrations = {
--         -- If enabled, use telescope for menu selection rather than vim.ui.select.
--         -- Allows multi-select and some things that vim.ui.select doesn't.
--         telescope = false,
--         -- Neogit only provides inline diffs. If you want a more traditional way to look at diffs, you can use `diffview`.
--         -- The diffview integration enables the diff popup.
--         --
--         -- Requires you to have `sindrets/diffview.nvim` installed.
--         diffview = true,

--         -- If enabled, uses fzf-lua for menu selection. If the telescope integration
--         -- is also selected then telescope is used instead
--         -- Requires you to have `ibhagwan/fzf-lua` installed.
--         fzf_lua = true,
--       },
--       ignored_settings = {
--         "NeogitPushPopup--force-with-lease",
--         "NeogitPushPopup--force",
--         "NeogitPullPopup--rebase",
--         "NeogitCommitPopup--allow-empty",
--       },
--     })

--     vim.keymap.set("n", "<leader>gs", vim.cmd.Neogit)

--     vim.keymap.set("n", "<leader>gdf", function()
--       vim.cmd("DiffviewFileHistory %")
--     end, { desc = "DiffviewFileHistory current file" })

--     vim.keymap.set("n", "<leader>gdo", vim.cmd.DiffviewOpen, { desc = "DiffviewOpen" })
--     vim.keymap.set("n", "<leader>gdc", vim.cmd.DiffviewClose, { desc = "DiffviewClose" })
--   end,
-- }
