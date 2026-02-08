require("mini.cursorword").setup()

require("mini.surround").setup()

require("mini.pairs").setup({
  mappings = {
    ["<"] = { action = "open", pair = "<>", neigh_pattern = ".[%(]", register = { cr = false } },
  },
})

require("mini.ai").setup({
  custom_textobjects = {
    b = false,
  },
})

require("mini.git").setup()

vim.keymap.set({ "n", "x" }, "<leader>gmc", "<Cmd>lua MiniGit.show_at_cursor()<CR>", { desc = "Show at cursor" })
vim.keymap.set({ "n", "x" }, "<leader>gmr", "<Cmd>lua MiniGit.show_range_history()<CR>", { desc = "Show at cursor" })

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "git" },
  callback = function(event)
    vim.keymap.set("n", "<C-]>", "<Cmd>lua MiniGit.show_diff_source()<CR>", {
      buffer = event.buf,
      silent = true,
      desc = "Show diff source",
    })
  end,
})

vim.api.nvim_create_autocmd("BufWinEnter", {
  callback = function(ev)
    local buf = ev.buf
    local name = vim.api.nvim_buf_get_name(buf)

    if name:match("git show$") then
      vim.bo[buf].filetype = "gitcommit"
    end
  end,
})
