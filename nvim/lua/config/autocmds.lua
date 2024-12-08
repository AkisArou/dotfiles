vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = vim.api.nvim_create_augroup("highlight-yank", { clear = true }),
  callback = function()
    vim.highlight.on_yank({ higroup = "Visual", timeout = 200 })
  end,
})

-- close some filetypes with <q>
vim.api.nvim_create_autocmd("FileType", {
  pattern = {
    "PlenaryTestPopup",
    "help",
    "lspinfo",
    "man",
    "notify",
    "qf",
    "query",
    "spectre_panel",
    "startuptime",
    "tsplayground",
    "neotest-output",
    "checkhealth",
    "neotest-summary",
    "neotest-output-panel",
    "toggleterm",
    "neo-tree",
    "gitsigns-blame",
    "AvanteAsk",
    "AvanteInput",
    "markdown",
  },
  callback = function(event)
    local bo = vim.bo[event.buf]
    if bo.filetype ~= "markdown" or bo.buftype == "help" then
      bo.buflisted = false
      vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = event.buf, silent = true })
    end
  end,
})

function Close_empty_unnamed_buffers()
  local buffers = vim.api.nvim_list_bufs()

  for _, bufnr in ipairs(buffers) do
    if vim.api.nvim_buf_is_loaded(bufnr) and vim.api.nvim_buf_get_name(bufnr) == "" then
      vim.api.nvim_buf_delete(bufnr, {
        force = true,
      })
    end
  end
end

vim.api.nvim_command("autocmd BufReadPost * lua Close_empty_unnamed_buffers()")
