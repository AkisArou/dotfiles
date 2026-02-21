local created_groups = {}

local function augroup(name)
  local full_name = "akisarou_" .. name
  if not created_groups[full_name] then
    created_groups[full_name] = vim.api.nvim_create_augroup(full_name, { clear = true })
  end
  return created_groups[full_name]
end

-- Open help in vertical split
vim.api.nvim_create_autocmd("FileType", {
  pattern = "help",
  command = "wincmd L",
})

-- Autoresize splits when terminal emulator window is resized
vim.api.nvim_create_autocmd("VimResized", {
  command = "wincmd =",
})

-- Enable treesitter
vim.api.nvim_create_autocmd("FileType", {
  callback = function()
    pcall(vim.treesitter.start)
    vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end,
})

-- Highlight on yank
vim.api.nvim_create_autocmd("TextYankPost", {
  desc = "Highlight when yanking (copying) text",
  group = augroup("highlight_yank"),
  callback = function()
    vim.hl.on_yank()
  end,
})

-- Check if we need to reload the file when it changed
vim.api.nvim_create_autocmd({ "FocusGained", "TermClose", "TermLeave" }, {
  group = augroup("checktime"),
  callback = function()
    if vim.o.buftype ~= "nofile" then
      vim.cmd("checktime")
    end
  end,
})

-- resize splits if window got resized
vim.api.nvim_create_autocmd({ "VimResized" }, {
  group = augroup("resize_splits"),
  callback = function()
    local current_tab = vim.fn.tabpagenr()
    vim.cmd("tabdo wincmd =")
    vim.cmd("tabnext " .. current_tab)
  end,
})

-- close some filetypes with <q>
vim.api.nvim_create_autocmd("FileType", {
  group = augroup("close_with_q"),
  pattern = {
    "PlenaryTestPopup",
    "checkhealth",
    "dbout",
    "gitsigns-blame",
    "grug-far",
    "help",
    "lspinfo",
    "neotest-output",
    "neotest-output-panel",
    "neotest-summary",
    "notify",
    "qf",
    "spectre_panel",
    "startuptime",
    "tsplayground",
    "nvim-undotree",
  },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.schedule(function()
      vim.keymap.set("n", "q", function()
        vim.cmd("close")
        pcall(vim.api.nvim_buf_delete, event.buf, { force = true })
      end, {
        buffer = event.buf,
        silent = true,
        desc = "Quit buffer",
      })
    end)
  end,
})

-- make it easier to close man-files when opened inline
vim.api.nvim_create_autocmd("FileType", {
  group = augroup("man_unlisted"),
  pattern = { "man" },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
  end,
})

-- check for spell in text filetypes
vim.api.nvim_create_autocmd("FileType", {
  group = augroup("spell"),
  pattern = { "text", "plaintex", "typst", "gitcommit", "markdown" },
  callback = function()
    -- Skip lsp doc window
    if vim.bo.buftype == "nofile" then
      return
    end

    vim.opt_local.spell = true
  end,
})

-- wrapping and better opts for filetypes
vim.api.nvim_create_autocmd("FileType", {
  group = augroup("wrap"),
  pattern = { "gitcommit", "markdown", "opencode_output" },
  callback = function()
    vim.opt_local.wrap = true

    vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", {
      buffer = 0,
      expr = true,
      silent = true,
    })

    vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", {
      buffer = 0,
      expr = true,
      silent = true,
    })
  end,
})

-- Fix conceallevel for json files
vim.api.nvim_create_autocmd({ "FileType" }, {
  group = augroup("json_conceal"),
  pattern = { "json", "jsonc", "json5" },
  callback = function()
    vim.opt_local.conceallevel = 0
  end,
})

-- Auto create dir when saving a file, in case some intermediate directory does not exist
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
  group = augroup("auto_create_dir"),
  callback = function(event)
    if event.match:match("^%w%w+:[\\/][\\/]") then
      return
    end
    local file = vim.uv.fs_realpath(event.match) or event.match
    vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
  end,
})

-- Autosave (only for existing files that have been modified)
vim.api.nvim_create_autocmd({ "BufLeave", "FocusLost" }, {
  group = augroup("autowrite"),
  pattern = "*",
  callback = function()
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_is_valid(buf) and vim.bo[buf].modified and vim.api.nvim_buf_get_name(buf) ~= "" then
        vim.api.nvim_buf_call(buf, function()
          vim.cmd("silent! write")
        end)
      end
    end
  end,
})

-- Restore cursor position on file open
vim.api.nvim_create_autocmd("BufReadPost", {
  group = augroup("restore_cursor"),
  pattern = "*",
  callback = function()
    local line = vim.fn.line("'\"")
    if
      line > 1
      and line <= vim.fn.line("$")
      and vim.bo.filetype ~= "commit"
      and vim.fn.index({ "xxd", "gitrebase" }, vim.bo.filetype) == -1
    then
      vim.cmd('normal! g`"')
      vim.cmd("normal! zz")
    end
  end,
})

-- Disable new line auto-comment
vim.api.nvim_create_autocmd("FileType", {
  group = augroup("auto-comment"),
  pattern = "*",
  callback = function()
    vim.cmd("set formatoptions-=cro")
    vim.cmd("setlocal formatoptions-=cro")
  end,
})

-- Terminal
vim.api.nvim_create_autocmd("TermOpen", {
  group = augroup("terminal-open"),
  callback = function()
    vim.opt.number = false
    vim.opt.relativenumber = false
    vim.cmd.startinsert()
  end,
})

-- Customize the built-in undotree window
vim.api.nvim_create_autocmd("FileType", {
  pattern = "nvim-undotree",
  callback = function()
    vim.cmd.wincmd("H")
    vim.api.nvim_win_set_width(0, 40)
  end,
})

vim.api.nvim_create_autocmd("User", {
  pattern = "OilActionsPost",
  group = augroup("oil-actions-post"),
  callback = function(e)
    if e.data.actions == nil then
      return
    end

    for _, action in ipairs(e.data.actions) do
      if action.entry_type == "file" and action.type == "delete" then
        local _, path = require("oil.util").parse_url(action.url)
        local bufnr = vim.fn.bufnr(path)

        if bufnr >= 0 then
          vim.bo[bufnr].buflisted = false
          vim.cmd.bwipeout({ bufnr, bang = true })
        end
      end
    end
  end,
})
