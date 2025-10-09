local created_groups = {}

local function augroup(name)
  local full_name = "akisarou_" .. name
  if not created_groups[full_name] then
    created_groups[full_name] = vim.api.nvim_create_augroup(full_name, { clear = true })
  end
  return created_groups[full_name]
end

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

-- wrap and check for spell in text filetypes
vim.api.nvim_create_autocmd("FileType", {
  group = augroup("wrap_spell"),
  pattern = { "text", "plaintex", "typst", "gitcommit", "markdown" },
  callback = function()
    -- Skip lsp doc window
    if vim.bo.buftype == "nofile" then
      return
    end

    vim.opt_local.wrap = true
    vim.opt_local.spell = true
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

-- Clear unnamed buffers
-- vim.api.nvim_create_autocmd("BufReadPost", {
--   group = augroup("clear_unnamed_bufs"),
--   pattern = "*",
--   callback = function()
--     local buffers = vim.api.nvim_list_bufs()
--
--     for _, bufnr in ipairs(buffers) do
--       if
--         vim.api.nvim_buf_is_loaded(bufnr)
--         and not vim.api.nvim_get_option_value("modified", { buf = bufnr })
--         and vim.api.nvim_buf_get_name(bufnr) == ""
--         and vim.bo.buftype ~= "quickfix"
--         and vim.bo.buftype ~= "gitcommit"
--         and vim.bo.buftype:find("^Neogit") == nil
--       then
--         vim.api.nvim_buf_delete(bufnr, {
--           force = true,
--         })
--       end
--     end
--   end,
-- })

-- Autosave
vim.api.nvim_create_autocmd({ "BufLeave", "FocusLost" }, {
  group = augroup("autowrite"),
  pattern = "*",
  callback = function()
    vim.cmd("silent! wall")
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

-- Refresh buffer when neogit status changed
vim.api.nvim_create_autocmd("User", {
  group = augroup("neogit"),
  pattern = "NeogitStatusRefreshed",
  callback = function()
    vim.cmd("set autoread | checktime")
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

-- keyboard
local session_type = os.getenv("XDG_SESSION_TYPE")
local is_wayland = session_type == "wayland"
local is_x11 = session_type == "x11"

local function config_keyboard(delay, rate)
  if is_wayland then
    vim.fn.system({ "swaymsg", "input", "type:keyboard", "repeat_delay", tostring(delay) })
    vim.fn.system({ "swaymsg", "input", "type:keyboard", "repeat_rate", tostring(rate) })
  elseif is_x11 then
    vim.fn.system({ "xset", "r", "rate", tostring(delay), tostring(rate) })
  end
end

vim.api.nvim_create_autocmd({ "VimEnter", "FocusGained" }, {
  group = augroup("keyboard"),
  callback = function()
    vim.schedule(function()
      config_keyboard(600, 25)
    end)
  end,
})

vim.api.nvim_create_autocmd({ "ExitPre", "FocusLost" }, {
  group = augroup("keyboard"),
  callback = function()
    config_keyboard(220, 33)
  end,
})
