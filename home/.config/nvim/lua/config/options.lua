vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.o.spelllang = "en"
vim.o.autowrite = true
vim.o.autowriteall = true
vim.o.cursorline = true -- highlight the current line
vim.o.backup = false -- creates a backup file
-- vim.o.clipboard = vim.env.SSH_TTY and "" or "unnamedplus" -- Sync with system clipboard
vim.opt.clipboard:append("unnamedplus") -- use system clipboard as default register
vim.o.cmdheight = 1 -- more space in the neovim command line for displaying messages
vim.o.cmdwinheight = 20
vim.o.completeopt = "menuone,noinsert,popup,fuzzy" -- mostly just for cmp
vim.o.conceallevel = 0 -- so that `` is visible in markdown files
vim.o.fileencoding = "utf-8" -- the encoding written to a file
vim.o.hlsearch = true -- highlight all matches on previous search pattern
vim.o.ignorecase = true -- ignore case in search patterns
vim.o.pumheight = 10 -- pop up menu height
vim.o.pumblend = 10 -- Popup blend
vim.o.showmode = false -- we don't need to see things like -- INSERT -- anymore
vim.o.showtabline = 1 -- show tabs when >1
vim.o.smartcase = true -- smart case
vim.o.smartindent = true -- make indenting smarter again
vim.o.splitbelow = true -- force all horizontal splits to go below current window
vim.o.splitright = true -- force all vertical splits to go to the right of current window
vim.opt.shortmess:append("cC")
vim.o.swapfile = false -- creates a swapfile
vim.o.termguicolors = true -- set term gui colors (most terminals support this)
vim.o.timeout = true
vim.o.timeoutlen = 300 -- time to wait for a mapped sequence to complete (in milliseconds)
vim.o.undofile = true -- enable persistent undo
vim.o.undolevels = 10000
vim.o.updatetime = 50 -- Save swap file and trigger CursorHold
vim.o.writebackup = false -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
vim.o.expandtab = true -- convert tabs to spaces
vim.o.shiftround = true -- Round indent
vim.o.shiftwidth = 2 -- the number of spaces inserted for each indentation
vim.o.tabstop = 2 -- insert 2 spaces for a tab
vim.o.softtabstop = 2
vim.o.number = true -- set numbered lines
vim.o.relativenumber = true
vim.o.virtualedit = "block" -- Allow cursor to move where there is no text in visual block mode
vim.o.laststatus = 3 -- only the last window will always have a status line
vim.o.showcmd = false -- hide (partial) command in the last line of the screen (for performance)
vim.o.ruler = false -- hide the line and column number of the cursor position
vim.o.numberwidth = 4 -- minimal number of columns to use for the line number {default 4}
vim.o.signcolumn = "yes" -- always show the sign column, otherwise it would shift the text each time
vim.o.wrap = false -- display lines as one long line
-- vim.o.scrolloff = 25 -- minimal number of screen lines to keep above and below the cursor
vim.o.sidescrolloff = 8 -- minimal number of screen columns to keep to the left and right of the cursor if wrap is `false`
vim.o.winminwidth = 5 -- Minimum window width
-- vim.o.termsync = true -- terminal synchronized output
vim.o.guifont = "Hasklug Nerd Font Mono:h12" -- the font used in graphical neovim applications
vim.opt.fillchars:append("eob: ") -- show empty lines at the end of a buffer as ` ` {default `~`}
vim.opt.shortmess:append("c") -- hide all the completion messages, e.g. "-- XXX completion (YYY)", "match 1 of 2", "The only match", "Pattern not found"
vim.opt.whichwrap:append("<,>,[,],h,l") -- keys allowed to move to the previous/next line when the beginning/end of line is reached
vim.o.formatoptions = vim.o.formatoptions:gsub("[cro]", "") -- This is a sequence of letters which describes how automatic formatting is to be done
vim.o.linebreak = true -- Wrap lines at convenient points
vim.o.nrformats = "blank"
vim.o.foldenable = false -- Disable folding by default
vim.o.foldmethod = "manual"

vim.o.grepprg = "rg --vimgrep --smart-case --hidden"
vim.o.grepformat = "%f:%l:%c:%m"
vim.o.exrc = true

vim.cmd([[set mouse=]])

-- Workaround for issue: https://github.com/neovim/neovim/issues/14061
-- Fixes issue with term buffers when :wqa
vim.api.nvim_create_user_command("Z", function()
  vim.cmd("write")
  vim.cmd("qa")
end, {})

vim.cmd([[cabbrev wqa Z]])

-- Disable built-in runtime plugins
local disabled_builtins = {
  "gzip",
  "tar",
  "tarPlugin",
  "zip",
  "zipPlugin",
  "getscript",
  "getscriptPlugin",
  "vimball",
  "vimballPlugin",
  "2html_plugin",
  "logipat",
  "rrhelper",
  "netrw",
  "netrwPlugin",
  "netrwSettings",
  "netrwFileHandlers",
  "tutor",
  "tohtml",
  "nvim_dir_plugin",
}

for _, plugin in ipairs(disabled_builtins) do
  vim.g["loaded_" .. plugin] = 1
end

require("vim._core.ui2").enable({
  enable = true,
  msg = { -- Options related to the message module.
    ---@type 'cmd'|'msg' Where to place regular messages, either in the
    ---cmdline or in a separate ephemeral message window.
    target = "cmd",
    -- timeout = 4000, -- Time a message is visible in the message window.
  },
})
