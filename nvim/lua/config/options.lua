vim.g.mapleader = " "
vim.g.maplocalleader = " "

local opt = vim.opt

opt.spelllang = { "en" }
opt.autowrite = true
opt.autowriteall = true
opt.cursorline = true -- highlight the current line
opt.backup = false -- creates a backup file
-- opt.clipboard = vim.env.SSH_TTY and "" or "unnamedplus" -- Sync with system clipboard
opt.clipboard:append("unnamedplus") -- use system clipboard as default register
opt.cmdheight = 1 -- more space in the neovim command line for displaying messages
opt.cmdwinheight = 20
opt.completeopt = { "menuone", "noselect", "popup", "fuzzy" } -- mostly just for cmp
opt.conceallevel = 0 -- so that `` is visible in markdown files
opt.fileencoding = "utf-8" -- the encoding written to a file
opt.hlsearch = true -- highlight all matches on previous search pattern
opt.ignorecase = true -- ignore case in search patterns
opt.mouse = "a" -- allow the mouse to be used in neovim
opt.pumheight = 10 -- pop up menu height
opt.pumblend = 10 -- Popup blend
opt.showmode = false -- we don't need to see things like -- INSERT -- anymore
opt.showtabline = 0 -- always show tabs
opt.smartcase = true -- smart case
opt.smartindent = true -- make indenting smarter again
opt.splitbelow = true -- force all horizontal splits to go below current window
opt.splitright = true -- force all vertical splits to go to the right of current window
opt.shortmess:append({ c = true, C = true })
opt.swapfile = false -- creates a swapfile
opt.termguicolors = true -- set term gui colors (most terminals support this)
opt.timeout = true
opt.timeoutlen = 300 -- time to wait for a mapped sequence to complete (in milliseconds)
opt.undofile = true -- enable persistent undo
opt.undolevels = 10000
opt.updatetime = 200 -- Save swap file and trigger CursorHold
opt.writebackup = false -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
opt.expandtab = true -- convert tabs to spaces
opt.shiftround = true -- Round indent
opt.shiftwidth = 2 -- the number of spaces inserted for each indentation
opt.tabstop = 2 -- insert 2 spaces for a tab
opt.softtabstop = 2
opt.number = true -- set numbered lines
opt.relativenumber = false
opt.virtualedit = "block" -- Allow cursor to move where there is no text in visual block mode
opt.laststatus = 3 -- only the last window will always have a status line
opt.showcmd = false -- hide (partial) command in the last line of the screen (for performance)
opt.ruler = false -- hide the line and column number of the cursor position
opt.numberwidth = 4 -- minimal number of columns to use for the line number {default 4}
opt.signcolumn = "yes" -- always show the sign column, otherwise it would shift the text each time
opt.wrap = false -- display lines as one long line
-- opt.scrolloff = 25 -- minimal number of screen lines to keep above and below the cursor
opt.sidescrolloff = 8 -- minimal number of screen columns to keep to the left and right of the cursor if wrap is `false`
opt.winminwidth = 5 -- Minimum window width
-- vim.opt.termsync = true -- terminal synchronized output
opt.guifont = "monospace:h17" -- the font used in graphical neovim applications
opt.fillchars.eob = " " -- show empty lines at the end of a buffer as ` ` {default `~`}
opt.shortmess:append("c") -- hide all the completion messages, e.g. "-- XXX completion (YYY)", "match 1 of 2", "The only match", "Pattern not found"
opt.whichwrap:append("<,>,[,],h,l") -- keys allowed to move to the previous/next line when the beginning/end of line is reached
opt.iskeyword:append("-") -- treats words with `-` as single words
opt.formatoptions:remove({ "c", "r", "o" }) -- This is a sequence of letters which describes how automatic formatting is to be done
opt.linebreak = true

opt.grepprg = "rg --vimgrep --smart-case --hidden"
opt.grepformat = "%f:%l:%c:%m"

vim.cmd([[autocmd FileType * set formatoptions-=ro]]) -- disable new line auto comment
