nnoremap <SPACE> <Nop>
let mapleader=" "

" Disable compatibility with vi which can cause unexpected issues.
set nocompatible

" Enable type file detection. Vim will be able to try to detect the type of file in use.
filetype on

" Enable plugins and load plugin for the detected file type.
filetype plugin on

" Load an indent file for the detected file type.
filetype indent on

syntax on

" Highlight cursor line underneath the cursor horizontally.
set cursorline

" Use a line cursor within insert mode and a block cursor everywhere else.
"
" Reference chart of values:
"   Ps = 0  -> blinking block.
"   Ps = 1  -> blinking block (default).
"   Ps = 2  -> steady block.
"   Ps = 3  -> blinking underline.
"   Ps = 4  -> steady underline.
"   Ps = 5  -> blinking bar (xterm).
"   Ps = 6  -> steady bar (xterm).
let &t_SI = "\e[6 q"
let &t_EI = "\e[0 q"

set nobackup
set clipboard=unnamedplus
set cmdheight=1
set completeopt=menuone,noselect
set conceallevel=0
set fileencoding=utf-8
set hlsearch
set ignorecase
set mouse=a
set pumheight=10
set noshowmode
set showtabline=0
set smartcase
set smartindent
set splitbelow
set splitright
set noswapfile
set termguicolors
set timeout timeoutlen=300
set undofile
set updatetime=300
set nowritebackup
set expandtab
set shiftwidth=2
set tabstop=2
set cursorline
set number
set norelativenumber
set laststatus=3
set noshowcmd
set noruler
set numberwidth=4
set signcolumn=yes
set nowrap
set scrolloff=8
set sidescrolloff=8
set guifont=monospace:h17
set shortmess+=c
set whichwrap+=<,>,[,],h,l
set iskeyword+=-
set formatoptions-=cro
set linebreak
set encoding=utf-8
set nobackup
set nowritebackup
set updatetime=300
set signcolumn=yes
set background=dark

colorscheme wildcharm

nnoremap <leader>w :w<CR>
