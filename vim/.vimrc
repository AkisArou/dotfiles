nnoremap <SPACE> <Nop>
let mapleader=" "

set nocompatible

syntax on

filetype on
filetype plugin on
filetype indent on

runtime ftplugin/man.vim

source ~/dotfiles/vim/config/plugin/plug.vim
source ~/dotfiles/vim/config/plugin/colorscheme.vim
source ~/dotfiles/vim/config/plugin/coc.vim
source ~/dotfiles/vim/config/plugin/fzf.vim
source ~/dotfiles/vim/config/plugin/tsc.vim
source ~/dotfiles/vim/config/plugin/wilder.vim
source ~/dotfiles/vim/config/plugin/netrw.vim
source ~/dotfiles/vim/config/plugin/lightline.vim
source ~/dotfiles/vim/config/plugin/yazi.vim
source ~/dotfiles/vim/config/plugin/sandwich.vim
source ~/dotfiles/vim/config/plugin/vimspector.vim
source ~/dotfiles/vim/config/plugin/vimtest.vim

let &t_SI = "\e[6 q"
let &t_EI = "\e[0 q"
let &t_Cs = "\e[4:3m"
let &t_Ce = "\e[0m"

set autowrite
set autowriteall
set cursorline
set ttyscroll=1
set nobackup
set incsearch
set clipboard+=unnamedplus
set cmdheight=1
set completeopt=menuone,noselect
set conceallevel=0
set fileencoding=utf-8
set hlsearch
set ignorecase
set mouse=
set pumheight=10
set showmode        
set showcmd         
set laststatus=2    
set showtabline=2
set smartcase
set smartindent
set splitbelow
set splitright
set noswapfile
set termguicolors
set timeout timeoutlen=300
set undofile
set nowritebackup
set expandtab
set shiftwidth=2
set tabstop=2
set cursorline
set number
set norelativenumber
set noshowcmd
set noruler
set numberwidth=4
set signcolumn=yes
set nowrap
set scrolloff=25
set sidescrolloff=8
set guifont=monospace:h17
set shortmess+=c
set whichwrap+=<,>,[,],h,l
set iskeyword+=-
set linebreak
set encoding=utf-8
set nobackup
set nowritebackup
set updatetime=300
set signcolumn=yes
set background=dark
set noshowmode
set formatoptions-=cro
set guioptions-=e

source ~/dotfiles/vim/config/keymap.vim
source ~/dotfiles/vim/config/autocmd.vim
source ~/dotfiles/vim/config/buffers.vim
