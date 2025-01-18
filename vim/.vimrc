nnoremap <SPACE> <Nop>
let mapleader=" "

" Disable compatibility with vi which can cause unexpected issues.
set nocompatible

syntax on

filetype on
filetype plugin on
filetype indent on

set cursorline

let &t_SI = "\e[6 q"
let &t_EI = "\e[0 q"

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
set showtabline=0
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
set formatoptions-=cro


let opts = {'noremap': v:true, 'silent': v:true}

" Move blocks
xnoremap J :m '>+1<CR>gv=gv
xnoremap K :m '<-2<CR>gv=gv

" Cursor stays in place when moving screen
nnoremap <C-d> m`<C-d>zz
nnoremap <C-u> m`<C-u>zz

" Better paste
xnoremap p P

" Delete char without copying
nnoremap x "_x

" Stay in indent mode
xnoremap < <gv
xnoremap > >gv

nnoremap <leader>w :w<CR>
nnoremap <leader>h :nohl<CR>

let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/fzf.vim'
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
  Plug 'chriszarate/yazi.vim'
  Plug 'tomasiser/vim-code-dark'
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'jiangmiao/auto-pairs'
  Plug 'kana/vim-textobj-user'
  Plug 'tpope/vim-commentary'
  Plug 'mg979/vim-visual-multi', {'branch': 'master'}
  Plug 'tpope/vim-fugitive'
  Plug 'schickling/vim-bufonly'
  Plug 'liuchengxu/vim-which-key'
  Plug 'gelguy/wilder.nvim'
  Plug 'kana/vim-textobj-user'
  Plug 'beloglazov/vim-textobj-quotes'
  Plug 'jasonccox/vim-wayland-clipboard'
  Plug 'markonm/traces.vim'
  Plug 'machakann/vim-sandwich'
  Plug 'AkisArou/npm-workspaces-lsp', {'do': 'pnpm install && pnpm run build-coc'}
  Plug 'sonph/onehalf', {'rtp': 'vim/'}
  Plug 'airblade/vim-gitgutter'
call plug#end()

nnoremap <silent> <leader> :WhichKey '<Space>'<CR>

" colorscheme codedark
colorscheme onehalfdark
let g:airline_theme='onehalfdark'

source ~/dotfiles/vim/config/coc.vim
source ~/dotfiles/vim/config/fzf.vim
source ~/dotfiles/vim/config/tsc.vim
source ~/dotfiles/vim/config/wilder.vim
source ~/dotfiles/vim/config/buffers.vim
source ~/dotfiles/vim/config/netrw.vim
source ~/dotfiles/vim/config/airline.vim
source ~/dotfiles/vim/config/fugitive.vim
source ~/dotfiles/vim/config/yazi.vim

autocmd BufLeave,FocusLost * silent! wall
