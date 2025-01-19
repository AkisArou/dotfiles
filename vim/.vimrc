nnoremap <SPACE> <Nop>
let mapleader=" "

set nocompatible

syntax on

filetype on
filetype plugin on
filetype indent on

source ~/dotfiles/vim/config/plug.vim
source ~/dotfiles/vim/config/colorscheme.vim
source ~/dotfiles/vim/config/coc.vim
source ~/dotfiles/vim/config/fzf.vim
source ~/dotfiles/vim/config/tsc.vim
source ~/dotfiles/vim/config/wilder.vim
source ~/dotfiles/vim/config/buffers.vim
source ~/dotfiles/vim/config/netrw.vim
source ~/dotfiles/vim/config/airline.vim
source ~/dotfiles/vim/config/yazi.vim
source ~/dotfiles/vim/config/sandwich.vim

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

" Move blocks
xnoremap J :m '>+1<CR>gv=gv
xnoremap K :m '<-2<CR>gv=gv

" Cursor stays in place when moving screen
function! Scroll(direction)
  set lazyredraw
  if a:direction == 'down'
    execute "normal! m`\<C-d>"
  elseif a:direction == 'up'
    execute "normal! m`\<C-u>"
  endif
  set nolazyredraw
  normal! zz
endfunction

nnoremap <silent> <C-d> :call Scroll('down')<CR>
nnoremap <silent> <C-u> :call Scroll('up')<CR>

" Better paste
xnoremap p P

" Delete char without copying
nnoremap x "_x

" Stay in indent mode
xnoremap < <gv
xnoremap > >gv

nnoremap <leader>w :w<CR>
nnoremap <leader>h :nohl<CR>

nnoremap <silent> <leader> :WhichKey '<Space>'<CR>
nnoremap <leader>gs :silent !lazygit<CR>\|:silent redraw!<CR>

autocmd BufLeave,FocusLost * silent! wall
