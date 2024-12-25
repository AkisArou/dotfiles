nnoremap <SPACE> <Nop>
let mapleader=" "

nnoremap <leader>w :w<CR>
nnoremap <leader>h :nohl<CR>

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

let g:netrw_dirhistmax = 0

set ttyscroll=1
set nobackup
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
set updatetime=300
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
set formatoptions-=cro
set linebreak
set encoding=utf-8
set nobackup
set nowritebackup
set updatetime=300
set signcolumn=yes
set background=dark

let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
  Plug 'chriszarate/yazi.vim'
  Plug 'tomasiser/vim-code-dark'
  Plug 'christoomey/vim-tmux-navigator'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'jiangmiao/auto-pairs'
  Plug 'kana/vim-textobj-user'
  Plug 'preservim/vim-textobj-quote'
  Plug 'tpope/vim-commentary'
  " Plug 'tpope/vim-vinegar'
  Plug 'tpope/vim-fugitive'
  Plug 'schickling/vim-bufonly'
call plug#end()

autocmd FileType gitcommit startinsert
autocmd FileType gitcommit nnoremap <buffer> <C-c> :x<CR>
autocmd FileType fugitive nnoremap <buffer> <C-s> :Git stage .<CR>
autocmd FileType fugitive nnoremap <buffer> Pp :Git push<CR>
autocmd FileType fugitive nnoremap <buffer> pp :Git pull<CR>
autocmd FileType fugitive nnoremap <buffer> q :bdelete!<CR>

" nnoremap <leader>e :Explore<CR>
nnoremap <leader>e :Yazi<CR>

let g:netrw_keepdir = 0
let g:netrw_localmkdir = "mkdir -p"
let g:netrw_localcopycmd = "cp -r"
let g:netrw_localrmdir = "rm -r"

function! NetrwMapping()
  nmap <buffer> <C-c> :bw<CR>
  nmap <buffer> H u
  nmap <buffer> h -^
  nmap <buffer> l <CR>
  nmap <buffer> . gh
  nmap <buffer> P <C-w>z
  nmap <buffer> L <CR>:Lexplore<CR>
  nmap <buffer> <Leader>dd :Lexplore<CR>
endfunction

augroup netrw_mapping
  autocmd!
  autocmd filetype netrw call NetrwMapping()
augroup END

" " Automatically focus the current file in netrw when opening netrw
" autocmd FileType netrw call FocusCurrentFile()

" function! FocusCurrentFile()
"   " Get the full path of the current file
"   let file = expand('%:p')
"   if filereadable(file)
"     " Run the :NetrwRefresh command to highlight the file in netrw
"     execute 'normal! m`'    " Store the current cursor position
"     execute 'normal! /'.file   " Search for the file in netrw
"     execute 'normal! n'      " Move to the file
"     execute 'normal! m'`'    " Restore the cursor position
"   endif
" endfunction


colorscheme codedark

source ~/dotfiles/vim/config/coc.vim
source ~/dotfiles/vim/config/fzf.vim

let g:airline_theme='lucius'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#fugitive#enabled = 1
let g:airline_powerline_fonts = 1

" nnoremap <silent> - :Yazi<cr>
" nnoremap <silent> _ :YaziWorkingDirectory<cr>

function! CloseEmptyUnnamedBuffers()
   let buffers = filter(range(1, bufnr('$')), 'bufexists(v:val)')
    
   for buf in buffers
     if buflisted(buf) && bufname(buf) == ''
       exe buf.'bd!'
     endif
   endfor
endfunction

autocmd BufReadPost * call CloseEmptyUnnamedBuffers()

" Define options for key mappings (same as `opts` in Lua)
let opts = {'noremap': v:true, 'silent': v:true}

nnoremap <S-h> :w<CR> :bnext<CR>
nnoremap <S-l> :w<CR> :bprevious<CR>

" close the current buffer
nnoremap <silent> <leader>bd :bdelete<CR>
" close all buffers except the current one
nnoremap <silent> <leader>bo  :wa!<CR> :BufOnly<CR> :AirlineRefresh<CR> :AirlineRefresh<CR>
" close all buffers
nnoremap <silent> <leader>ba :wa!<CR> :bufdo bdelete<CR>

set nocompatible
filetype plugin on       " may already be in your .vimrc

augroup textobj_quote
  autocmd!
  autocmd FileType vim,markdown,textile call textobj#quote#init()
  autocmd FileType text call textobj#quote#init({'educate': 0})
augroup END

nnoremap <leader>gs :tab Git<CR>
