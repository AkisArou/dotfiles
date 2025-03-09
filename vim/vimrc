nnoremap <SPACE> <Nop>
let mapleader=" "

set nocompatible

syntax on

filetype on
filetype plugin on
filetype indent on

let &t_SI = "\e[6 q"
let &t_EI = "\e[0 q"

" Undercurl support
let &t_Cs = "\e[4:3m"
let &t_Ce = "\e[4:0m"

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
set encoding=utf-8
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
set backspace=eol,start,indent
set whichwrap+=<,>,[,],h,l
set iskeyword+=-
set linebreak
set nobackup
set nowritebackup
set updatetime=300
set signcolumn=yes
set background=dark
set wop+=fuzzy
set noshowmode
set formatoptions-=cro
set guioptions-=e
set wildoptions+=pum

colorscheme sorbet

set autoread

let g:netrw_dirhistmax = 0
let g:netrw_keepdir = 1
let g:netrw_localmkdir = "mkdir -p"
let g:netrw_localcopycmd = "cp -r"
let g:netrw_localrmdir = "rm -r"

" Automatically focus the current file in netrw when opening netrw
" nmap <silent> <Leader>x :Ex <bar> :sil! call search(expand("#:t"))<CR>
"
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

autocmd BufLeave,FocusLost * silent! wall

nnoremap <silent> <S-h> :bprevious<CR>
nnoremap <silent> <S-l> :bnext<CR>

function! CloseOtherBuffers()
  " Get the current buffer number
  let l:current_buffer = bufnr('%')

  " Loop through all buffers and delete them, except the current one                                                 
  for l:buf in range(1, bufnr('$'))
    if l:buf != l:current_buffer && bufexists(l:buf)
      execute 'bwipeout' l:buf
    endif
  endfor
endfunction                                                                                                          

nnoremap <silent> <leader>bd :bdelete<CR>
nnoremap <silent> <leader>bo :wa!<CR> :call CloseOtherBuffers()<CR>
nnoremap <silent> <leader>ba :wa!<CR> :bufdo bdelete<CR>

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


let fzf_path = system('which fzf')                                                                                   
let fzf_path = substitute(fzf_path, '\n', '', 'g') " Remove any trailing newline                                     
if !empty(fzf_path)                                                                                                  
  execute 'set rtp+=' . fzf_path                                                                                     
endif

nnoremap <leader>ff <cmd>FZF<CR>                                                                                     
nnoremap <leader>e <cmd>Ex<CR>                                                                                     
