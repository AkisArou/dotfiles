let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()
  Plug 'tpope/vim-sensible'
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
  Plug 'schickling/vim-bufonly'
  Plug 'liuchengxu/vim-which-key'
  Plug 'gelguy/wilder.nvim'
  Plug 'beloglazov/vim-textobj-quotes'
  Plug 'jasonccox/vim-wayland-clipboard'
  Plug 'markonm/traces.vim'
  Plug 'machakann/vim-sandwich'
  Plug 'tpope/vim-surround'
  Plug 'AkisArou/npm-workspaces-lsp', {'do': 'pnpm install && pnpm run build-coc'}
  Plug 'airblade/vim-gitgutter'
  Plug 'joshdick/onedark.vim'
  Plug 'sheerun/vim-polyglot'
call plug#end()

delc PlugUpgrade

autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif
