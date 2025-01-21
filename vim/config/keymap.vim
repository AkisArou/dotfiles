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

nmap <leader>y <Plug>YADefault
xmap <leader>y <Plug>YADefault

