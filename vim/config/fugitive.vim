autocmd FileType gitcommit startinsert
autocmd FileType gitcommit nnoremap <buffer> <C-c> :x<CR>
autocmd FileType fugitive nnoremap <buffer> <C-s> :Git stage .<CR>
autocmd FileType fugitive nnoremap <buffer> Pp :Git push<CR>
autocmd FileType fugitive nnoremap <buffer> pp :Git pull<CR>
autocmd FileType fugitive nnoremap <buffer> q :bdelete!<CR>

nnoremap <leader>gs :tab Git<CR>
