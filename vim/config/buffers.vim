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

nnoremap <S-h> :w<CR> :bprevious<CR>
nnoremap <S-l> :w<CR> :bnext<CR>

" close the current buffer
nnoremap <silent> <leader>bd :bdelete<CR>
" close all buffers except the current one
nnoremap <silent> <leader>bo  :wa!<CR> :BufOnly<CR> :AirlineRefresh<CR> :AirlineRefresh<CR>
" close all buffers
nnoremap <silent> <leader>ba :wa!<CR> :bufdo bdelete<CR>
