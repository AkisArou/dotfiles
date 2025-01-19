function! SaveAndYazi()
  if &modified
    write!
  endif
  Yazi
endfunction

nnoremap <leader>e :call SaveAndYazi()<CR>
