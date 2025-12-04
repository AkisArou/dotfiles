if !exists('$NVIM_EMAIL')
  call timer_start(50, { -> execute('nnoremap <buffer> q :bd!<CR>') })
endif
