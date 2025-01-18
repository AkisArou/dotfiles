let g:airline_theme='lucius'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#fugitive#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'

function! UpdateAirlineTabHighlight() abort
  if !exists(':CocCommand')
    return
  endif

  let info = get(b:, 'coc_diagnostic_info', {})

  if empty(info) | return | endif

  if get(info, 'error', 0)
    execute 'highlight airline_tabsel guifg=#ff0000'
    return
  endif

  if get(info, 'warning', 0)
    execute 'highlight airline_tabsel guifg=#ffa500'
    return
  endif


  execute 'highlight airline_tabsel guifg=#ffffff'
endfunction


:autocmd User CocDiagnosticChange call UpdateAirlineTabHighlight()
