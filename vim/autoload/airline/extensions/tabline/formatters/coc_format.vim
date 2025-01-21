function! airline#extensions#tabline#formatters#coc_format#format(bufnr, buffers)
  " Get the buffer name
  let l:bufname = bufname(a:bufnr)
  if l:bufname == ''
    return '[No Name]'
  endif

  " Get diagnostic info from b:coc_diagnostic_info
  let l:diagnostic_info = getbufvar(a:bufnr, 'coc_diagnostic_info', {'error': 0, 'warning': 0})

  " Extract error and warning counts
  let l:error = l:diagnostic_info['error']
  let l:warning = l:diagnostic_info['warning']

  let l:error_circle = ''
  let l:warning_circle = ''

  " Build the tabline output
  let l:output = fnamemodify(l:bufname, ':t')

  " Append errors if present
  if l:error > 0
    execute 'highlight airline_tabsel guifg=#e86671'
    let l:output .= printf(' %s %d', l:error_circle, l:error)
  endif

  " Append warnings if present
  if l:warning > 0
    execute 'highlight airline_tabsel guifg=#e5c07b'
    let l:output .= printf(' %s %d', l:warning_circle, l:warning)
  endif

  if l:error == 0 && l:warning == 0
    execute 'highlight airline_tabsel guifg=#abb2bf'
  endif

  return l:output
endfunction
