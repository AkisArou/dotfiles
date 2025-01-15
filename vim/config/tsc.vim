function! HandleTSCOutput(channel, line)
  if a:line =~ '\v^(.+)\((\d+),(\d+)\): error TS\d+: (.+)$'
    " Parse the line into file, line, column, and message
    let l:match = matchlist(a:line, '\v^(.+)\((\d+),(\d+)\): error TS\d+: (.+)$')
    if !empty(l:match)
      let l:file = l:match[1]
      let l:lnum = str2nr(l:match[2])
      let l:col = str2nr(l:match[3])
      let l:text = l:match[4]

      " Add the error to the quickfix list
      call setqflist([{'filename': l:file, 'lnum': l:lnum, 'col': l:col, 'text': l:text}], 'a')
    endif
  endif
endfunction


function! HandleTSCExit(channel, exit_code)
    echom "tsc --watch --build exited with code " . string(a:exit_code)
endfunction

function! StartTSCWatch()
 " Start tsc --watch --build as a background job
 let s:tsc_job = job_start(['npx', 'tsc', '--build', '--watch'], #{
		\ out_cb: function('HandleTSCOutput'),
		\ err_cb: function('HandleTSCOutput'),
		\ exit_cb: function('HandleTSCExit'),
		\ })

   echom "Started tsc --watch --build, job id: " . s:tsc_job
endfunction

" Auto-run the TypeScript watch if the current working directory contains 'nable-solutions'
autocmd VimEnter * 
  \ if match(getcwd(), '/nable-solutions') >= 0 |
  \   echom "nable-solutions directory found, starting tsc watch..." | 
  \   call StartTSCWatch() | 
  \ endif
