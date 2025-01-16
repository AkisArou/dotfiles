let g:netrw_dirhistmax = 0
let g:netrw_keepdir = 0
let g:netrw_localmkdir = "mkdir -p"
let g:netrw_localcopycmd = "cp -r"
let g:netrw_localrmdir = "rm -r"

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

" " Automatically focus the current file in netrw when opening netrw
" autocmd FileType netrw call FocusCurrentFile()

" function! FocusCurrentFile()
"   " Get the full path of the current file
"   let file = expand('%:p')
"   if filereadable(file)
"     " Run the :NetrwRefresh command to highlight the file in netrw
"     execute 'normal! m`'    " Store the current cursor position
"     execute 'normal! /'.file   " Search for the file in netrw
"     execute 'normal! n'      " Move to the file
"     execute 'normal! m'`'    " Restore the cursor position
"   endif
" endfunction

