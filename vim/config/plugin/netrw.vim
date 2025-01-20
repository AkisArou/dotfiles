let g:netrw_dirhistmax = 0
let g:netrw_keepdir = 1
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

" Automatically focus the current file in netrw when opening netrw
" nmap <silent> <Leader>x :Ex <bar> :sil! call search(expand("#:t"))<CR>
