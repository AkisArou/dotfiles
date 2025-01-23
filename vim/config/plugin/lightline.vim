" Disable lightline if using a limited terminal:
if !has('gui_running') && &t_Co < 256
  let g:lightline = {
        \ 'enable': {
        \   'statusline': 0,
        \   'tabline'   : 0,
        \ },
        \ }
  finish
endif

let g:lightline#bufferline#modified = ' ●'

let g:lightline = {
      \ 'colorscheme': 'one',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'modified', 'gitbranch', 'cocstatus', 'currentfunction' ] ]
      \ },
      \ 'tabline': {
      \   'left': [ ['buffers'] ],
      \   'right': [ [] ]
      \ },
      \ 'component_expand': {
      \   'buffers': 'lightline#bufferline#buffers'
      \ },
      \ 'component_type': {
      \   'buffers': 'tabsel'
      \ },
      \ 'component_function': {
      \   'cocstatus': 'coc#status',
      \   'currentfunction': 'CocCurrentFunction',
      \   'gitbranch': 'gitbranch#name'
      \ },
      \ }


let g:lightline.enable = {
        \ 'statusline': 1,
        \ 'tabline': 1
        \ }

function! Show_coc_status() 
  let l:diagnostic_info = getbufvar(bufnr('%'), 'coc_diagnostic_info', {'error': 0, 'warning': 0})


  " Extract error and warning counts
  let l:error = l:diagnostic_info['error']
  let l:warning = l:diagnostic_info['warning']

  " Append warnings if present
  if l:warning > 0
    execute 'highlight LightlineLeft_active_1 guibg=#e5c07b guifg=#282a36'
  endif

  " Append errors if present
  if l:error > 0
    execute 'highlight LightlineLeft_active_1 guibg=#e86671 guifg=#282a36'
  endif


  if l:error == 0 && l:warning == 0
    execute 'highlight LightlineLeft_active_1 ctermfg=145 ctermbg=240 guifg=#abb2bf guibg=#3e4452'
  endif

endfunction

autocmd User CocStatusChange,CocDiagnosticChange call lightline#update()
autocmd User CocStatusChange,CocDiagnosticChange call Show_coc_status()

function! OverrideColors() abort
  let g:lightline#colorscheme#{g:lightline.colorscheme}#palette.tabline.tabsel = [['#abb2bf', '#282a36', 235, 176]]
  call g:lightline#colorscheme()
endfunction

" Set up a timer to call the function after 1 second (1000 ms)
autocmd VimEnter * call OverrideColors()
