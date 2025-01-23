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
      \             [ 'readonly', 'filename', 'modified'  ], [ 'cocstatus', 'currentfunction' ] ],
      \   'right': [ [ 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'fileformat', 'fileencoding', 'filetype' ],
      \              [ 'gitbranch' ] ]
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


function! UpdateTabsel(color1, color2)
  let g:lightline#colorscheme#{g:lightline.colorscheme}#palette.tabline.tabsel = [[a:color1, a:color2, 235, 176]]
  call g:lightline#colorscheme()
endfunction

function! ShowStatusWarning() 
  let warning_bg = '#4e3a22'
  let warning_fg = '#abb2bf'
  
  call UpdateTabsel(warning_fg, warning_bg)

  execute 'highlight LightlineLeft_active_2 guibg=' . warning_bg . ' guifg=' . warning_fg
endfunction

function! ShowStatusError() 
  let error_bg = '#52272b'
  let error_fg = '#abb2bf'
  
  call UpdateTabsel(error_fg, error_bg)
  
  execute 'highlight LightlineLeft_active_2 guibg=' . error_bg . ' guifg=' . error_fg
endfunction

function! ShowStatusNormal() 
  let normal_bg = '#282a36'
  let normal_fg = '#abb2bf'
  
  call UpdateTabsel(normal_fg, normal_bg)
  
  execute 'highlight LightlineLeft_active_2 ctermfg=145 ctermbg=240 guifg=' . normal_fg . ' guibg=' . normal_bg
endfunction

function! Show_coc_status() 
  let l:diagnostic_info = getbufvar(bufnr('%'), 'coc_diagnostic_info', {'error': 0, 'warning': 0})

  let l:error = l:diagnostic_info['error']
  let l:warning = l:diagnostic_info['warning']

  if l:warning > 0
    call ShowStatusWarning()
  endif

  if l:error > 0
    call ShowStatusError()
  endif


  if l:error == 0 && l:warning == 0
    call ShowStatusNormal()
  endif

endfunction

autocmd User CocStatusChange,CocDiagnosticChange call lightline#update()
autocmd User CocStatusChange,CocDiagnosticChange call Show_coc_status()

autocmd VimEnter * call ShowStatusNormal()



