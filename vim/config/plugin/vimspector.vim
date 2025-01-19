let g:vimspector_install_gadgets = ['vscode-js-debug', 'debugger-for-chrome']

" let g:vimspector_enable_mappings = 'HUMAN'
nnoremap <leader>dB <Plug>VimspectorToggleConditionalBreakpoint
nnoremap <leader>db <Plug>VimspectorToggleBreakpoint
nnoremap <leader>dc <Plug>VimspectorContinue
nnoremap <leader>di <Plug>VimspectorStepInto
nnoremap <leader>do	<Plug>VimspectorStepOut
nnoremap <leader>dO	<Plug>VimspectorStepOver
nnoremap <leader>dp	<Plug>VimspectorPause
nnoremap <leader>dt	<Plug>VimspectorStop
nnoremap <leader>dC	<Plug>VimspectorRunToCursor
nnoremap <leader>dr	<Plug>VimspectorRestart
nnoremap <leader>dT	:VimspectorReset<CR>
" F8	<Plug>VimspectorAddFunctionBreakpoint	Add a function breakpoint for the expression under cursor

function! PickProcess( ... ) abort
  let ps = 'ps aux'

  let line_selected = fzf#run( {
      \ 'source': ps,
      \ 'options': '--header-lines=1  '
      \          . '--prompt="Select Process: " '
      \ ,
      \
      \ } )[ 0 ]
  if empty( line_selected)
    return 0
  endif
  let pid = split( line_selected )[ 0 ]
  return str2nr( pid )
endfunction


let g:vimspector_custom_process_picker_func = 'PickProcess'
