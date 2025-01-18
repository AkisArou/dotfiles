autocmd CmdlineEnter * ++once call s:wilder_init() | call wilder#main#start()

function! s:wilder_init() abort
  call wilder#setup({
      \ 'modes': [':', '/', '?'],
      \ 'next_key': '<C-n>',
      \ 'previous_key': '<C-p>',
      \ 'accept_key': '<C-e>',
      \ 'reject_key': '<C-c>',
      \ })

  call wilder#set_option('noselect', 0)


  let l:wilder_hl = {
        \ 'default': wilder#make_hl('WilderPmenu', [
        \     {}, {'foreground': 0, 'background': 11},
        \     {'foreground': '#dcdfe4', 'background': '#1a1d21'}
        \ ]),
        \ 'selected': wilder#make_hl('WilderAccent', 'WilderPmenu', [
        \     {}, {}, {'foreground': '#c678dd'}])
        \ }

  let l:wilder_renderer_option = {
    \ 'highlights': l:wilder_hl,
    \ }

  call wilder#set_option('renderer', wilder#popupmenu_renderer(l:wilder_renderer_option))

  call wilder#set_option('pipeline', [
        \   wilder#branch(
        \     wilder#cmdline_pipeline({
        \       'language': 'vim',
        \       'fuzzy': 1,
        \       'fuzzy_filter': wilder#vim_fuzzy_filter(),
        \     }),
        \     wilder#search_pipeline({
        \       'debounce': 10,
        \     }),
        \   ),
        \ ])
endfunction
