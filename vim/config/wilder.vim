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

  " Can also be passed to the 'highlights' option
  call wilder#set_option('renderer', wilder#popupmenu_renderer({
        \ 'highlighter': wilder#basic_highlighter(),
        \ 'highlights': {
        \   'accent': wilder#make_hl('WilderAccent', 'Pmenu', [{}, {}, {'foreground': '#f4468f'}]),
        \ },
        \ 'max_height': "20%"
        \ }))

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
