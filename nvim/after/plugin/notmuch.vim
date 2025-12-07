augroup MyNotmuch
  autocmd!
  " Run for any Notmuch-related buffer
  autocmd FileType notmuch-show,notmuch-folders,notmuch-search call s:setup_notmuch()
augroup END

function! s:setup_notmuch() abort
  " Single function to find and call a script-local function with optional args
  function! s:call_snr(funcname, ...)
    " Find full <SNR>xx function name
    let fullfunc = ''
    for f in split(execute('function'), "\n")
      if f =~ a:funcname
        let fullfunc = matchstr(f, 'function\s\+\zs<SNR>\d\+_' . a:funcname)
        if !empty(fullfunc)
          break
        endif
      endif
    endfor

    if empty(fullfunc)
      return
    endif

    " Call it
    if a:0
      execute 'call ' . fullfunc . '(' . join(a:000, ', ') . ')'
    else
      execute 'call ' . fullfunc . '()'
    endif
  endfunction

  autocmd FileType notmuch-show call s:call_snr('search_tag', "'-unread'")
  autocmd FileType notmuch-folders call timer_start(0, {-> s:call_snr('folders_refresh')})

  let g:notmuch_folders = [
    \ [ 'new', 'tag:inbox and tag:unread' ],
    \ [ 'inbox', 'tag:inbox' ],
    \ [ 'gmail', 'folder:gmail/Inbox' ],
    \ [ 'nablesolutions', 'folder:nablesolutions/Inbox' ],
    \ [ 'support', 'folder:support/Inbox' ],
    \ ]

  let g:notmuch_folders_maps = {
    \ '<C-e>':	'folders_show_search()',
    \ '<Enter>':	'folders_show_search()',
    \ 's':		'folders_search_prompt()',
    \ '=':		'folders_refresh()',
    \ 'c':		'compose()',
    \ }


  let g:notmuch_search_maps = {
    \ '<C-e>':	'search_show_thread(1)',
    \ 'q':		'kill_this_buffer()',
    \ '<Enter>':	'search_show_thread(1)',
    \ '<Space>':	'search_show_thread(2)',
    \ 'A':		'search_tag("-inbox -unread")',
    \ 'I':		'search_tag("-unread")',
    \ 't':		'search_tag("")',
    \ 's':		'search_search_prompt()',
    \ '=':		'search_refresh()',
    \ '?':		'search_info()',
    \ 'c':		'compose()',
    \ }

  let g:notmuch_show_maps = {
    \ 'q':		'kill_this_buffer()',
    \ 'A':		'show_tag("-inbox -unread")',
    \ 'I':		'show_tag("-unread")',
    \ 't':		'show_tag("")',
    \ 'o':		'show_open_msg()',
    \ 'e':		'show_extract_msg()',
    \ 's':		'show_save_msg()',
    \ 'p':		'show_save_patches()',
    \ 'r':		'show_reply()',
    \ '?':		'show_info()',
    \ '.':		'show_copy_id()',
    \ '<Tab>':	'show_next_msg()',
    \ '<Space>':	'show_message_tag("-inbox -unread")',
    \ 'c':		'compose()',
    \ }


  highlight nmComposeHelp        guifg=#7aa2f7      gui=none      " blue
  highlight nmComposeHelpLine    guifg=#f7768e      gui=bold      " red (error-like)

  highlight nmFoldersCount       guifg=#7aa2f7      gui=none      " yellow (statement)
  highlight nmFoldersName        guifg=#bb9af7      gui=none      " magenta (type)
  highlight nmFoldersSearch      guifg=#7dcfff      gui=none      " cyan (string-like)

  highlight CursorLine           term=NONE cterm=NONE gui=NONE

  highlight nmSearchDate         guifg=#7aa2f7      gui=none      " yellow
  highlight nmSearchNum          guifg=#bb9af7      gui=none      " magenta
  highlight nmSearchFrom         guifg=#7aa2f7      gui=none      " blue
  highlight nmSearchSubject      guifg=#c0caf5      gui=none      " normal fg
  highlight nmSearchTags         guifg=#7dcfff      gui=none      " cyan

  highlight Folded gui=none      guifg=#565f89      guibg=#1f2335
endfunction
