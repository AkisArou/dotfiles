" " Auto-mark messages as read when opened in notmuch-vim
"
" echo "EEEEEEEEEEE"
"
" " Only run when Notmuch is loaded
" augroup NotmuchAutoRead
"   autocmd!
"   " After the Notmuch-Vim plugin is loaded
"   autocmd User NotmuchLoaded call s:SetupAutoRead()
"   echo "AAAAAAAAAA"
" augroup END
"
" function! s:SetupAutoRead()
"   " Define a global function to mark messages as read
"   function! ShowAutoRead(thread_id)
"     call s:show(a:thread_id)
"     ruby << EOF
" $messages.each do |msg|
"   if msg.mail.tags.include?('unread')
"     do_tag('id:' + msg.message_id, '-unread')
"   end
" end
" EOF
"   endfunction
"
"   " Override <Enter> in show view
"   let g:notmuch_custom_show_maps = get(g:, 'notmuch_custom_show_maps', {})
"   let g:notmuch_custom_show_maps['<Enter>'] = 'ShowAutoRead(VIM::evaluate("get_thread_id"))'
" endfunction
"
