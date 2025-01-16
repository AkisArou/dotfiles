nmap <Leader>f [fzf-p]
xmap <Leader>f [fzf-p]

let g:fzf_layout = { 'window': { 'width': 0.85, 'height': 0.9  }  }

 nnoremap <silent> <leader>ff :Files<CR>
 nnoremap <silent> <leader>fr :CocCommand fzf-preview.ProjectOldFiles<CR>
 nnoremap <silent> <leader>fs :Rg<CR>
 nnoremap <silent> <leader>fw :CocCommand fzf-preview.ProjectGrep <C-r><C-w><CR>
 nnoremap <silent> <leader>fW :CocCommand fzf-preview.ProjectGrep <C-r><C-a><CR>
 nnoremap <silent> <leader>fb :CocCommand fzf-preview.BufferLines<CR>
 nnoremap <silent> <leader>fc :CocCommand fzf-preview.GitActions<CR>
 nnoremap <silent> <leader>fd :CocCommand fzf-preview.NvimLspCurrentDiagnostics<CR>
 nnoremap <silent> <leader>fq :CocCommand fzf-preview.QuickFix<CR>
 nnoremap <silent> <leader>fe :Buffers<CR>
