let s:vim_suggest = {}

let s:vim_suggest.cmd = {
    \ 'enable': v:true,
    \ 'pum': v:true,
    \ 'exclude': [],
    \ 'onspace': ['b\%[uffer]','colo\%[rscheme]'],
    \ 'alwayson': v:true,
    \ 'popupattrs': {},
    \ 'wildignore': v:true,
    \ 'addons': v:true,
    \ 'trigger': 'n',
    \ 'reverse': v:false,
    \ 'prefixlen': 1,
\ }

let s:vim_suggest.search = {
    \ 'enable': v:true,
    \ 'pum': v:true,
    \ 'fuzzy': v:true,
    \ 'alwayson': v:true,
    \ 'popupattrs': {
    \   'maxheight': 12
    \ },
    \ 'range': 100,
    \ 'timeout': 200,
    \ 'async': v:true,
    \ 'async_timeout': 3000,
    \ 'async_minlines': 1000,
    \ 'highlight': v:true,
    \ 'trigger': 'n',
    \ 'prefixlen': 1,
\ }

autocmd VimEnter * call g:VimSuggestSetOptions(s:vim_suggest)
