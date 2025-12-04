syntax region nmSearch		start=/^/ end=/$/		oneline contains=nmSearchDate
syntax match nmSearchDate	/^.\{-13}/			contained nextgroup=nmSearchNum
syntax match nmSearchNum	/.\{-4}/			contained nextgroup=nmSearchFrom
syntax match nmSearchFrom	/.\{-21}/			contained nextgroup=nmSearchSubject
syntax match nmSearchSubject	/.\{0,}\(([^()]\+)$\)\@=/	contained nextgroup=nmSearchTags
syntax match nmSearchTags	/.\+$/				contained
syntax match nmSearchUnread /.*\cunread\c.*/ containedin=nmSearch
syntax match nmSearchAttachment /\cattachment\c/ containedin=nmSearchTags

highlight link nmSearchDate Statement
highlight link nmSearchNum Type
highlight link nmSearchFrom Include
highlight link nmSearchSubject Normal
highlight link nmSearchTags String
highlight nmSearchUnread guibg=#000000 guifg=#0db9d7
highlight nmSearchAttachment guibg=#000000 guifg=#9ece6a
