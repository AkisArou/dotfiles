set -g mode-style "fg=#7aa2f7,bg=#1a1b26"

set -g message-style "fg=#7aa2f7,bg=#1a1b26"
set -g message-command-style "fg=#7aa2f7,bg=#1a1b26"

set -g pane-border-style "fg=#1a1b26"
set -g pane-active-border-style "fg=#7aa2f7"

set -g popup-border-style 'fg=#3b4261'
set -g popup-border-lines rounded

set -g status "on"
set -g status-justify "left"

set -g status-style "fg=#7aa2f7,bg=#1a1b26"
set -g status-left-length "100"
set -g status-right-length "100"

set -g status-left-style NONE
set -g status-right-style NONE

set -g status-left "#[fg=#7aa2f7,bg=#1a1b26,nobold] #S #[fg=#7aa2f7,bg=#1a1b26,nobold,nounderscore,noitalics]"
set -g status-right "#[fg=#1a1b26,bg=#1a1b26,nobold,nounderscore,noitalics]#[fg=#7aa2f7,bg=#1a1b26] #{prefix_highlight} #[fg=#1a1b26,bg=#1a1b26,nobold,nounderscore,noitalics]#[fg=#7aa2f7,bg=#1a1b26] %Y-%m-%d  %H:%M #[fg=#7aa2f7,bg=#1a1b26,nobold,nounderscore,noitalics]#[fg=#7aa2f7,bg=#1a1b26,nobold] #h "

setw -g window-status-activity-style "underscore,fg=#a9b1d6,bg=#1a1b26"
setw -g window-status-separator ""
setw -g window-status-style "NONE,fg=#7881aa,bg=#1a1b26"
setw -g window-status-format "#[fg=#1a1b26,bg=#1a1b26,nobold,nounderscore,noitalics]#[default] #I  #W #F #[fg=#1a1b26,bg=#1a1b26,nobold,nounderscore,noitalics]"
setw -g window-status-current-format "#[fg=#1a1b26,bg=#1a1b26,nobold,nounderscore,noitalics]#[fg=#7aa2f7,bg=#1a1b26,nobold] #I  #W #F #[fg=#1a1b26,bg=#1a1b26,nobold,nounderscore,noitalics]"

set -g @prefix_highlight_output_prefix "#[fg=#e0af68]#[bg=#1a1b26]#[fg=#e0af68]#[bg=#1a1b26]"
