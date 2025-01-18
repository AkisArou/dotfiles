set -g mode-style "fg=#61afef,bg=#313640"

set -g message-style "fg=#61afef,bg=#313640"
set -g message-command-style "fg=#61afef,bg=#313640"

set -g pane-border-style "fg=#313640"
set -g pane-active-border-style "fg=#61afef"

set -g status "on"
set -g status-justify "left"

set -g status-style "fg=#61afef,bg=#313640"

set -g status-left-length "100"
set -g status-right-length "100"

set -g status-left-style NONE
set -g status-right-style NONE

set -g status-left "#[fg=#61afef,bg=#313640,bold] #S #[fg=#61afef,bg=#313640,nobold,nounderscore,noitalics]"
set -g status-right "#[fg=#313640,bg=#313640,nobold,nounderscore,noitalics]#[fg=#61afef,bg=#313640] #{prefix_highlight} #[fg=#61afef,bg=#313640,bold] #h"

setw -g window-status-activity-style "underscore,fg=#61afef,bg=#313640"
setw -g window-status-separator ""
setw -g window-status-style "NONE,fg=#61afef,bg=#313640"
setw -g window-status-format "#[fg=#1f2335,bg=#313640,nobold,nounderscore,noitalics]#[default] #I | #W #F #[fg=#313640,bg=#313640,nobold,nounderscore,noitalics]"
setw -g window-status-current-format "#[fg=#313640,bg=#313640,nobold,nounderscore,noitalics]#[fg=#61afef,bg=#313640,bold] #I | #W #F #[fg=#313640,bg=#313640,nobold,nounderscore,noitalics]"

# tmux-plugins/tmux-prefix-highlight support
set -g @prefix_highlight_output_prefix "#[fg=#C586C0]#[bg=#313640]#[fg=#313640]#[bg=#C586C0]"
