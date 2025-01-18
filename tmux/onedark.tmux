set -g mode-style "fg=#61afef,bg=#282c34"

set -g message-style "fg=#61afef,bg=#282c34"
set -g message-command-style "fg=#61afef,bg=#282c34"

set -g pane-border-style "fg=#282c34"
set -g pane-active-border-style "fg=#61afef"

set -g status "on"
set -g status-justify "left"

set -g status-style "fg=#61afef,bg=#282c34"

set -g status-left-length "100"
set -g status-right-length "100"

set -g status-left-style NONE
set -g status-right-style NONE

set -g status-left "#[fg=#61afef,bg=#282c34,bold] #S #[fg=#61afef,bg=#282c34,nobold,nounderscore,noitalics]"
set -g status-right "#[fg=#282c34,bg=#282c34,nobold,nounderscore,noitalics]#[fg=#61afef,bg=#282c34] #{prefix_highlight} #[fg=#61afef,bg=#282c34,bold] #h"

setw -g window-status-activity-style "underscore,fg=#61afef,bg=#282c34"
setw -g window-status-separator ""
setw -g window-status-style "NONE,fg=#61afef,bg=#282c34"
setw -g window-status-format "#[fg=#1f2335,bg=#282c34,nobold,nounderscore,noitalics]#[default] #I | #W #F #[fg=#282c34,bg=#282c34,nobold,nounderscore,noitalics]"
setw -g window-status-current-format "#[fg=#282c34,bg=#282c34,nobold,nounderscore,noitalics]#[fg=#61afef,bg=#282c34,bold] #I | #W #F #[fg=#282c34,bg=#282c34,nobold,nounderscore,noitalics]"

# tmux-plugins/tmux-prefix-highlight support
set -g @prefix_highlight_output_prefix "#[fg=#C586C0]#[bg=#282c34]#[fg=#282c34]#[bg=#C586C0]"
