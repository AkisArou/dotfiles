set -g mode-style "fg=#569cd6,bg=#202328"

set -g message-style "fg=#569cd6,bg=#202328"
set -g message-command-style "fg=#569cd6,bg=#202328"

set -g pane-border-style "fg=#202328"
set -g pane-active-border-style "fg=#569cd6"

set -g status "on"
set -g status-justify "left"

set -g status-style "fg=#569cd6,bg=#202328"

set -g status-left-length "100"
set -g status-right-length "100"

set -g status-left-style NONE
set -g status-right-style NONE

set -g status-left "#[fg=#569cd6,bg=#202328,bold] #S #[fg=#569cd6,bg=#202328,nobold,nounderscore,noitalics]"
set -g status-right "#[fg=#202328,bg=#202328,nobold,nounderscore,noitalics]#[fg=#569cd6,bg=#202328] #{prefix_highlight} #[fg=#569cd6,bg=#202328,bold] #h"

setw -g window-status-activity-style "underscore,fg=#569cd6,bg=#202328"
setw -g window-status-separator ""
setw -g window-status-style "NONE,fg=#569cd6,bg=#202328"
setw -g window-status-format "#[fg=#1f2335,bg=#202328,nobold,nounderscore,noitalics]#[default] #I | #W #F #[fg=#202328,bg=#202328,nobold,nounderscore,noitalics]"
setw -g window-status-current-format "#[fg=#202328,bg=#202328,nobold,nounderscore,noitalics]#[fg=#569cd6,bg=#202328,bold] #I | #W #F #[fg=#202328,bg=#141414,nobold,nounderscore,noitalics]"

# tmux-plugins/tmux-prefix-highlight support
set -g @prefix_highlight_output_prefix "#[fg=#C586C0]#[bg=#202328]#[fg=#202328]#[bg=#C586C0]"
