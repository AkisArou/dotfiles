set -g mode-style "bg=#1a1a1a"

set -g message-style "fg=#999999,bg=#0c0c0c"
set -g message-command-style "fg=#999999,bg=#0c0c0c"

set -g pane-border-style "fg=#0c0c0c"
set -g pane-active-border-style "fg=#999999"

set -g popup-style "bg=#111111,fg=#999999"
set -g popup-border-style "bg=#111111,fg=#999999"
set -g popup-border-lines none

set -g status "on"
set -g status-justify "left"

set -g status-style "fg=#999999,bg=#0c0c0c"
set -g status-left-length "100"
set -g status-right-length "100"

set -g status-left-style NONE
set -g status-right-style NONE

set -g status-left "#[fg=#999999,bg=#0c0c0c,nobold] #S"
set -g status-right "#[fg=#999999,bg=#0c0c0c] #{prefix_highlight} #h"

setw -g window-status-activity-style "underscore"
setw -g window-status-separator ""
setw -g window-status-style "NONE,fg=#555555,bg=#0c0c0c"
setw -g window-status-format "#[fg=#0c0c0c,bg=#0c0c0c,nobold,nounderscore,noitalics]#[default]  #I:#W #F"
setw -g window-status-current-format "#[fg=#999999,bg=#0c0c0c,nobold,nounderscore,noitalics]  #I:#W #F"

set -g @prefix_highlight_output_prefix "#[fg=##9d7cd8]#[bg=#0c0c0c]#[fg=##9d7cd8]#[bg=#0c0c0c]"
