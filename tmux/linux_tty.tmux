set -g mode-style "fg=#dddddd,bg=#000000"

set -g message-style "fg=#dddddd,bg=#000000"
set -g message-command-style "fg=#dddddd,bg=#000000"

set -g pane-border-style "fg=#000000"
set -g pane-active-border-style "fg=#dddddd"

set -g popup-style "bg=#16161e,fg=#c0caf5"
set -g popup-border-style "bg=#16161e,fg=#c0caf5"
set -g popup-border-lines none

set -g status "on"
set -g status-justify "left"

set -g status-style "fg=#dddddd,bg=#000000"
set -g status-left-length "100"
set -g status-right-length "100"

set -g status-left-style NONE
set -g status-right-style NONE

set -g status-left "#[fg=#dddddd,bg=#000000,nobold] #S"
set -g status-right "#[fg=#dddddd,bg=#000000] #{prefix_highlight} #h"

setw -g window-status-activity-style "underscore"
setw -g window-status-separator ""
setw -g window-status-style "NONE,fg=#5A6288,bg=#000000"
setw -g window-status-format "#[fg=#000000,bg=#000000,nobold,nounderscore,noitalics]#[default]  #I:#W #F"
setw -g window-status-current-format "#[fg=#dddddd,bg=#000000,nobold,nounderscore,noitalics]  #I:#W #F"

set -g @prefix_highlight_output_prefix "#[fg=#aa5500]#[bg=#000000]#[fg=#aa5500]#[bg=#000000]"
