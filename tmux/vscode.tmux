set -g mode-style "bg=#264F78"

set -g message-style "fg=#AEAFAD,bg=#18191A"
set -g message-command-style "fg=#AEAFAD,bg=#18191A"

set -g pane-border-style "fg=#373737"
set -g pane-active-border-style "fg=#636369"

set -g popup-style "bg=#18191A,fg=#D4D4D4"
set -g popup-border-style "bg=#18191A,fg=#D4D4D4"
set -g popup-border-lines none

set -g status "on"
set -g status-justify "left"

set -g status-style "fg=#AEAFAD,bg=#18191A"
set -g status-left-length "100"
set -g status-right-length "100"

set -g status-left-style NONE
set -g status-right-style NONE

set -g status-left "#[fg=#AEAFAD,bg=#18191A,nobold] #S"
set -g status-right "#[fg=#AEAFAD,bg=#18191A] #{prefix_highlight} #h"

setw -g window-status-activity-style "underscore"
setw -g window-status-separator ""
setw -g window-status-style "NONE,fg=#808080,bg=#18191A"
setw -g window-status-format "#[fg=#18191A,bg=#18191A,nobold,nounderscore,noitalics]#[default]  #I:#W #F"
setw -g window-status-current-format "#[fg=#AEAFAD,bg=#18191A,nobold,nounderscore,noitalics]  #I:#W #F"

set -g @prefix_highlight_output_prefix "#[fg=#C586C0]#[bg=#18191A]#[fg=#C586C0]#[bg=#18191A]"
