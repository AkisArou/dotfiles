set -g mode-style "fg=#98c379,bg=#282c34"

set -g message-style "fg=#98c379,bg=#282c34"
set -g message-command-style "fg=#98c379,bg=#282c34"

set -g pane-border-style "fg=#282c34"
set -g pane-active-border-style "fg=#98c379"

set -g status "on"
set -g status-justify "left"

set -g status-style "fg=#98c379,bg=#282c34"

set -g status-left-length "100"
set -g status-right-length "100"

set -g status-left-style NONE
set -g status-right-style NONE

set -g status-left "#[fg=#98c379,bg=#282c34,bold] #S #[fg=#98c379,bg=#282c34,nobold,nounderscore,noitalics]"
set -g status-right "#[fg=#282c34,bg=#282c34,nobold,nounderscore,noitalics]#[fg=#98c379,bg=#282c34] #{prefix_highlight} #[fg=#98c379,bg=#282c34,bold] #h"

setw -g window-status-separator ""
# setw -g window-status-style "NONE,fg=#98c379,bg=#282c34"
# setw -g window-status-format "#[fg=#1f2335,bg=#282c34,nobold,nounderscore,noitalics]#[default] #I | #W #F #[fg=#282c34,bg=#282c34,nobold,nounderscore,noitalics]"
# setw -g window-status-current-format "#[fg=#282c34,bg=#282c34,nobold,nounderscore,noitalics]#[fg=#98c379,bg=#282c34,bold] #I | #W #F #[fg=#282c34,bg=#282c34,nobold,nounderscore,noitalics]"

# Uniform styling for all inactive windows
setw -g window-status-style "bg=#3c4048,fg=#98c379"
setw -g window-status-format "#[fg=#1f2335,bg=#3c4048,nobold,nounderscore,noitalics]#[default] #I | #W #F #[fg=#3c4048,bg=#3c4048,nobold,nounderscore,noitalics]"

setw -g window-status-activity-style "bg=#3c4048,fg=#98c379"

# Uniform styling for the active window
setw -g window-status-current-style "bg=#282c34,fg=#98c379"
setw -g window-status-current-format "#[fg=#282c34,bg=#282c34,nobold,nounderscore,noitalics]#[fg=#98c379,bg=#282c34,bold] #I | #W #F #[fg=#282c34,bg=#282c34,nobold,nounderscore,noitalics]"

set -g @prefix_highlight_output_prefix "#[fg=#C586C0]#[bg=#282c34]#[fg=#282c34]#[bg=#C586C0]"
