set -g mode-style "fg=#d3869b,bg=#282828"

set -g message-style "fg=#d3869b,bg=#282828"
set -g message-command-style "fg=#d3869b,bg=#282828"

set -g pane-border-style "fg=#3c3836"
set -g pane-active-border-style "fg=#d3869b"

set -g status "on"
set -g status-justify "left"

set -g status-style "fg=#d4be98,bg=#282828"

set -g status-left-length "100"
set -g status-right-length "100"

set -g status-left-style NONE
set -g status-right-style NONE

set -g status-left "#[fg=#d3869b,bg=#282828,bold] #S #[fg=#d4be98,bg=#282828,nobold,nounderscore,noitalics]"
set -g status-right "#[fg=#282828,bg=#282828,nobold,nounderscore,noitalics]#[fg=#d3869b,bg=#282828] #{prefix_highlight} #[fg=#d3869b,bg=#282828,bold] #h"

setw -g window-status-activity-style "underscore,fg=#d3869b,bg=#282828"
setw -g window-status-separator ""
setw -g window-status-style "NONE,fg=#a89984,bg=#282828"
setw -g window-status-format "#[fg=#3c3836,bg=#282828,nobold,nounderscore,noitalics]#[default] #I | #W #F #[fg=#282828,bg=#282828,nobold,nounderscore,noitalics]"
setw -g window-status-current-format "#[fg=#282828,bg=#282828,nobold,nounderscore,noitalics]#[fg=#d3869b,bg=#282828,bold] #I | #W #F #[fg=#282828,bg=#282828,nobold,nounderscore,noitalics]"

# tmux-plugins/tmux-prefix-highlight support
set -g @prefix_highlight_output_prefix "#[fg=#d3869b]#[bg=#282828]#[fg=#282828]#[bg=#d3869b]"
