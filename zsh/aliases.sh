alias sudo="sudo -E"
alias ta="tmux attach"
alias vim="TERM=xterm-256color vim"
alias v='nvim'
alias dir="dir --color=auto"
alias grep="grep --color=auto"
alias ip="ip -color=auto"
alias copy="xclip -sel clip"
alias grep="grep --color=auto"

alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"

alias df="df -h"
alias free="free -m"

alias cat="bat"

alias ls='eza --group-directories-first --icons=never'
alias ll='ls -lh --git'

alias la='ll -a'
alias tree='ll --tree --level=2'

alias update_all="paru -Syu && echo && ~/dotfiles/scripts/build-nvim.sh"
