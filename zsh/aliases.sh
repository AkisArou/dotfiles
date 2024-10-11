alias sudo="sudo -E"
alias ta="tmux a"
alias v="nvim --listen /tmp/nvim.pipe"
alias r="yazi"
alias dir="dir --color=auto"
alias grep="grep --color=auto"
alias ip="ip -color=auto"
alias copy="xclip -sel clip"
alias grep="grep --color=auto"

# confirm before overwriting something
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"

# easier to read disk
alias df="df -h"     # human-readable sizes
alias free="free -m" # show sizes in MB

if command -v bat &>/dev/null; then
  alias bat="bat -pp --theme \"Visual Studio Dark+\""
  alias cat="bat -pp --theme \"Visual Studio Dark+\""
  alias catt="bat --theme \"Visual Studio Dark+\""
fi
