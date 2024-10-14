alias sudo="sudo -E"
alias ta="tmux a"
alias v="nvim"
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

# Create alias override commands using 'eza'
alias ls='eza --group-directories-first --icons=never'

# Use the --git flag if the installed version of eza supports git
# Related to https://github.com/ogham/exa/issues/978
if eza --version | grep -q '+git'; then
  alias ll='ls -lh --git'
else
  alias ll='ls -lh'
fi

alias la='ll -a'
alias tree='ll --tree --level=2'
