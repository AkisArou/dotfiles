alias sudo="sudo -E"
alias ta='tmux a'
alias v='nvim --listen /tmp/nvim.pipe'
alias r='yazi'
alias update-grub='sudo grub-mkconfig -o /boot/grub/grub.cfg'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='grep -F --color=auto'
alias egrep='grep -E --color=auto'
alias ip='ip -color=auto'

alias copy='xclip -sel clip'

# Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

# easier to read disk
alias df='df -h'     # human-readable sizes
alias free='free -m' # show sizes in MB

# get top process eating memory
alias psmem='ps auxf | sort -nr -k 4 | head -5'

# get top process eating cpu ##
alias pscpu='ps auxf | sort -nr -k 3 | head -5'

if command -v bat &>/dev/null; then
  alias bat="bat -pp --theme \"Visual Studio Dark+\""
  alias cat="bat -pp --theme \"Visual Studio Dark+\""
  alias catt="bat --theme \"Visual Studio Dark+\""
fi
