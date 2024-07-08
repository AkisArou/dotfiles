fastfetch
[ -f "$HOME/.local/share/zap/zap.zsh" ] && source "$HOME/.local/share/zap/zap.zsh"

# plugins
plug "zsh-users/zsh-autosuggestions"
plug "zap-zsh/supercharge"
plug "zap-zsh/vim"
plug "zap-zsh/zap-prompt"
plug "zap-zsh/fzf"
plug "zap-zsh/exa"
plug "zsh-users/zsh-syntax-highlighting"

# keybinds
bindkey '^ ' autosuggest-accept

# completions
source "$HOME/dotfiles/zsh/npm-completion.sh"
source "$HOME/dotfiles/zsh/docker-completion.sh"
source "$HOME/dotfiles/zsh/asdf.sh"
source /usr/share/zsh/plugins/pnpm-shell-completion/pnpm-shell-completion.zsh

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

# source
source "$HOME/dotfiles/zsh/aliases.zsh"
source "$HOME/dotfiles/zsh/exports.zsh"
source "$HOME/dotfiles/zsh/functions.zsh"
source "$HOME/dotfiles/zsh/history.zsh"
source "$HOME/dotfiles/scripts/add-ssh-key-to-agent.sh"
