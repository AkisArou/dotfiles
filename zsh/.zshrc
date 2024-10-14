if [[ -z "$TMUX" ]]; then
  tmux attach-session -t default || tmux new-session -s default
fi

#figlet "Arch Linux" &&
echo ""
fastfetch -l none --color blue -s "Title:OS:Kernel:Host:Terminal:Uptime:Battery"

export STARSHIP_CONFIG=~/.config/starship/starship.toml
eval "$(starship init zsh)"

[ -f "$HOME/.local/share/zap/zap.zsh" ] && source "$HOME/.local/share/zap/zap.zsh"

# plugins
plug "zsh-users/zsh-autosuggestions"
plug "zap-zsh/supercharge"
plug "zap-zsh/vim"
plug "zap-zsh/fzf"
plug "zsh-users/zsh-syntax-highlighting"

# keybinds
bindkey '^ ' autosuggest-accept

# source
source "$HOME/dotfiles/zsh/aliases.sh"
source "$HOME/dotfiles/zsh/exports.sh"
source "$HOME/dotfiles/zsh/functions.sh"
source "$HOME/dotfiles/zsh/history.sh"

# completions
source "$HOME/dotfiles/zsh/npm-completion.sh"
source "$HOME/dotfiles/zsh/pnpm-completion.sh"
source "$HOME/dotfiles/zsh/docker-completion.sh"
source "$HOME/dotfiles/zsh/asdf.sh"

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"
