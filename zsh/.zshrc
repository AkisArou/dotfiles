source "$HOME/dotfiles/zsh/exports.sh"

if [[ -z "$TMUX" && "$TERM_PROGRAM" != "vscode" ]]; then
  tmux attach-session -t default || tmux new-session -s default
fi

fastfetch

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
bindkey -M menuselect '^N' down-line-or-history
bindkey -M menuselect '^P' up-line-or-history

# source
source "$HOME/dotfiles/zsh/aliases.sh"
source "$HOME/dotfiles/zsh/functions.sh"
source "$HOME/dotfiles/zsh/history.sh"

# completions
source /usr/share/zsh/plugins/pnpm-shell-completion/pnpm-shell-completion.zsh
source "$HOME/dotfiles/scripts/try-source-completions.sh"
fpath=(${ASDF_DATA_DIR:-$HOME/.asdf}/completions $fpath)
autoload -Uz compinit && compinit

_fzf_complete_pnpm() {
  _fzf_complete --multi --reverse --prompt="pnpm run> " -- "$@" < <(
    cat package.json | jq -r '.scripts | keys[]'
  )
}
