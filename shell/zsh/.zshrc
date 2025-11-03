eval "$(oh-my-posh init zsh --config "$HOME"/dotfiles/shell/zsh/oh-my-posh.json)"

if [[ -z "$TMUX" && "$TERM_PROGRAM" != "vscode" && -z "$NO_TMUX_AUTO_ATTACH" ]]; then
  tmux attach-session -t default || {
    "$HOME/dotfiles/scripts/start-tmux" && tmux attach-session -t default
  }
fi

# source
source <(fzf --zsh)
source "$HOME/dotfiles/shell/zsh/completions"
source "$HOME/dotfiles/shell/zsh/zinit"
source "$HOME/dotfiles/shell/common/aliases"
source "$HOME/dotfiles/shell/zsh/history"
source "$HOME/dotfiles/shell/zsh/opts"
source "$HOME/dotfiles/shell/zsh/vim-mode"
source "$HOME/dotfiles/shell/common/functions"
[ -f ~/.asdf/plugins/java/set-java-home.zsh ] && . ~/.asdf/plugins/java/set-java-home.zsh
