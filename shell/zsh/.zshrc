if [[ -z "$TMUX" && "$TERM_PROGRAM" != "vscode" ]]; then
  tmux attach-session -t default || {
    "$HOME/dotfiles/scripts/start-tmux" && tmux attach-session -t default
  }
fi

# source
source <(fzf --zsh)
source "$HOME/dotfiles/shell/zsh/zinit"
source "$HOME/dotfiles/shell/common/aliases"
source "$HOME/dotfiles/shell/zsh/history"
source "$HOME/dotfiles/shell/zsh/opts"
source "$HOME/dotfiles/shell/zsh/vim-mode"
zsh-defer source "$HOME/dotfiles/shell/common/functions"
zsh-defer source "$HOME/dotfiles/shell/zsh/completions"
zsh-defer [ -f ~/.asdf/plugins/java/set-java-home.zsh ] && zsh-defer . ~/.asdf/plugins/java/set-java-home.zsh

zsh-defer export-lan-ip
