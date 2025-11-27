if [[ -z "$TMUX" && "$TERM_PROGRAM" != "vscode" && -z "$NO_TMUX_AUTO_ATTACH" ]]; then
  tmux attach-session -t default || {
    "$HOME/dotfiles/scripts/start-tmux" && tmux attach-session -t default
  }
fi

source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"

source <(fzf --zsh)
source "$HOME/dotfiles/shell/zsh/completions"
source "$HOME/dotfiles/shell/zsh/zinit"
source "$HOME/dotfiles/shell/common/aliases"
source "$HOME/dotfiles/shell/zsh/history"
source "$HOME/dotfiles/shell/zsh/opts"
source "$HOME/dotfiles/shell/zsh/vim-mode"
source "$HOME/dotfiles/shell/common/functions"
source "$HOME/dotfiles/shell/zsh/.p10k.zsh"
[ -f "$HOME/.asdf/plugins/java/set-java-home.zsh" ] && . "$HOME/.asdf/plugins/java/set-java-home.zsh"

fast-theme XDG:tokyonight &>/dev/null
