fastfetch && echo

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh


if [[ -z "$TMUX" && "$TERM_PROGRAM" != "vscode" ]]; then
  tmux attach-session -t default || tmux new-session -s default
fi


if [ -f "$HOME/.local/share/zap/zap.zsh" ]; then
  source "$HOME/.local/share/zap/zap.zsh"

  # plugins
  plug "zap-zsh/supercharge"
  ZVM_INIT_MODE=sourcing
  plug "jeffreytse/zsh-vi-mode"
  plug "zsh-users/zsh-autosuggestions"
  plug "zsh-users/zsh-syntax-highlighting"
  plug "Aloxaf/fzf-tab"
  plug "zap-zsh/fzf"
  plug "romkatv/powerlevel10k"
fi

# keybinds
bindkey '^E' autosuggest-accept
bindkey -M menuselect '^N' down-line-or-history
bindkey -M menuselect '^P' up-line-or-history

# source
source "$HOME/dotfiles/shell/common/aliases"
source "$HOME/dotfiles/shell/common/functions"
source "$HOME/dotfiles/shell/zsh/history"
[ -f ~/.asdf/plugins/java/set-java-home.zsh ] && . ~/.asdf/plugins/java/set-java-home.zsh

# completions
source /usr/share/zsh/plugins/pnpm-shell-completion/pnpm-shell-completion.zsh
source "$HOME/dotfiles/scripts/try-source-completions"
fpath=(${ASDF_DATA_DIR:-$HOME/.asdf}/completions $fpath)
autoload -Uz compinit && compinit

