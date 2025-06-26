if [[ -z "$TMUX" && "$TERM_PROGRAM" != "vscode" ]]; then
  tmux attach-session -t default || tmux new-session -s default
fi

fastfetch; echo;

# source
source "$HOME/dotfiles/shell/zsh/zinit"
source "$HOME/dotfiles/shell/common/aliases"
source "$HOME/dotfiles/shell/common/functions"
source "$HOME/dotfiles/shell/zsh/history"
source "$HOME/dotfiles/shell/zsh/opts"
[ -f ~/.asdf/plugins/java/set-java-home.zsh ] && zsh-defer . ~/.asdf/plugins/java/set-java-home.zsh

# completions
autoload -Uz compinit 
if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]]; then
	compinit;
else
	compinit -C;
fi;

fpath=(${ASDF_DATA_DIR:-$HOME/.asdf}/completions $fpath)
zsh-defer source /usr/share/zsh/plugins/pnpm-shell-completion/pnpm-shell-completion.zsh
zsh-defer source "$HOME/dotfiles/scripts/try-source-completions"
