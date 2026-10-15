if [[ -z "$TMUX" && "$TERM_PROGRAM" != "vscode" && -z "$NO_TMUX_AUTO_ATTACH" ]]; then
  "$HOME/.config/tmux/start"
  tmux attach-session -t '=default'
fi

_dotfiles_p10k_instant_prompt="${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
[[ -r $_dotfiles_p10k_instant_prompt ]] && source "$_dotfiles_p10k_instant_prompt"
unset _dotfiles_p10k_instant_prompt

source "$HOME/.config/shell/common/fzf"
source <(fzf --zsh)
source "$HOME/.config/shell/zsh/completions"
source "$HOME/.config/shell/common/aliases"
source "$HOME/.config/shell/zsh/history"
source "$HOME/.config/shell/zsh/opts"
source "$HOME/.config/shell/zsh/vim-mode"
source "$HOME/.config/shell/zsh/p10k-focus-refresh"
source "$HOME/.config/shell/common/functions"
source "$HOME/.config/shell/zsh/zinit"
source "$HOME/.config/shell/zsh/.p10k.zsh"

_dotfiles_update_java_home() {
  local java_path

  java_path=$(asdf which java 2>/dev/null) || return

  export JAVA_HOME=${java_path:A:h:h}
  export JDK_HOME=$JAVA_HOME
}

if (($+commands[asdf])); then
  autoload -Uz add-zsh-hook
  add-zsh-hook chpwd _dotfiles_update_java_home
  _dotfiles_update_java_home
fi

() {
  local theme_file="${XDG_CONFIG_HOME:-$HOME/.config}/fsh/vscode.ini"
  local compiled_theme="$FAST_WORK_DIR/current_theme.zsh"

  if [[ ! -r $theme_file || $FAST_THEME_NAME != vscode ||
    ! -r $compiled_theme || $theme_file -nt $compiled_theme ]]; then
    fast-theme -q XDG:vscode
  fi
}
