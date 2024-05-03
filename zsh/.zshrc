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

# asdf
. "$HOME/.asdf/asdf.sh"

# append completions to fpath
fpath=(${ASDF_DIR}/completions $fpath)
# initialise completions with ZSH's compinit
autoload -Uz compinit && compinit


source "$HOME/dotfiles/zsh/npm-completion.sh"
source "$HOME/dotfiles/zsh/docker-completion.sh"

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

# source
source "$HOME/dotfiles/zsh/.config/zsh/aliases.zsh"
source "$HOME/dotfiles/zsh/.config/zsh/exports.zsh"
source "$HOME/dotfiles/zsh/.config/zsh/functions.zsh"
source "$HOME/dotfiles/zsh/.config/zsh/history.zsh"
source "$HOME/dotfiles/scripts/add-ssh-key-to-agent.sh"
