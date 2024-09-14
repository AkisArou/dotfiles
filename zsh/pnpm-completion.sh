source /usr/share/zsh/plugins/pnpm-shell-completion/pnpm-shell-completion.zsh

_fzf_complete_pnpm() {
  _fzf_complete --multi --reverse --prompt="pnpm run> " -- "$@" < <(
    cat package.json | jq -r '.scripts | keys[]'
  )
}
