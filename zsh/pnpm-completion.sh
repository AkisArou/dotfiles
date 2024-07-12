_fzf_complete_pnpm() {
  _fzf_complete --multi --reverse --prompt="pnpm run> " -- "$@" < <(
    cat package.json | jq -r '.scripts | keys[]'
  )
}
