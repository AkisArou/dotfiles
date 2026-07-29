autoload -Uz compinit
compinit -u -D

_shell_sense_blink_completion() {
  local -a descriptions=(
    '--all — stage modified and deleted files'
    '--amend — replace the previous commit'
  )
  compadd -J options -X 'command options' -d descriptions -- --all --amend
}
compdef _shell_sense_blink_completion blink-test

blink-test() {
  print -r -- "<BLINK-EXEC>$*</BLINK-EXEC>"
}

PS1='BLINK> '
RPS1=

source "$SHELL_SENSE_TEST_ROOT/shell/zsh/shell-sense.plugin.zsh"
print -r -- '<BLINK-SHELL-READY/>'
