_shell_sense_test_dump() {
  {
    printf 'LINE=%q\n' "$COMP_LINE"
    printf 'POINT=%q\n' "$COMP_POINT"
    printf 'CWORD=%q\n' "$COMP_CWORD"
    printf 'WORDS='
    printf '<%q>' "${COMP_WORDS[@]}"
    printf '\n'
  } >>"$SHELL_SENSE_TEST_DUMP"
  COMPREPLY=()
}
complete -F _shell_sense_test_dump shell-sense-test
PS1='PROMPT> '
