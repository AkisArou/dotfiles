typeset -g SHELL_SENSE_PTY_NAME=
typeset -g SHELL_SENSE_PTY_OUTPUT=
typeset -g SHELL_SENSE_PTY_CHUNK=

shell_sense_pty_start() {
  local name=$1
  shift
  SHELL_SENSE_PTY_NAME=$name
  SHELL_SENSE_PTY_OUTPUT=
  SHELL_SENSE_PTY_CHUNK=
  zpty "$name" "$@"
}

shell_sense_pty_reset() {
  SHELL_SENSE_PTY_OUTPUT=
  SHELL_SENSE_PTY_CHUNK=
}

shell_sense_pty_read_until() {
  local pattern=$1
  local -i attempts=${2:-500}
  local -i attempt
  local plain_output
  for (( attempt = 1; attempt <= attempts; attempt++ )); do
    while zpty -r -t "$SHELL_SENSE_PTY_NAME" SHELL_SENSE_PTY_CHUNK 2>/dev/null; do
      SHELL_SENSE_PTY_OUTPUT+=$SHELL_SENSE_PTY_CHUNK
      SHELL_SENSE_PTY_CHUNK=
    done
    plain_output=${SHELL_SENSE_PTY_OUTPUT//$'\e'\[[0-9;]#[[:alpha:]]/}
    [[ $plain_output == ${~pattern} ]] && return 0
    zselect -t 1 >/dev/null 2>&1 || true
  done
  return 1
}

shell_sense_pty_write_line() {
  zpty -w "$SHELL_SENSE_PTY_NAME" "$1"
}

shell_sense_pty_write_raw() {
  zpty -n -w "$SHELL_SENSE_PTY_NAME" "$1"
}

shell_sense_pty_close() {
  [[ -z $SHELL_SENSE_PTY_NAME ]] || zpty -d "$SHELL_SENSE_PTY_NAME" 2>/dev/null || true
  SHELL_SENSE_PTY_NAME=
}
