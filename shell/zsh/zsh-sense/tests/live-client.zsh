#!/usr/bin/env zsh

emulate -L zsh
setopt errexit nounset pipefail no_aliases extendedglob

zmodload zsh/zpty zsh/zselect zsh/datetime
typeset -gx SENSE_ZSH_TEST_ROOT=${0:A:h:h}
typeset -g SENSE_ZSH_TEST_TEMP
SENSE_ZSH_TEST_TEMP=$(mktemp -d)
typeset -gx XDG_RUNTIME_DIR="$SENSE_ZSH_TEST_TEMP/runtime"
typeset -gx XDG_STATE_HOME="$SENSE_ZSH_TEST_TEMP/state"
typeset -gx SENSE_ZSH_SOCKET="$SENSE_ZSH_TEST_TEMP/daemon.sock"
typeset -gx SENSE_ZSH_CONFIG="$SENSE_ZSH_TEST_ROOT/config.example.toml"
typeset -gx SENSE_ZSH_COMMAND="$SENSE_ZSH_TEST_ROOT/target/debug/zsh-sense"
typeset -gx SENSE_ZSH_NO_DAEMON_AUTOSTART=1
typeset -gx SENSE_ZSH_TEST_WORK="$SENSE_ZSH_TEST_TEMP/work"
typeset -gx TERM=xterm-256color
command mkdir -m 700 -- "$XDG_RUNTIME_DIR" "$XDG_STATE_HOME"
command mkdir -p -- "$SENSE_ZSH_TEST_WORK/dotfiles/nvim"

typeset -gi daemon_pid=0
typeset output= chunk=
read_until() {
  local pattern=$1
  local -i attempts=${2:-500}
  local -i attempt
  local plain_output
  for (( attempt = 1; attempt <= attempts; attempt++ )); do
    while zpty -r -t sense-live chunk 2>/dev/null; do
      output+=$chunk
      chunk=
    done
    plain_output=${output//$'\e'\[[0-9;]#[[:alpha:]]/}
    [[ $plain_output == ${~pattern} ]] && return 0
    zselect -t 1 >/dev/null 2>&1 || true
  done
  return 1
}

cleanup() {
  zpty -d sense-live 2>/dev/null || true
  if (( daemon_pid )); then
    kill -INT $daemon_pid 2>/dev/null || true
    wait $daemon_pid 2>/dev/null || true
  fi
  [[ ${SENSE_ZSH_KEEP_TEST_TEMP:-0} == 1 ]] || command rm -rf -- "$SENSE_ZSH_TEST_TEMP"
}
trap cleanup EXIT

"$SENSE_ZSH_COMMAND" daemon --socket "$SENSE_ZSH_SOCKET" \
  --config "$SENSE_ZSH_CONFIG" >"$SENSE_ZSH_TEST_TEMP/daemon.log" 2>&1 &
daemon_pid=$!
for _ in {1..200}; do
  [[ -S $SENSE_ZSH_SOCKET ]] && break
  zselect -t 1 >/dev/null 2>&1 || true
done
[[ -S $SENSE_ZSH_SOCKET ]] || {
  print -u2 -- 'daemon socket did not appear'
  command cat -- "$SENSE_ZSH_TEST_TEMP/daemon.log" >&2
  return 1
}

zpty sense-live zsh -f
output=
read_until '*%*' || {
  print -u2 -- 'initial Zsh prompt did not appear'
  return 1
}
zpty -w sense-live ". ${(q)SENSE_ZSH_TEST_ROOT}/tests/fixtures/live-client-init.zsh"
output=
read_until '*<SENSE-PROMPT>*' || {
  print -u2 -- 'live client initialization did not finish'
  return 1
}

# Allow the worker's ready/config stream to install ZLE hooks and keybindings.
zselect -t 20 >/dev/null 2>&1 || true

# A one-character command query can expose thousands of executable names.
# Continuous completion must never monopolize ZLE long enough to delay the
# next ordinary key, and Space/Backspace must retain their editor semantics.
zpty -n -w sense-live c
# Cross the configured 15 ms debounce boundary so this specifically probes a
# key arriving while the one-character command capture is active.
zselect -t 4 >/dev/null 2>&1 || true
typeset -F responsiveness_started=$EPOCHREALTIME
zpty -n -w sense-live $'d\x18\x07'
output=
read_until '*buffer="cd"*</STATE>*<SENSE-PROMPT>*' 200 || {
  print -u2 -- 'the next typed character stalled behind command completion'
  print -u2 -r -- "$output"
  return 1
}
typeset -F responsiveness_elapsed=$(( EPOCHREALTIME - responsiveness_started ))
(( responsiveness_elapsed < 0.10 )) || {
  print -u2 -- "the next key took ${responsiveness_elapsed}s; completion blocked ZLE"
  return 1
}
[[ ${SENSE_ZSH_REPORT_TIMINGS:-0} == 1 ]] &&
  print -r -- "command-preemption-ms=$(( responsiveness_elapsed * 1000.0 ))"

# Space and Backspace retain their ordinary editor semantics too.
zpty -n -w sense-live $'cd x\x7f\x18\x07'
output=
read_until '*buffer="cd "*</STATE>*<SENSE-PROMPT>*' 200 || {
  print -u2 -- 'ordinary Space/Backspace editing stalled or produced the wrong buffer'
  print -u2 -r -- "$output"
  return 1
}

zpty -n -w sense-live 'sense-test --a'
output=
read_until '*replace the previous commit*' || {
  print -u2 -- 'continuous popup did not include completion descriptions'
  print -u2 -r -- "$output"
  typeset -a worker_logs=( $XDG_STATE_HOME/zsh-sense/worker-*.log(N) )
  (( $#worker_logs )) && command tail -100 -- "${worker_logs[@]}" >&2
  command cat -- "$SENSE_ZSH_TEST_TEMP/daemon.log" >&2
  zpty -n -w sense-live $'\x18\x07'
  output=
  read_until '*<STATE>ready=[01]*</STATE>*<SENSE-PROMPT>*' 100 || true
  print -u2 -r -- "$output"
  return 1
}

typeset plain_output
plain_output=$(print -rn -- "$output" | sed $'s/\033\\[[0-9;]*[[:alpha:]]//g')
for expected in '--all' '--amend' 'stage modified and deleted files'; do
  [[ $plain_output == *$expected* ]] || {
    print -u2 -- "popup is missing: $expected"
    print -u2 -r -- "$output"
    return 1
  }
done
for expected in \
    $'\e[48;2;32;32;32m' \
    $'\e[48;2;52;59;65m' \
    $'\e[38;2;24;162;254m' \
    $'\e[38;2;255;214;2m'; do
  [[ $output == *$expected* ]] || {
    print -u2 -- 'popup is missing a configured BlinkCmp highlight'
    print -u2 -r -- "$output"
    return 1
  }
done
[[ $output != *completions* ]] || {
  print -u2 -- 'popup still contains the completions title'
  print -u2 -r -- "$output"
  return 1
}
[[ $output != *'^[[38;'* ]] || {
  print -u2 -- 'popup contains literal ANSI escape text'
  return 1
}
typeset escaped_meta='\M-'
[[ $output != *$escaped_meta* ]] || {
  print -u2 -- 'popup contains locale-escaped UTF-8 bytes'
  print -u2 -r -- "$output"
  return 1
}

# Ctrl-N selects --amend; Ctrl-E accepts through the captured compadd record.
zpty -n -w sense-live $'\x0e\x05\r'
output=
read_until '*<EXEC>--amend</EXEC>*<FINISH-POST>0</FINISH-POST>*<SENSE-PROMPT>*' || {
  print -u2 -- 'navigation and Zsh-owned acceptance did not complete'
  print -u2 -r -- "$output"
  zpty -n -w sense-live $'\x18\x07'
  output=
  read_until '*<STATE>*</STATE>*<SENSE-PROMPT>*' 100 || true
  print -u2 -r -- "$output"
  return 1
}

# Enter can execute the buffer without first accepting a candidate. The popup
# must still be removed during line-finish rather than becoming scrollback.
zpty -n -w sense-live 'sense-test --a'
output=
read_until '*replace the previous commit*' || {
  print -u2 -- 'line-finish cleanup setup did not open the popup'
  return 1
}
zpty -n -w sense-live $'\r'
output=
read_until '*<EXEC>--a</EXEC>*<FINISH-POST>0</FINISH-POST>*<SENSE-PROMPT>*' || {
  print -u2 -- 'line-finish did not clear POSTDISPLAY before execution'
  print -u2 -r -- "$output"
  return 1
}

# Frizbee sees the unfiltered Zsh candidate universe, so a missing character
# can still select the semantically valid completion.
zpty -n -w sense-live 'sense-verb rstart'
output=
read_until '*restart one or more units*' || {
  print -u2 -- 'fuzzy typo completion did not rank restart'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x05\r'
output=
read_until '*<VERB>restart</VERB>*<SENSE-PROMPT>*' || return 1

# Ctrl-E also accepts the command-name fast path, whose compact transport and
# insertion replay are intentionally separate from generic compadd capture.
zpty -n -w sense-live 'sense-ver'
output=
read_until '*shell function*' || {
  print -u2 -- 'fast command completion did not open'
  return 1
}
zpty -n -w sense-live $'\x05\r'
output=
read_until '*<VERB></VERB>*<SENSE-PROMPT>*' || {
  print -u2 -- 'Ctrl-E did not accept the fast command candidate'
  print -u2 -r -- "$output"
  return 1
}

# Manual mode remains first-class: Tab opens a closed popup, then Ctrl-E
# accepts it. This mutation is confined to the PTY fixture.
zpty -w sense-live '_zsh_sense_activation_mode=manual'
output=
read_until '*<SENSE-PROMPT>*' || return 1
zpty -n -w sense-live $'sense-test --a\t'
output=
read_until '*replace the previous commit*' || {
  print -u2 -- 'manual Tab trigger did not open the popup'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x05\r'
output=
read_until '*<EXEC>--all</EXEC>*<SENSE-PROMPT>*' || {
  print -u2 -- 'manual popup acceptance failed'
  print -u2 -r -- "$output"
  return 1
}

# Page-down moves by the configured popup height (10 rows).
zpty -n -w sense-live $'sense-many --option\t'
output=
read_until '*candidate number 10*' || {
  print -u2 -- 'paged completion popup did not open'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x04\x05\r'
output=
read_until '*<MANY>--option-11</MANY>*<SENSE-PROMPT>*' || {
  print -u2 -- 'page-down did not select the eleventh candidate'
  print -u2 -r -- "$output"
  return 1
}

# Navigation state and Zsh-owned acceptance must stay aligned. First inspect
# the state after one Ctrl-N, then use a fresh popup to round-trip through
# Ctrl-N/Ctrl-P and accept the first item.
zpty -n -w sense-live $'sense-many --option\t'
output=
read_until '*candidate number 10*' || {
  print -u2 -- 'navigation round-trip popup did not open'
  return 1
}
zpty -n -w sense-live $'\x0e\x18\x07'
output=
read_until '*selected=2*identity=2*</STATE>*<SENSE-PROMPT>*' || {
  print -u2 -- 'Ctrl-N did not select the second visible row'
  print -u2 -r -- "$output"
  return 1
}

zpty -n -w sense-live $'sense-many --option\t'
output=
read_until '*candidate number 10*' || {
  print -u2 -- 'navigation acceptance popup did not open'
  return 1
}
zpty -n -w sense-live $'\x0e\x10\x05\r'
output=
read_until '*<MANY>--option-01</MANY>*<SENSE-PROMPT>*' || {
  print -u2 -- 'Ctrl-N/Ctrl-P did not restore the first selected row'
  print -u2 -r -- "$output"
  return 1
}

# Buffer-destructive edits are completion events too. Deleting "am" must
# regenerate the candidate universe for the remaining "--" query.
zpty -w sense-live '_zsh_sense_activation_mode=continuous'
output=
read_until '*<SENSE-PROMPT>*' || return 1
zpty -n -w sense-live 'sense-test --am'
output=
read_until '*replace the previous commit*' || {
  print -u2 -- 'backspace regression setup did not open the popup'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x7f\x7f'
# Highlight-range updates can legitimately interleave terminal SGR sequences
# between unchanged characters during a differential redraw. Inspect the
# completion state instead of treating the raw PTY byte stream as plain text.
zselect -t 20 >/dev/null 2>&1 || true
zpty -n -w sense-live $'\x18\x07'
output=
read_until '*captured=--all\|--amend*buffer="sense-test --"*</STATE>*<SENSE-PROMPT>*' || {
  print -u2 -- 'Backspace did not regenerate completions'
  print -u2 -r -- "$output"
  return 1
}

# Fuzzy path completion must retain Zsh's directory semantics. Accepting the
# first directory inserts '/', triggers the nested context, and classifies the
# child with the directory icon rather than the generic file icon.
zpty -w sense-live "cd ${(q)SENSE_ZSH_TEST_WORK}"
output=
read_until '*<SENSE-PROMPT>*' || return 1
zpty -w sense-live '_zsh_sense_border=rounded'
output=
read_until '*<SENSE-PROMPT>*' || return 1
zpty -n -w sense-live 'cd dfil'
output=
read_until '*dotfiles*' || return 1
zpty -n -w sense-live $'\x18\x07'
output=
read_until '*kinds=directory*aligned=1*buffer="cd dfil"*</STATE>*<SENSE-PROMPT>*' || {
  print -u2 -- 'custom bordered directory rows were not cell-aligned'
  print -u2 -r -- "$output"
  return 1
}
zpty -w sense-live '_zsh_sense_border=none'
output=
read_until '*<SENSE-PROMPT>*' || return 1
zpty -n -w sense-live 'cd dfil'
output=
read_until '*dotfiles*' || {
  print -u2 -- 'fuzzy directory completion did not find dotfiles'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x05'
output=
read_until '*nvim*' || {
  print -u2 -- 'accepting a directory did not trigger its nested completion'
  print -u2 -r -- "$output"
  zpty -n -w sense-live $'\x18\x07'
  output=
  read_until '*<STATE>*</STATE>*<SENSE-PROMPT>*' 100 || true
  print -u2 -r -- "$output"
  return 1
}
[[ $output == *'󰉋'* ]] || {
  print -u2 -- 'a Zsh local-directory candidate used the file icon'
  print -u2 -r -- "$output"
  return 1
}
# Accept the empty path component immediately. `_path_files` returns the
# basename `nvim` while PREFIX is still `dotfiles/`; replay must reconstruct
# the full candidate so compadd can accept it and append the directory slash.
zpty -n -w sense-live $'\x05'
zselect -t 20 >/dev/null 2>&1 || true
zpty -n -w sense-live $'\x18\x07'
output=
read_until '*buffer="cd dotfiles/nvim/"*</STATE>*<SENSE-PROMPT>*' || {
  print -u2 -- 'accepting an empty nested path component failed'
  print -u2 -r -- "$output"
  return 1
}

# The same chain remains fuzzy after the parent path has been accepted.
zpty -w sense-live "cd ${(q)SENSE_ZSH_TEST_WORK}"
output=
read_until '*<SENSE-PROMPT>*' || return 1
zpty -n -w sense-live 'cd dfil'
output=
read_until '*dotfiles*' || return 1
zpty -n -w sense-live $'\x05'
output=
read_until '*nvim*' || return 1
zpty -n -w sense-live 'nv'
# The worker deliberately debounces ordinary typing. Let the newest `nv`
# generation replace the immediately rendered previous-directory view before
# exercising acceptance.
zselect -t 20 >/dev/null 2>&1 || true
zpty -n -w sense-live $'\x05\r'
output=
read_until '*<SENSE-PROMPT>*' || return 1
zpty -w sense-live 'print -r -- "<PWD>$PWD</PWD>"'
output=
read_until "*<PWD>$SENSE_ZSH_TEST_WORK/dotfiles/nvim</PWD>*<SENSE-PROMPT>*" || {
  print -u2 -- 'accepted nested directory did not preserve Zsh path semantics'
  print -u2 -r -- "$output"
  return 1
}

print -r -- 'live-client-ok'
