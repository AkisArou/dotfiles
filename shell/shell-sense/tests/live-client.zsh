#!/usr/bin/env zsh

emulate -L zsh
setopt errexit nounset pipefail no_aliases extendedglob

zmodload zsh/zpty zsh/zselect zsh/datetime
typeset -gx SHELL_SENSE_TEST_ROOT=${0:A:h:h}
typeset -g SHELL_SENSE_TEST_TEMP
SHELL_SENSE_TEST_TEMP=$(mktemp -d)
typeset -gx XDG_RUNTIME_DIR="$SHELL_SENSE_TEST_TEMP/runtime"
typeset -gx XDG_STATE_HOME="$SHELL_SENSE_TEST_TEMP/state"
typeset -gx SHELL_SENSE_SOCKET="$SHELL_SENSE_TEST_TEMP/daemon.sock"
typeset -gx SHELL_SENSE_CONFIG="$SHELL_SENSE_TEST_ROOT/config.example.toml"
typeset -gx SHELL_SENSE_COMMAND="$SHELL_SENSE_TEST_ROOT/target/debug/shell-sense"
typeset -gx SHELL_SENSE_NO_DAEMON_AUTOSTART=1
typeset -gx SHELL_SENSE_TEST_WORK="$SHELL_SENSE_TEST_TEMP/work"
typeset -gx TERM=xterm-256color
command mkdir -m 700 -- "$XDG_RUNTIME_DIR" "$XDG_STATE_HOME"
command mkdir -p -- "$SHELL_SENSE_TEST_WORK/dotfiles/nvim"
for index in {01..24}; do
  command touch -- "$SHELL_SENSE_TEST_WORK/dotfiles/entry-$index"
done

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

assert_synchronized_frames() {
  local stream=$1 context=$2 prefix remainder=$1
  [[ -n $remainder ]] || {
    print -u2 -- "$context produced no terminal frame"
    return 1
  }
  while [[ $remainder == *$'\e[?2026h'* ]]; do
    prefix=${remainder%%$'\e[?2026h'*}
    [[ -z $prefix ]] || {
      print -u2 -- "$context emitted terminal output before synchronization began"
      print -u2 -r -- "$stream"
      return 1
    }
    remainder=${remainder#*$'\e[?2026h'}
    [[ $remainder == *$'\e[?2026l'* ]] || {
      print -u2 -- "$context left a synchronized-output transaction open"
      return 1
    }
    remainder=${remainder#*$'\e[?2026l'}
  done
  [[ -z $remainder ]] || {
    print -u2 -- "$context was not composed exclusively from complete synchronized frames"
    print -u2 -r -- "$stream"
    return 1
  }
  [[ $stream != *$'\e[M'* && $stream != *$'\e[L'* ]] || {
    print -u2 -- "$context used ZLE insert/delete-line and exposed a partial popup frame"
    print -u2 -r -- "$stream"
    return 1
  }
}

cleanup() {
  zpty -d sense-live 2>/dev/null || true
  if (( daemon_pid )); then
    kill -INT $daemon_pid 2>/dev/null || true
    wait $daemon_pid 2>/dev/null || true
  fi
  [[ ${SHELL_SENSE_KEEP_TEST_TEMP:-0} == 1 ]] || command rm -rf -- "$SHELL_SENSE_TEST_TEMP"
}
trap cleanup EXIT

"$SHELL_SENSE_COMMAND" daemon --socket "$SHELL_SENSE_SOCKET" \
  --config "$SHELL_SENSE_CONFIG" >"$SHELL_SENSE_TEST_TEMP/daemon.log" 2>&1 &
daemon_pid=$!
for _ in {1..200}; do
  [[ -S $SHELL_SENSE_SOCKET ]] && break
  zselect -t 1 >/dev/null 2>&1 || true
done
[[ -S $SHELL_SENSE_SOCKET ]] || {
  print -u2 -- 'daemon socket did not appear'
  command cat -- "$SHELL_SENSE_TEST_TEMP/daemon.log" >&2
  return 1
}

zpty sense-live zsh -f
output=
read_until '*%*' || {
  print -u2 -- 'initial Zsh prompt did not appear'
  return 1
}
zpty -w sense-live ". ${(q)SHELL_SENSE_TEST_ROOT}/tests/fixtures/live-client-init.zsh"
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
[[ ${SHELL_SENSE_REPORT_TIMINGS:-0} == 1 ]] &&
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
  typeset -a worker_logs=( $XDG_STATE_HOME/shell-sense/worker-*.log(N) )
  (( $#worker_logs )) && command tail -100 -- "${worker_logs[@]}" >&2
  command cat -- "$SHELL_SENSE_TEST_TEMP/daemon.log" >&2
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
[[ $output == *$'\e[?2026h'*$'\e[?2026l'* ]] || {
  print -u2 -- 'popup redisplay was not enclosed by synchronized-output markers'
  print -u2 -r -- "$output"
  return 1
}
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

# A unique authoritative prefix contributes end-of-line ghost text. Right
# accepts that completion token through Zsh rather than splicing display text
# into BUFFER, preserving all ordinary completion semantics.
zpty -n -w sense-live 'sense-test --al'
output=
read_until '*stage modified and deleted files*' || return 1
[[ $output == *$'\e[38;2;112;112;112m'* ]] || {
  print -u2 -- 'completion ghost text is missing its configured highlight'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x18\x08'
read_until '*<GHOST>value="l" stored="l" stale=0 visible=1 selected=1 cursor=15 length=15</GHOST>*' || {
  print -u2 -- 'the authoritative ghost suffix was not active before Right'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\e[C\r'
output=
read_until '*<EXEC>--all</EXEC>*<FINISH-POST>0</FINISH-POST>*<SENSE-PROMPT>*' || {
  print -u2 -- 'completion-derived ghost text was not accepted with Right'
  print -u2 -r -- "$output"
  return 1
}

# A normal edit starts a new completion generation, but must not blank the
# existing frame while that generation is debounced. Rebase the old predicted
# line immediately, exactly as Blink redraws its current item against the new
# buffer, then replace it when the authoritative view arrives.
zpty -n -w sense-live 'sense-single --f'
output=
read_until '*update only if the remote ref is unchanged*' || return 1
zpty -n -w sense-live $'o\x18\x08'
output=
read_until '*<GHOST>value="rce-with-lease" stored="orce-with-lease" stale=1 visible=1 *' 100 || {
  print -u2 -- 'an edit cleared the popup or ghost while its replacement was pending'
  print -u2 -r -- "$output"
  return 1
}
zselect -t 20 >/dev/null 2>&1 || true
output=
zpty -n -w sense-live $'\x18\x08'
read_until '*<GHOST>value="rce-with-lease" stored="rce-with-lease" stale=0 visible=1 *' || {
  print -u2 -- 'the rebased ghost was not replaced by the authoritative generation'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x03'
output=
read_until '*<SENSE-PROMPT>*' || return 1

# Word-mode partial acceptance inserts only the next literal-safe component,
# then lets the ordinary edit lifecycle request a fresh authoritative view.
zpty -w sense-live '_shell_sense_ghost_partial_accept=word'
output=
read_until '*<SENSE-PROMPT>*' || return 1
zpty -n -w sense-live 'sense-single --f'
output=
read_until '*update only if the remote ref is unchanged*' || return 1
zpty -n -w sense-live $'\e[C\x18\x07'
output=
read_until '*buffer="sense-single --force"*</STATE>*<SENSE-PROMPT>*' || {
  print -u2 -- 'word-mode ghost acceptance did not stop at the next word boundary'
  print -u2 -r -- "$output"
  return 1
}
zpty -w sense-live '_shell_sense_ghost_partial_accept=token'
output=
read_until '*<SENSE-PROMPT>*' || return 1

# Zsh represents a short-option continuation with distinct insertion and
# presentation values (`-la` versus `-a`). The popup must show the structured
# description once, classify every flag as an option, and put the scrollbar
# at the actual outer edge rather than leaving padding after it.
zpty -n -w sense-live 'ls -l'
output=
read_until '*list entries starting with .*' || {
  print -u2 -- 'standard ls option metadata did not reach the popup'
  print -u2 -r -- "$output"
  return 1
}
[[ $output == *'󰌋'* && $output != *'󰘵'* ]] || {
  print -u2 -- 'ls options did not use the keyword-style option icon'
  print -u2 -r -- "$output"
  return 1
}
zselect -t 50 >/dev/null 2>&1 || true
zpty -n -w sense-live $'\x18\x04'
read_until '*<DOC>placement=side offset=0 total=<-> *text=*-a, --all*do not ignore entries starting with .*</DOC>*' || {
  print -u2 -- 'selected ls option did not resolve focused man-page documentation'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x18\x07'
output=
read_until '*kinds=option*duplicates=0*flush=1*buffer="ls -l"*</STATE>*<SENSE-PROMPT>*' || {
  print -u2 -- 'option labels, descriptions, kinds, or scrollbar geometry regressed'
  print -u2 -r -- "$output"
  return 1
}

# Path documentation comes from the configured argv resolver. Its output is
# additional information, while the native `local directory` text stays only
# in the menu. Explicit side mode must keep the same documentation pane to the
# right whenever the minimum two-panel layout fits.
zpty -w sense-live "cd -- ${(q)SHELL_SENSE_TEST_WORK}"
output=
read_until '*<SENSE-PROMPT>*' || return 1
zpty -n -w sense-live 'cd dotfil'
output=
read_until '*entry-01*' || {
  print -u2 -- 'directory documentation did not reach the popup'
  print -u2 -r -- "$output"
  return 1
}
zselect -t 50 >/dev/null 2>&1 || true
zpty -n -w sense-live $'\x18\x04'
read_until '*<DOC>placement=side offset=0 total=<-> viewport=10 lines=10 scrollbar=1 render-rows=10 text=*entry-01*</DOC>*' || {
  print -u2 -- 'configured directory documentation did not resolve the typed path'
  print -u2 -r -- "$output"
  return 1
}

# Documentation has its own viewport and bindings. A page movement must not
# change the selected completion, and the viewport clamps back to the first
# row independently of candidate navigation.
output=
zpty -n -w sense-live $'\x06'
zselect -t 10 >/dev/null 2>&1 || true
zpty -n -w sense-live $'\x18\x04'
read_until '*<DOC>placement=side offset=[1-9]* total=<-> *text=*</DOC>*' || {
  print -u2 -- 'documentation page-down did not move its independent viewport'
  print -u2 -r -- "$output"
  return 1
}
output=
zpty -n -w sense-live $'\x02'
zselect -t 10 >/dev/null 2>&1 || true
zpty -n -w sense-live $'\x18\x04'
read_until '*<DOC>placement=side offset=0 total=<-> *text=*</DOC>*' || {
  print -u2 -- 'documentation page-up did not return to its first row'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x18\x07'
output=
read_until '*doc-place=side*buffer="cd dotfil"*</STATE>*<SENSE-PROMPT>*' || {
  print -u2 -- 'explicit side documentation mode did not remain on the right'
  print -u2 -r -- "$output"
  return 1
}

# Scrolloff keeps two candidates visible after the selection. With ten popup
# rows, selecting the ninth item moves the viewport start to the second item
# instead of pinning the selection to the final row.
zpty -n -w sense-live $'\x03'
output=
read_until '*<SENSE-PROMPT>*' || return 1
zpty -n -w sense-live $'sense-many --option\t'
output=
read_until '*candidate number 10*' || return 1
# Settle on the last selection before the viewport starts moving, then isolate
# the one key that advances both the selection and the viewport. Every byte of
# that transition must stay inside synchronized-output transactions, and ZLE
# must not express the update as a terminal insert/delete-line operation.
zpty -n -w sense-live $'\x0e\x0e\x0e\x0e\x0e\x0e\x0e'
zselect -t 20 >/dev/null 2>&1 || true
while zpty -r -t sense-live chunk 2>/dev/null; do chunk=; done
output=
zpty -n -w sense-live $'\x0e'
zselect -t 20 >/dev/null 2>&1 || true
while zpty -r -t sense-live chunk 2>/dev/null; do
  output+=$chunk
  chunk=
done
typeset viewport_output=$output
assert_synchronized_frames "$viewport_output" 'viewport navigation' || return 1
zpty -n -w sense-live $'\x18\x07'
output=
read_until '*selected=9 *menu-start=1 render-first=2 render-rows=10 *buffer="sense-many --option"*</STATE>*<SENSE-PROMPT>*' || {
  print -u2 -- 'popup scrolloff did not retain two following candidates'
  print -u2 -r -- "$output"
  return 1
}
[[ $output == *'sync-fd='[0-9]##' sync-active=0 '* ]] || {
  print -u2 -- 'native navigation redraw did not settle its event-loop transaction'
  print -u2 -r -- "$output"
  return 1
}

# A Blink-style viewport is stateful. After scrolling down, reversing direction
# moves the selection through the existing viewport; it must not immediately
# scroll the list backward or move the same selected item to another row.
zpty -n -w sense-live 'sense-many --option'
output=
read_until '*candidate number 10*' || return 1
zpty -n -w sense-live $'\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e'
zselect -t 20 >/dev/null 2>&1 || true
zpty -n -w sense-live $'\x10\x18\x0e'
output=
read_until '*<NAV>selected=10 selected-absolute=9 serial=11 window-start=0 menu-start=3 render-first=4 *' || {
  print -u2 -- 'reverse navigation recomputed the viewport instead of preserving it'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x03'
output=
read_until '*<SENSE-PROMPT>*' || return 1

# Crossing the worker's larger prefetch boundary must not alter the visible
# menu viewport. The worker rotates from candidates 1..20 to 8..27 before the
# next item is needed; reversing afterward moves only the highlight until the
# upper scrolloff margin is reached.
zpty -n -w sense-live 'sense-window --window'
output=
read_until '*viewport candidate 10*' || return 1
typeset seventeen_next=$'\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e'
zpty -n -w sense-live "$seventeen_next"
zselect -t 20 >/dev/null 2>&1 || true
zpty -n -w sense-live $'\x18\x0e'
output=
read_until '*<NAV>selected=11 selected-absolute=17 serial=17 window-start=7 menu-start=10 render-first=4 *' || {
  print -u2 -- 'candidate prefetch changed the visible menu viewport'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x0e\x10'
zselect -t 20 >/dev/null 2>&1 || true
zpty -n -w sense-live $'\x18\x0e'
output=
read_until '*<NAV>selected=11 selected-absolute=17 serial=19 window-start=7 menu-start=11 render-first=5 *' || {
  print -u2 -- 'reverse navigation moved the prefetched viewport instead of only the highlight'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x03'
output=
read_until '*<SENSE-PROMPT>*' || return 1

# Candidate selection is locally immediate and navigation acknowledgements are
# serialised. Forty key presses cross the end of this 15-item list twice; stale
# worker acknowledgements must never move the highlight backwards afterward.
zpty -n -w sense-live 'sense-many --option'
output=
read_until '*candidate number 10*' || return 1
typeset rapid_navigation=$'\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e\x0e'
rapid_navigation+=$rapid_navigation$rapid_navigation$rapid_navigation
typeset -F navigation_started=$EPOCHREALTIME
zpty -n -w sense-live "$rapid_navigation"$'\x18\x0e'
output=
read_until '*<NAV>selected=11 selected-absolute=10 serial=40 *documentation-current=<-></NAV>*' 200 || {
  print -u2 -- 'rapid cyclic navigation did not reach the expected item immediately'
  print -u2 -r -- "$output"
  return 1
}
typeset -F navigation_elapsed=$(( EPOCHREALTIME - navigation_started ))
(( navigation_elapsed < 0.20 )) || {
  print -u2 -- "rapid navigation took ${navigation_elapsed}s"
  return 1
}
zselect -t 20 >/dev/null 2>&1 || true
output=
zpty -n -w sense-live $'\x18\x0e'
read_until '*<NAV>selected=11 selected-absolute=10 serial=40 *documentation-current=<-></NAV>*' || {
  print -u2 -- 'a stale navigation acknowledgement moved the selected item backwards'
  print -u2 -r -- "$output"
  return 1
}
[[ $output == *'render-dirty=0 redraw-pending=0 '* ]] || {
  print -u2 -- 'coalesced navigation redraw did not settle on the latest selection'
  print -u2 -r -- "$output"
  return 1
}

# A second rapid burst observes the display stream independently of the state
# probe above. Coalescing may skip intermediate selections, but every frame it
# does publish must remain atomic across cycling, viewport and scrollbar moves.
while zpty -r -t sense-live chunk 2>/dev/null; do chunk=; done
output=
zpty -n -w sense-live "$rapid_navigation"
zselect -t 20 >/dev/null 2>&1 || true
while zpty -r -t sense-live chunk 2>/dev/null; do
  output+=$chunk
  chunk=
done
assert_synchronized_frames "$output" 'rapid navigation' || return 1
zpty -n -w sense-live $'\x03'
output=
read_until '*<SENSE-PROMPT>*' || return 1

# Match Blink's stable-document behavior: changing the selected item keeps the
# old document visible during the update delay, then swaps atomically to the
# newly resolved document. It must never clear and reopen between those states.
zpty -n -w sense-live 'ls -l'
output=
read_until '*list entries starting with .*' || return 1
zselect -t 50 >/dev/null 2>&1 || true
zpty -n -w sense-live $'\x0e\x18\x0e'
output=
read_until '*<NAV>*documentation-id=?*documentation-current=0</NAV>*' 100 || {
  print -u2 -- 'documentation was cleared instead of being retained during navigation'
  print -u2 -r -- "$output"
  return 1
}
zselect -t 20 >/dev/null 2>&1 || true
output=
zpty -n -w sense-live $'\x18\x0e'
read_until '*<NAV>*documentation-current=1</NAV>*' || {
  print -u2 -- 'documentation did not settle on the final selected item'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x03'
output=
read_until '*<SENSE-PROMPT>*' || return 1

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

# Ctrl-C has the same two-phase terminal cleanup requirement as Enter, but it
# ultimately delegates to the original send-break widget and starts a clean
# prompt instead of executing the buffer.
zpty -w sense-live '_shell_sense_test_interrupt_erase_count=0; _shell_sense_test_key_dispatch_count=0'
output=
read_until '*<SENSE-PROMPT>*' || return 1
zpty -n -w sense-live 'sense-many --option'
output=
read_until '*candidate number 10*' || {
  print -u2 -- 'Ctrl-C cleanup setup did not open the popup'
  return 1
}
zpty -n -w sense-live $'\x03'
output=
read_until '*<SENSE-PROMPT>*' || {
  print -u2 -- 'Ctrl-C did not return to a fresh prompt'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live $'\x18\x07'
output=
read_until '*terminal-interrupt=1*dispatches=1*erase=1*buffer=""*</STATE>*<SENSE-PROMPT>*' || {
  print -u2 -- 'Ctrl-C bypassed physical popup cleanup'
  print -u2 -r -- "$output"
  return 1
}
zpty -w sense-live 'sense-tty'
output=
read_until '*<TTY-INT>enabled</TTY-INT>*<SENSE-PROMPT>*' || {
  print -u2 -- 'the terminal interrupt character was not restored before execution'
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
zpty -w sense-live '_shell_sense_activation_mode=manual'
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
zpty -w sense-live '_shell_sense_activation_mode=continuous'
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
zpty -w sense-live "cd ${(q)SHELL_SENSE_TEST_WORK}"
output=
read_until '*<SENSE-PROMPT>*' || return 1
zpty -w sense-live '_shell_sense_border=rounded'
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
zpty -w sense-live '_shell_sense_border=none'
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
zpty -w sense-live "cd ${(q)SHELL_SENSE_TEST_WORK}"
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
read_until "*<PWD>$SHELL_SENSE_TEST_WORK/dotfiles/nvim</PWD>*<SENSE-PROMPT>*" || {
  print -u2 -- 'accepted nested directory did not preserve Zsh path semantics'
  print -u2 -r -- "$output"
  return 1
}

# A worker crash must not require re-sourcing the plugin. The next ZLE session
# owns recovery, re-establishes a fresh native session, and reinstalls the
# current configuration before completion resumes.
zpty -w sense-live 'typeset -g _sense_test_old_worker=$_shell_sense_worker_pid; kill -KILL $_sense_test_old_worker; sleep 0.05'
output=
read_until '*<SENSE-PROMPT>*' || {
  print -u2 -- 'Zsh did not return to a prompt after its worker was killed'
  return 1
}
zselect -t 20 >/dev/null 2>&1 || true
zpty -w sense-live 'print -r -- "<WORKER-RECOVERED>$(( _sense_test_old_worker != _shell_sense_worker_pid )):$_shell_sense_ready:$_shell_sense_configured</WORKER-RECOVERED>"'
output=
read_until '*<WORKER-RECOVERED>1:1:1</WORKER-RECOVERED>*<SENSE-PROMPT>*' || {
  print -u2 -- 'Zsh did not establish a fresh configured worker after a crash'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-live 'sense-test --a'
output=
read_until '*replace the previous commit*' || {
  print -u2 -- 'Zsh completion did not recover after its worker restarted'
  return 1
}
zpty -n -w sense-live $'\x18\x07'
output=
read_until '*<SENSE-PROMPT>*' || return 1

print -r -- 'live-client-ok'
