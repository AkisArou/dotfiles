#!/usr/bin/env zsh

emulate -L zsh
setopt errexit nounset pipefail no_aliases extendedglob

zmodload zsh/zpty zsh/zselect zsh/datetime
typeset -gr project_root=${0:A:h:h}
typeset -g test_temp
test_temp=$(mktemp -d)
typeset -gx XDG_RUNTIME_DIR="$test_temp/runtime"
typeset -gx XDG_STATE_HOME="$test_temp/state"
typeset -gx SENSE_ZSH_SOCKET="$test_temp/daemon.sock"
typeset -gx SENSE_ZSH_CONFIG="$project_root/config.example.toml"
typeset -gx SENSE_ZSH_COMMAND=${SENSE_ZSH_COMMAND:-"$project_root/target/release/zsh-sense"}
typeset -gx SENSE_ZSH_NO_DAEMON_AUTOSTART=1
typeset -gx NO_TMUX_AUTO_ATTACH=1
typeset -gx TERM=xterm-256color
command mkdir -m 700 -- "$XDG_RUNTIME_DIR" "$XDG_STATE_HOME"

typeset -gi daemon_pid=0
typeset output= chunk=
read_until() {
  local pattern=$1
  local -i attempts=${2:-500}
  local -i attempt
  local plain_output
  for (( attempt = 1; attempt <= attempts; attempt++ )); do
    while zpty -r -t sense-user chunk 2>/dev/null; do
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
  zpty -d sense-user 2>/dev/null || true
  if (( daemon_pid )); then
    kill -INT $daemon_pid 2>/dev/null || true
    wait $daemon_pid 2>/dev/null || true
  fi
  if [[ ${SENSE_ZSH_KEEP_TEST_TEMP:-0} != 1 && -d $test_temp && $test_temp == /tmp/tmp.* ]]; then
    command rm -rf -- "$test_temp"
  fi
}
trap cleanup EXIT

"$SENSE_ZSH_COMMAND" daemon --socket "$SENSE_ZSH_SOCKET" \
  --config "$SENSE_ZSH_CONFIG" >"$test_temp/daemon.log" 2>&1 &
daemon_pid=$!
for _ in {1..200}; do
  [[ -S $SENSE_ZSH_SOCKET ]] && break
  zselect -t 1 >/dev/null 2>&1 || true
done
[[ -S $SENSE_ZSH_SOCKET ]] || {
  print -u2 -- 'user-config smoke daemon socket did not appear'
  command cat -- "$test_temp/daemon.log" >&2
  return 1
}

zpty sense-user zsh -i
# Give the real prompt/plugin stack time to initialize and enter ZLE.
zselect -t 100 >/dev/null 2>&1 || true
output=
while zpty -r -t sense-user chunk 2>/dev/null; do
  output+=$chunk
  chunk=
done

# zsh-autosuggestions owns Ctrl-E before zsh-sense loads. The state-aware
# dispatcher must replace that binding in vi insert mode while retaining the
# original widget as the closed-popup fallback.
zpty -w sense-user "bindkey -M viins '^E'"
output=
read_until '*.zsh-sense-key-ctrl-e*' || {
  print -u2 -- 'Ctrl-E was not bound to the zsh-sense dispatcher'
  print -u2 -r -- "$output"
  return 1
}

# A first-character command popup is the largest routine command-name
# universe. It must render once, with the user's UTF-8 locale restored.
typeset -F command_popup_started=$EPOCHREALTIME
zpty -n -w sense-user c
output=
read_until '*shell alias*' 100 || {
  print -u2 -- 'one-character command popup did not appear promptly'
  print -u2 -r -- "$output"
  return 1
}
typeset -F command_popup_elapsed=$(( EPOCHREALTIME - command_popup_started ))
(( command_popup_elapsed < 0.8 )) || {
  print -u2 -- "one-character command popup took ${command_popup_elapsed}s"
  return 1
}
[[ ${SENSE_ZSH_REPORT_TIMINGS:-0} == 1 ]] &&
  print -r -- "user-command-popup-ms=$(( command_popup_elapsed * 1000.0 ))"
typeset escaped_meta='\M-'
[[ $output != *$escaped_meta* ]] || {
  print -u2 -- 'one-character popup contains locale-escaped UTF-8 bytes'
  print -u2 -r -- "$output"
  return 1
}
[[ $output != *completions* ]] || {
  print -u2 -- 'popup still contains the completions title'
  print -u2 -r -- "$output"
  return 1
}
for decoration in '╭' '╰' '│' '›'; do
  [[ $output != *$decoration* ]] || {
    print -u2 -- "default popup still contains disabled decoration: $decoration"
    print -u2 -r -- "$output"
    return 1
  }
done
[[ $output == *'▐'* ]] || {
  print -u2 -- 'default popup did not render the narrow scrollbar glyph'
  print -u2 -r -- "$output"
  return 1
}
[[ $output != *' · '* ]] || {
  print -u2 -- 'popup still contains the generic dot indicator'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-user $'\x1b\x15'
zselect -t 10 >/dev/null 2>&1 || true

zpty -n -w sense-user 'ls -'
output=
read_until '*list entries starting with .*' || {
  print -u2 -- 'the popup did not coexist with the real ZLE plugin stack'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-user $'\x03'
output=
read_until '*❯*' || {
  print -u2 -- 'Ctrl-C did not interrupt a popup under the real plugin stack'
  print -u2 -r -- "$output"
  return 1
}
[[ $output != *'read-only variable'* ]] || {
  print -u2 -- 'Ctrl-C touched ZLE state that is immutable during SIGINT'
  print -u2 -r -- "$output"
  return 1
}
[[ $output != *'list entries starting with .'* ]] || {
  print -u2 -- 'the prompt stack redrew the popup while handling Ctrl-C'
  print -u2 -r -- "$output"
  return 1
}

# Verify ordinary editing through the complete user configuration. `cd .`
# succeeds only when `c`, `d`, Space, and Backspace all reached ZLE in order.
typeset -F responsiveness_started=$EPOCHREALTIME
zpty -n -w sense-user c
zselect -t 5 >/dev/null 2>&1 || true
zpty -n -w sense-user d
zselect -t 5 >/dev/null 2>&1 || true
zpty -n -w sense-user ' '
zselect -t 5 >/dev/null 2>&1 || true
zpty -n -w sense-user x
zpty -n -w sense-user $'\x7f.\r'
zpty -w sense-user 'print -r -- "<EDIT-STATUS>$?</EDIT-STATUS>"'
output=
read_until '*<EDIT-STATUS>0</EDIT-STATUS>*' 300 || {
  print -u2 -- 'Space/Backspace editing failed through the real .zshrc'
  print -u2 -r -- "$output"
  return 1
}
typeset -F responsiveness_elapsed=$(( EPOCHREALTIME - responsiveness_started ))
(( responsiveness_elapsed < 0.75 )) || {
  print -u2 -- "real-shell editing took ${responsiveness_elapsed}s; completion blocked ZLE"
  return 1
}

# Reproduce the real home-directory path chain. The second acceptance used to
# fail only under the full user completion stack because this `_path_files`
# call omitted an explicit `-p dotfiles/` replay option.
zpty -w sense-user 'cd "$HOME"'
output=
read_until '*❯*' || return 1
zpty -n -w sense-user 'cd dotfil'
output=
read_until '*dotfiles*local directory*' || {
  print -u2 -- 'real-shell fuzzy directory completion did not open'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-user $'\x05'
output=
read_until '*alacritty*local directory*' || {
  print -u2 -- 'real-shell path chaining did not open the child directory list'
  print -u2 -r -- "$output"
  return 1
}
zpty -n -w sense-user $'\x05\r'
output=
read_until '*❯*' || return 1
zpty -w sense-user 'print -r -- "<CHAIN-PWD>$PWD</CHAIN-PWD>"'
output=
read_until "*<CHAIN-PWD>$HOME/dotfiles/*</CHAIN-PWD>*" || {
  print -u2 -- 'real-shell second path acceptance failed'
  print -u2 -r -- "$output"
  return 1
}

print -r -- 'user-config-smoke-ok'
