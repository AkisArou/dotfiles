#!/usr/bin/env zsh

emulate -L zsh
setopt errexit nounset pipefail no_aliases

zmodload zsh/system zsh/zselect

typeset -gr project_root=${0:A:h:h}
typeset -gr sense_binary=$project_root/target/debug/zsh-sense
typeset -g test_dir
test_dir=$(mktemp -d)
command chmod 700 -- "$test_dir"

typeset -gi daemon_pid=0
typeset -gi worker_pid=0
typeset -gi read_fd=-1
typeset -gi write_fd=-1

cleanup() {
  setopt localoptions noerrexit
  (( read_fd >= 0 )) && exec {read_fd}>&-
  (( write_fd >= 0 )) && exec {write_fd}>&-
  (( worker_pid > 0 )) && kill -TERM $worker_pid 2>/dev/null || true
  (( daemon_pid > 0 )) && kill -INT $daemon_pid 2>/dev/null || true
  (( worker_pid > 0 )) && wait $worker_pid 2>/dev/null || true
  (( daemon_pid > 0 )) && wait $daemon_pid 2>/dev/null || true
  local path
  for path in "$test_dir/in" "$test_dir/out" "$test_dir/daemon.sock" \
      "$test_dir/worker.log" "$test_dir/daemon.log"; do
    [[ -e $path || -p $path || -S $path ]] && command unlink -- "$path" 2>/dev/null
  done
  command rmdir -- "$test_dir" 2>/dev/null || true
}
trap cleanup EXIT

[[ -x $sense_binary ]] || {
  print -u2 -- "missing test binary: $sense_binary"
  return 1
}

command mkfifo -m 600 -- "$test_dir/in" "$test_dir/out"
"$sense_binary" daemon --socket "$test_dir/daemon.sock" \
  --config "$project_root/config.example.toml" \
  >"$test_dir/daemon.log" 2>&1 &
daemon_pid=$!

for _ in {1..200}; do
  [[ -S $test_dir/daemon.sock ]] && break
  zselect -t 1 >/dev/null 2>&1 || true
done
[[ -S $test_dir/daemon.sock ]] || {
  print -u2 -- 'daemon socket did not appear'
  command cat -- "$test_dir/daemon.log" >&2
  return 1
}

"$sense_binary" worker --socket "$test_dir/daemon.sock" \
  --config "$project_root/config.example.toml" \
  --shell-input-fifo "$test_dir/in" --shell-output-fifo "$test_dir/out" \
  --no-daemon-autostart --zsh-executable "${commands[zsh]}" \
  --zsh-version "$ZSH_VERSION" --zsh-patchlevel "$ZSH_PATCHLEVEL" \
  >"$test_dir/worker.log" 2>&1 &
worker_pid=$!

for _ in {1..200}; do
  sysopen -w -o cloexec,nonblock -u write_fd "$test_dir/in" 2>/dev/null && break
  kill -0 $worker_pid 2>/dev/null || break
  zselect -t 1 >/dev/null 2>&1 || true
done
(( write_fd >= 0 )) || {
  print -u2 -- 'could not connect the shell-to-worker FIFO'
  command cat -- "$test_dir/worker.log" >&2
  return 1
}

sysopen -r -o cloexec,nonblock -u read_fd "$test_dir/out"

typeset stream= chunk=
typeset -gi count=0
typeset -gi read_status=0
for _ in {1..500}; do
  chunk=
  read_status=0
  sysread -c count -i $read_fd -s 65536 -t 0 chunk 2>/dev/null || read_status=$?
  (( read_status == 0 )) && stream+=$chunk
  [[ $stream == *ready* && $stream == *config-end* ]] && break
  kill -0 $worker_pid 2>/dev/null || break
  zselect -t 1 >/dev/null 2>&1 || true
done

[[ $stream == *ready* && $stream == *config-end* ]] || {
  print -u2 -- "startup stream was incomplete: ${(qqq)stream}"
  command cat -- "$test_dir/worker.log" >&2
  return 1
}

source "$project_root/shell/client.zsh"
_zsh_sense_rebuild_styles
[[ $_zsh_sense_style_label == 'fg=#d4d4d4,bg=#202020' &&
   $_zsh_sense_style_label_selected == 'fg=#d4d4d4,bg=#343b41' &&
   $_zsh_sense_style_label_match_selected == 'fg=#18a2fe,bg=#343b41,bold' &&
   $_zsh_sense_style_scrollbar_thumb == 'fg=#bbbbbb,bg=#202020' &&
   $_zsh_sense_style_scrollbar_gutter == 'fg=#343b41,bg=#202020' &&
   $_zsh_sense_style_ghost == 'fg=#707070' ]] || {
  print -u2 -- 'BlinkCmp component styles did not compose with menu and selection backgrounds'
  return 1
}
[[ $_zsh_sense_border == none && -z $_zsh_sense_selected_marker &&
   $_zsh_sense_scrollbar_character == '▐' ]] || {
  print -u2 -- 'borderless, markerless popup defaults were not initialized'
  return 1
}
_zsh_sense_scrollbar_geometry 10 46 0
[[ $REPLY == 2:0 ]] || {
  print -u2 -- "scrollbar did not start at the top: $REPLY"
  return 1
}
_zsh_sense_scrollbar_geometry 10 46 45
[[ $REPLY == 2:8 ]] || {
  print -u2 -- "scrollbar did not end at the bottom: $REPLY"
  return 1
}
_zsh_sense_ghost_chunk 'orce-with-lease' word
[[ $REPLY == orce ]] || {
  print -u2 -- "word ghost acceptance selected the wrong chunk: $REPLY"
  return 1
}
_zsh_sense_ghost_chunk 'files/nvim/init.lua' path-segment
[[ $REPLY == 'files/' ]] || {
  print -u2 -- "path ghost acceptance selected the wrong segment: $REPLY"
  return 1
}
typeset -ga parsed_commands=()
_zsh_sense_dispatch() {
  parsed_commands+=( "$1" )
}
_zsh_sense_rx_buffer=$stream
typeset -gi parse_status=0
_zsh_sense_parse_messages || parse_status=$?
[[ -z $_zsh_sense_rx_buffer && $parsed_commands[1] == ready &&
   ${parsed_commands[(Ie)popup-option]} -gt 0 &&
   ${parsed_commands[(Ie)ghost-config]} -gt 0 &&
   $parsed_commands[-1] == config-end ]] || {
  print -u2 -- "Zsh could not parse the startup stream (status $parse_status, parsed ${(j:,:)parsed_commands}): ${(qqq)_zsh_sense_rx_buffer}"
  return 1
}

print -r -- 'fifo-transport-ok'
