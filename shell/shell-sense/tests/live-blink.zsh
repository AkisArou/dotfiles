#!/usr/bin/env zsh

emulate -L zsh
setopt errexit nounset pipefail no_aliases extendedglob

zmodload zsh/zpty zsh/zselect
typeset -gr project_root=${0:A:h:h}
typeset -gr binary="$project_root/target/debug/shell-sense"
typeset -gr blink_cmp_root=${SHELL_SENSE_BLINK_CMP_ROOT:-$HOME/.local/share/nvim/site/pack/core/opt/blink.cmp}
typeset -gr blink_lib_root=${SHELL_SENSE_BLINK_LIB_ROOT:-$HOME/.local/share/nvim/site/pack/core/opt/blink.lib}
typeset -g runtime_dir
runtime_dir=$(mktemp -d /tmp/shell-sense-live-blink.XXXXXX)
typeset -gi daemon_pid=0
typeset -g output= chunk=

fail() {
  print -u2 -- "$1"
  [[ -z $output ]] || print -u2 -r -- "$output"
  [[ ! -f $runtime_dir/result ]] || command cat -- "$runtime_dir/result" >&2
  [[ ! -f $runtime_dir/daemon.log ]] || command sed -n '1,240p' "$runtime_dir/daemon.log" >&2
  local worker_log
  for worker_log in "$runtime_dir"/state/shell-sense/worker-*.log(N); do
    command tail -n 240 -- "$worker_log" >&2
  done
  return 1
}

cleanup() {
  zpty -d sense-blink 2>/dev/null || true
  if (( daemon_pid )); then
    kill -INT $daemon_pid 2>/dev/null || true
    wait $daemon_pid 2>/dev/null || true
  fi
  [[ ${SHELL_SENSE_KEEP_TEST_TEMP:-0} == 1 ]] || command rm -rf -- "$runtime_dir"
}
trap cleanup EXIT

[[ -x $binary ]] || fail "build $binary before running this test"
(( $+commands[nvim] )) || fail 'Neovim is required for the live Blink test'
[[ -f $blink_cmp_root/lua/blink/cmp/init.lua ]] || fail "Blink.cmp was not found at $blink_cmp_root"
[[ -f $blink_lib_root/lua/blink/lib/init.lua ]] || fail "blink.lib was not found at $blink_lib_root"

command mkdir -m 700 -- "$runtime_dir/state"
command mkdir -p -- "$runtime_dir/work/dotfiles/nvim"
command touch -- "$runtime_dir/work/dotfiles/README.md"

"$binary" daemon --socket "$runtime_dir/daemon.sock" \
  --config "$project_root/config.example.toml" >"$runtime_dir/daemon.log" 2>&1 &
daemon_pid=$!
for _ in {1..200}; do
  [[ -S $runtime_dir/daemon.sock ]] && break
  zselect -t 1 >/dev/null 2>&1 || true
done
[[ -S $runtime_dir/daemon.sock ]] || fail 'Blink test daemon socket did not appear'

zpty sense-blink exec env \
  HOME="$HOME" \
  LANG=C.UTF-8 \
  PATH="$PATH" \
  TERM=xterm-256color \
  XDG_RUNTIME_DIR="$runtime_dir" \
  XDG_STATE_HOME="$runtime_dir/state" \
  SHELL_SENSE_COMMAND="$binary" \
  SHELL_SENSE_CONFIG="$project_root/config.example.toml" \
  SHELL_SENSE_SOCKET="$runtime_dir/daemon.sock" \
  SHELL_SENSE_NO_DAEMON_AUTOSTART=1 \
  SHELL_SENSE_TEST_ROOT="$project_root" \
  SHELL_SENSE_TEST_WORK="$runtime_dir/work" \
  SHELL_SENSE_BLINK_RESULT="$runtime_dir/result" \
  SHELL_SENSE_BLINK_CMP_ROOT="$blink_cmp_root" \
  SHELL_SENSE_BLINK_LIB_ROOT="$blink_lib_root" \
  nvim --clean -u NONE -l "$project_root/tests/live-blink.lua"

for _ in {1..6000}; do
  while zpty -r -t sense-blink chunk 2>/dev/null; do
    output+=$chunk
    chunk=
  done
  [[ -f $runtime_dir/result ]] && break
  zselect -t 1 >/dev/null 2>&1 || true
done

[[ -f $runtime_dir/result ]] || fail 'the live Blink test did not finish'
[[ $(<"$runtime_dir/result") == live-blink-ok ]] || fail 'the live Blink lifecycle failed'
print -r -- live-blink-ok
