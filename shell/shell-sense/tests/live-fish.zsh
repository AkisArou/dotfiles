#!/usr/bin/env zsh

emulate -L zsh
setopt errexit nounset pipefail no_aliases extendedglob

zmodload zsh/zpty zsh/zselect
typeset -gr project_root=${0:A:h:h}
typeset -gr binary="$project_root/target/debug/shell-sense"
typeset -gr entry="$project_root/shell/fish/shell-sense.fish"
typeset -g runtime_dir
runtime_dir=$(mktemp -d /tmp/shell-sense-live-fish.XXXXXX)
typeset -gi daemon_pid=0

source "$project_root/tests/lib/live-pty.zsh"

fail() {
  print -u2 -- "$1"
  print -u2 -r -- "$SHELL_SENSE_PTY_OUTPUT"
  [[ ! -f $runtime_dir/daemon.log ]] || command sed -n '1,240p' "$runtime_dir/daemon.log" >&2
  local worker_log
  for worker_log in "$runtime_dir"/state/shell-sense/worker-*.log(N); do
    command sed -n '1,240p' "$worker_log" >&2
  done
  return 1
}

wait_for_fish_state() {
  local pattern=$1 message=$2
  local -i attempt
  for (( attempt = 1; attempt <= 50; attempt++ )); do
    shell_sense_pty_reset
    shell_sense_pty_write_raw $'\x18\x07'
    if shell_sense_pty_read_until '*<FISH-STATE>*' 50 &&
       [[ $SHELL_SENSE_PTY_OUTPUT == ${~pattern} ]]; then
      return 0
    fi
    zselect -t 2 >/dev/null 2>&1 || true
  done
  fail "$message"
}

cleanup() {
  shell_sense_pty_close
  if (( daemon_pid )); then
    kill -INT $daemon_pid 2>/dev/null || true
    wait $daemon_pid 2>/dev/null || true
  fi
  command rm -rf -- "$runtime_dir"
}
trap cleanup EXIT

[[ -x $binary ]] || fail "build $binary before running this test"
command mkdir -m 700 -- "$runtime_dir/state"
"$binary" daemon --socket "$runtime_dir/daemon.sock" >"$runtime_dir/daemon.log" 2>&1 &
daemon_pid=$!
for _ in {1..200}; do
  [[ -S $runtime_dir/daemon.sock ]] && break
  zselect -t 1 >/dev/null 2>&1 || true
done
[[ -S $runtime_dir/daemon.sock ]] || fail 'Fish test daemon socket did not appear'

shell_sense_pty_start sense-fish env -i \
  HOME=/home/akisarou \
  LANG=C.UTF-8 \
  PATH=/usr/bin:/bin \
  SHELL=/usr/bin/fish \
  TERM=dumb \
  XDG_RUNTIME_DIR="$runtime_dir" \
  XDG_STATE_HOME="$runtime_dir/state" \
  SHELL_SENSE_COMMAND="$binary" \
  SHELL_SENSE_CONFIG="$project_root/config.example.toml" \
  SHELL_SENSE_SOCKET="$runtime_dir/daemon.sock" \
  fish --no-config --interactive

shell_sense_pty_read_until '*> *' || fail 'initial Fish prompt did not appear'
shell_sense_pty_write_line 'stty -echo'
shell_sense_pty_reset
shell_sense_pty_read_until '*> *' || fail 'Fish did not disable terminal echo'
shell_sense_pty_write_line 'cd /home/akisarou; printf "<FISH-CWD>%s</FISH-CWD>\n" "$PWD"'
shell_sense_pty_reset
shell_sense_pty_read_until '*<FISH-CWD>/home/akisarou</FISH-CWD>*' ||
  fail 'Fish test shell did not enter the fixture directory'
shell_sense_pty_write_line 'function x; printf "<FISH-ACCEPT>%s</FISH-ACCEPT>\n" "$argv[1]"; end'
shell_sense_pty_write_line "complete -c x -a 'restart reset-failed rescue reload' -d 'native action'"
shell_sense_pty_write_line 'function __shell_sense_test_prompt --on-event fish_prompt; printf "<FISH-PROMPT-PWD>%s</FISH-PROMPT-PWD>\n" "$PWD"; end'
shell_sense_pty_write_line 'function __shell_sense_test_state; printf "<FISH-STATE>line=%s,active=%s,ready=%s,visible=%s</FISH-STATE>\n" (commandline -b) "$_shell_sense_fish_active_buffer" "$_shell_sense_fish_view_ready" "$_shell_sense_fish_popup_visible"; end'
shell_sense_pty_write_line 'function __shell_sense_test_worker_state; set -l changed 0; test $__sense_test_old_worker -ne $_shell_sense_fish_worker_pid; and set changed 1; printf "<FISH-WORKER-RECOVERED>%s:%s:%s</FISH-WORKER-RECOVERED>\n" $changed $_shell_sense_fish_ready $_shell_sense_fish_configured; end'
shell_sense_pty_write_line "source ${(q)entry}; bind \\cx\\cg __shell_sense_test_state; bind \\cx\\cw __shell_sense_test_worker_state; printf '<FISH-READY>ready</FISH-READY>\\n'"
shell_sense_pty_reset
shell_sense_pty_read_until '*<FISH-READY>ready</FISH-READY>*' 1000 ||
  fail 'Fish client initialization did not finish'

# Native filename completion is accepted by Fish itself, including its slash.
shell_sense_pty_reset
shell_sense_pty_write_raw 'cd dot'
shell_sense_pty_read_until '*dotfiles/*' || fail 'Fish directory completion setup did not settle'
shell_sense_pty_reset
shell_sense_pty_write_raw 'f'
shell_sense_pty_read_until '*dotfiles/*' || fail 'Fish directory popup did not refresh for the final edit'
wait_for_fish_state \
  '*<FISH-STATE>line=cd dotf,active=cd dotf,ready=1,visible=1</FISH-STATE>*' \
  'Fish directory popup did not settle on the current commandline buffer'
shell_sense_pty_reset
shell_sense_pty_write_raw $'\x05'
shell_sense_pty_read_until '*cd dotfiles/*' ||
  fail 'Fish did not apply the native directory candidate'
shell_sense_pty_reset
shell_sense_pty_write_raw $'\r'
shell_sense_pty_read_until '*<FISH-PROMPT-PWD>/home/akisarou/dotfiles</FISH-PROMPT-PWD>*' ||
  fail 'Fish executed a different directory after completion acceptance'

# Backspace must create a new native request and Ctrl-E must accept that view.
shell_sense_pty_reset
shell_sense_pty_write_raw 'x restar'
shell_sense_pty_read_until '*restart*' || fail 'Fish custom completion setup did not settle'
shell_sense_pty_reset
shell_sense_pty_write_raw 'X'
shell_sense_pty_read_until '*restart*' || fail 'Fish custom popup did not refresh for the final edit'
wait_for_fish_state \
  '*<FISH-STATE>line=x restarX,active=x restarX,ready=1,visible=1</FISH-STATE>*' \
  'Fish custom native popup did not settle on the current commandline buffer'
shell_sense_pty_reset
shell_sense_pty_write_raw $'\x7f'
shell_sense_pty_read_until '*restart*' || fail 'Fish native popup did not render after Backspace'
wait_for_fish_state \
  '*<FISH-STATE>line=x restar,active=x restar,ready=1,visible=1</FISH-STATE>*' \
  'Fish did not refresh native candidates after Backspace'
shell_sense_pty_reset
shell_sense_pty_write_raw $'\x05'
shell_sense_pty_read_until '*x restart *' ||
  fail 'Fish did not apply the refreshed native candidate'
shell_sense_pty_write_raw $'\r'
shell_sense_pty_reset
shell_sense_pty_read_until '*<FISH-ACCEPT>restart</FISH-ACCEPT>*' ||
  fail 'Fish executed a different custom candidate after completion acceptance'
shell_sense_pty_read_until '*<FISH-PROMPT-PWD>*' ||
  fail 'Fish did not settle before the worker recovery test'

# fish_prompt owns recovery after a bridge crash; no re-sourcing or synthetic
# candidates are involved.
shell_sense_pty_reset
shell_sense_pty_write_line 'set -g __sense_test_old_worker $_shell_sense_fish_worker_pid; kill -KILL $__sense_test_old_worker; sleep 0.05; printf "<FISH-WORKER-KILLED>1</FISH-WORKER-KILLED>\n"'
shell_sense_pty_read_until '*<FISH-WORKER-KILLED>1</FISH-WORKER-KILLED>*<FISH-PROMPT-PWD>*' ||
  fail 'Fish did not return to a prompt after its worker was killed'
shell_sense_pty_reset
shell_sense_pty_write_raw $'\x18\x17'
shell_sense_pty_read_until '*<FISH-WORKER-RECOVERED>1:1:1</FISH-WORKER-RECOVERED>*' ||
  fail 'Fish did not establish a fresh configured worker after a crash'
shell_sense_pty_write_raw 'x rstart'
shell_sense_pty_read_until '*restart*' ||
  fail 'Fish completion did not recover after its worker restarted'
shell_sense_pty_write_raw $'\x18\x07'
shell_sense_pty_read_until '*<FISH-STATE>*' || fail 'Fish recovery popup did not remain interactive'
shell_sense_pty_write_raw $'\x03'
shell_sense_pty_reset
shell_sense_pty_read_until '*> *' || fail 'Fish did not clear the recovery probe'

shell_sense_pty_write_line exit
print 'live Fish client test passed'
