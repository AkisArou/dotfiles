#!/usr/bin/env zsh

emulate -L zsh
setopt errexit nounset pipefail no_aliases extendedglob

zmodload zsh/zpty zsh/zselect
typeset -gr project_root=${0:A:h:h}
typeset -gr binary="$project_root/target/debug/shell-sense"
typeset -gr entry="$project_root/shell/bash/shell-sense.bash"
typeset -g runtime_dir
runtime_dir=$(mktemp -d /tmp/shell-sense-live-bash.XXXXXX)
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
[[ -S $runtime_dir/daemon.sock ]] || fail 'Bash test daemon socket did not appear'

shell_sense_pty_start sense-bash env -i \
  HOME=/home/akisarou \
  LANG=C.UTF-8 \
  PATH=/usr/bin:/bin \
  SHELL=/bin/bash \
  TERM=xterm-256color \
  XDG_RUNTIME_DIR="$runtime_dir" \
  XDG_STATE_HOME="$runtime_dir/state" \
  SHELL_SENSE_COMMAND="$binary" \
  SHELL_SENSE_CONFIG="$project_root/config.example.toml" \
  SHELL_SENSE_SOCKET="$runtime_dir/daemon.sock" \
  bash --noprofile --norc -i

shell_sense_pty_read_until '*$ *' || fail 'initial Bash prompt did not appear'
shell_sense_pty_write_line 'stty -echo'
shell_sense_pty_reset
shell_sense_pty_read_until '*$ *' || fail 'Bash did not disable terminal echo'
shell_sense_pty_write_line 'cd /home/akisarou; printf "<BASH-CWD>%s</BASH-CWD>\n" "$PWD"'
shell_sense_pty_reset
shell_sense_pty_read_until '*<BASH-CWD>/home/akisarou</BASH-CWD>*' ||
  fail 'Bash test shell did not enter the fixture directory'
shell_sense_pty_write_line 'x() { printf "<BASH-ACCEPT>%s</BASH-ACCEPT>\n" "$1"; }'
shell_sense_pty_write_line "complete -W 'restart reset-failed rescue reload' x"
shell_sense_pty_write_line '_shell_sense_test_state() { printf "<BASH-STATE>line=%s,active=%s,visible=%s</BASH-STATE>\n" "$READLINE_LINE" "$_shell_sense_bash_active_buffer" "$_shell_sense_bash_popup_visible"; }'
shell_sense_pty_write_line 'bind -x '\''"\C-x\C-g":_shell_sense_test_state'\'''
shell_sense_pty_write_line '_shell_sense_test_worker_state() { local changed=0; ((__sense_test_old_worker != _shell_sense_bash_worker_pid)) && changed=1; printf "<BASH-WORKER-RECOVERED>%s:%s:%s</BASH-WORKER-RECOVERED>\n" "$changed" "$_shell_sense_bash_ready" "$_shell_sense_bash_configured"; }'
shell_sense_pty_write_line 'bind -x '\''"\C-x\C-w":_shell_sense_test_worker_state'\'''
shell_sense_pty_write_line '_shell_sense_test_kill_worker() { __sense_test_old_worker=$_shell_sense_bash_worker_pid; kill -KILL "$__sense_test_old_worker"; sleep 0.05; printf "<BASH-WORKER-KILLED>1</BASH-WORKER-KILLED>\n"; }'
shell_sense_pty_write_line 'bind -x '\''"\C-x\C-k":_shell_sense_test_kill_worker'\'''
shell_sense_pty_write_line 'PROMPT_COMMAND='\''printf "<BASH-PROMPT-PWD>%s</BASH-PROMPT-PWD>\n" "$PWD"'\'''
shell_sense_pty_write_line "source ${(q)entry}; printf '<BASH-READY>ready</BASH-READY>\\n'"
shell_sense_pty_reset
shell_sense_pty_read_until '*<BASH-READY>ready</BASH-READY>*' 1000 ||
  fail 'Bash client initialization did not finish'

# Native filename completion is accepted by Bash itself, including its slash.
shell_sense_pty_reset
shell_sense_pty_write_raw $'cd dotf\x18\x07'
shell_sense_pty_read_until '*<BASH-STATE>line=cd dotf,active=cd dotf,visible=1</BASH-STATE>*' ||
  fail 'Bash directory popup did not settle on the current Readline buffer'
shell_sense_pty_reset
shell_sense_pty_write_raw $'\x05\x18\x07'
shell_sense_pty_read_until '*<BASH-STATE>line=cd dotfiles/,active=cd dotfiles/,visible=1</BASH-STATE>*' ||
  fail 'Bash did not apply the native directory candidate'
shell_sense_pty_reset
shell_sense_pty_write_raw $'\r'
shell_sense_pty_read_until '*<BASH-PROMPT-PWD>/home/akisarou/dotfiles</BASH-PROMPT-PWD>*' ||
  fail 'Bash executed a different directory after completion acceptance'

# Backspace must create a new native request and Ctrl-E must accept that view.
shell_sense_pty_reset
shell_sense_pty_write_raw $'x rstart\x18\x07'
shell_sense_pty_read_until '*<BASH-STATE>line=x rstart,active=x rstart,visible=1</BASH-STATE>*' 1000 ||
  fail 'Bash custom native popup did not settle on the current Readline buffer'
shell_sense_pty_reset
shell_sense_pty_write_raw $'\x08\x18\x07'
shell_sense_pty_read_until '*<BASH-STATE>line=x rstar,active=x rstar,visible=1</BASH-STATE>*' ||
  fail 'Bash did not refresh native candidates after Backspace'
shell_sense_pty_reset
shell_sense_pty_write_raw $'\x05\x18\x07'
shell_sense_pty_read_until '*<BASH-STATE>line=x restart ,active=x restart ,visible=1</BASH-STATE>*' ||
  fail 'Bash did not apply the refreshed native candidate'
shell_sense_pty_write_raw $'\r'
shell_sense_pty_reset
shell_sense_pty_read_until '*<BASH-ACCEPT>restart</BASH-ACCEPT>*' ||
  fail 'Bash executed a different custom candidate after completion acceptance'
shell_sense_pty_read_until '*<BASH-PROMPT-PWD>/home/akisarou/dotfiles</BASH-PROMPT-PWD>*' ||
  fail 'Bash did not return to Readline after custom candidate execution'

# Readline may reserve the terminal's configured erase byte and bypass a
# reported bind -x registration. Any Shell Sense action must detect that edit,
# refresh the native generation, and only then act on the current view.
shell_sense_pty_reset
shell_sense_pty_write_raw $'x restarX\x18\x07'
shell_sense_pty_read_until '*<BASH-STATE>line=x restarX,active=x restarX,visible=1</BASH-STATE>*' ||
  fail 'Bash erase-byte guard setup did not produce a current native view'
shell_sense_pty_reset
shell_sense_pty_write_raw $'\x7f\x05\x18\x07'
shell_sense_pty_read_until '*<BASH-STATE>line=x restart ,active=x restart ,visible=1</BASH-STATE>*' ||
  fail 'Bash acted on a stale native view after Readline-owned Backspace'
shell_sense_pty_write_raw $'\r'
shell_sense_pty_reset
shell_sense_pty_read_until '*<BASH-ACCEPT>restart</BASH-ACCEPT>*' ||
  fail 'Bash rejected the refreshed candidate after Readline-owned Backspace'
shell_sense_pty_read_until '*<BASH-PROMPT-PWD>*' ||
  fail 'Bash did not settle before the worker recovery test'

# PROMPT_COMMAND owns worker recovery without replacing the user's existing
# prompt command or recapturing Shell Sense's own Readline bindings.
shell_sense_pty_reset
shell_sense_pty_write_raw $'\x18\x0b'
shell_sense_pty_read_until '*<BASH-WORKER-KILLED>1</BASH-WORKER-KILLED>*' ||
  fail 'Bash did not execute the worker crash probe'
shell_sense_pty_write_raw $'\r'
shell_sense_pty_read_until '*<BASH-WORKER-KILLED>1</BASH-WORKER-KILLED>*<BASH-PROMPT-PWD>*' ||
  fail 'Bash did not return to a prompt after its worker was killed'
shell_sense_pty_reset
shell_sense_pty_write_raw $'\x18\x17'
shell_sense_pty_read_until '*<BASH-WORKER-RECOVERED>1:1:1</BASH-WORKER-RECOVERED>*' ||
  fail 'Bash did not establish a fresh configured worker after a crash'
shell_sense_pty_write_raw $'x rstart\x18\x07'
shell_sense_pty_read_until '*<BASH-STATE>line=x rstart,active=x rstart,visible=1</BASH-STATE>*' ||
  fail 'Bash completion did not recover after its worker restarted'
shell_sense_pty_write_raw $'\x03'
shell_sense_pty_reset
shell_sense_pty_read_until '*<BASH-PROMPT-PWD>*' || fail 'Bash did not clear the recovery probe'

shell_sense_pty_write_line exit
print 'live Bash client test passed'
