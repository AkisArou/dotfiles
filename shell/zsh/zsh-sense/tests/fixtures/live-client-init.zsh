autoload -Uz compinit
compinit -u

_zsh_sense_live_completion() {
  local -a descriptions=(
    '--all — stage modified and deleted files'
    '--amend — replace the previous commit'
  )
  compadd -J options -X 'command options' -d descriptions -- --all --amend
}
compdef _zsh_sense_live_completion sense-test

_zsh_sense_live_fuzzy_completion() {
  local -a descriptions=(
    'reload — reload unit configuration'
    'reset-failed — reset failed unit state'
    'restart — restart one or more units'
  )
  compadd -J subcommands -X 'service actions' -d descriptions -- reload reset-failed restart
}
compdef _zsh_sense_live_fuzzy_completion sense-verb

_zsh_sense_live_many_completion() {
  local -a words=() descriptions=()
  local option
  local -i index
  for (( index = 1; index <= 15; index++ )); do
    printf -v option '%02d' $index
    words+=( "--option-$option" )
    descriptions+=( "--option-$option — candidate number $option" )
  done
  compadd -J options -X 'many options' -d descriptions -- "${words[@]}"
}
compdef _zsh_sense_live_many_completion sense-many

sense-test() {
  print -r -- "<EXEC>$*</EXEC>"
  print -r -- "<FINISH-POST>${#_zsh_sense_test_finish_postdisplay}</FINISH-POST>"
}

sense-verb() {
  print -r -- "<VERB>$*</VERB>"
}

sense-many() {
  print -r -- "<MANY>$*</MANY>"
}

source "$SENSE_ZSH_TEST_ROOT/shell/zsh-sense.plugin.zsh"

# Observe line-finish after zsh-sense. This verifies lifecycle state directly;
# raw PTY transcripts retain bytes that were subsequently erased and therefore
# cannot by themselves distinguish live screen content from scrollback.
typeset -g _zsh_sense_test_finish_postdisplay=unobserved
_zsh_sense_test_line_finish_observer() {
  _zsh_sense_test_finish_postdisplay=$POSTDISPLAY
}
autoload -Uz add-zle-hook-widget
add-zle-hook-widget line-finish _zsh_sense_test_line_finish_observer

_zsh_sense_test_state() {
  local handler=
  local -i render_aligned=1 render_width=0
  local render_line
  if (( $#_zsh_sense_render_lines )); then
    render_width=${#_zsh_sense_render_lines[1]}
    for render_line in "${_zsh_sense_render_lines[@]}"; do
      (( ${#render_line} == render_width )) || render_aligned=0
    done
  fi
  (( _zsh_sense_read_fd >= 0 )) && {
    handler=$(zle -F -L $_zsh_sense_read_fd 2>/dev/null)
    _zsh_sense_fd_callback $_zsh_sense_read_fd
  }
  zle -I
  print -r -- "<STATE>ready=$_zsh_sense_ready configured=$_zsh_sense_configured read=$_zsh_sense_read_fd write=$_zsh_sense_write_fd worker=$_zsh_sense_worker_pid fifo=$_zsh_sense_fifo_in log=$_zsh_sense_log_file request=$_zsh_sense_active_request items=$#_zsh_sense_item_ids captured=${(j:|:)_zsh_sense_capture_words} kinds=${(j:|:)_zsh_sense_item_kinds} flags=${(j:|:)_zsh_sense_capture_flags} prefixes=${(j:|:)_zsh_sense_capture_prefixes} selected=$_zsh_sense_selected backend=${_zsh_sense_item_acceptance_backends[_zsh_sense_selected]-} identity=${_zsh_sense_item_acceptance_identities[_zsh_sense_selected]-} serial=$_zsh_sense_capture_serial apply=${_zsh_sense_last_apply_status-unset} aligned=$render_aligned width=$render_width buffer=${(qqq)BUFFER} handler=${(qqq)handler} error=${_zsh_sense_last_error-}</STATE>"
  BUFFER=
  CURSOR=0
  zle accept-line
}
zle -N _zsh_sense_test_state
bindkey '^X^G' _zsh_sense_test_state

PS1='<SENSE-PROMPT>'
