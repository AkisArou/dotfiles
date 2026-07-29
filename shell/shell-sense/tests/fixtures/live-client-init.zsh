autoload -Uz compinit
compinit -u

_shell_sense_live_completion() {
  local -a descriptions=(
    '--all — stage modified and deleted files'
    '--amend — replace the previous commit'
  )
  compadd -J options -X 'command options' -d descriptions -- --all --amend
}
compdef _shell_sense_live_completion sense-test

_shell_sense_live_fuzzy_completion() {
  local -a descriptions=(
    'reload — reload unit configuration'
    'reset-failed — reset failed unit state'
    'restart — restart one or more units'
  )
  compadd -J subcommands -X 'service actions' -d descriptions -- reload reset-failed restart
}
compdef _shell_sense_live_fuzzy_completion sense-verb

_shell_sense_live_single_completion() {
  local -a descriptions=(
    '--force-with-lease — update only if the remote ref is unchanged'
  )
  compadd -J options -X 'single option' -d descriptions -- --force-with-lease
}
compdef _shell_sense_live_single_completion sense-single

_shell_sense_live_many_completion() {
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
compdef _shell_sense_live_many_completion sense-many

sense-test() {
  print -r -- "<EXEC>$*</EXEC>"
  print -r -- "<FINISH-POST>${#_shell_sense_test_finish_postdisplay}</FINISH-POST>"
}

sense-verb() {
  print -r -- "<VERB>$*</VERB>"
}

sense-many() {
  print -r -- "<MANY>$*</MANY>"
}

sense-single() {
  print -r -- "<SINGLE>$*</SINGLE>"
}

sense-tty() {
  local settings
  settings=$(command stty -a </dev/tty 2>/dev/null)
  if [[ $settings =~ '(^|[[:space:];])intr[[:space:]]*=[[:space:]]*\^C([;[:space:]]|$)' ]]; then
    print -r -- '<TTY-INT>enabled</TTY-INT>'
  else
    print -r -- '<TTY-INT>unexpected</TTY-INT>'
  fi
}

source "$SHELL_SENSE_TEST_ROOT/shell/zsh/shell-sense.plugin.zsh"

# Count physical popup erasures while still exercising the production
# implementation. This makes Ctrl-C cleanup observable in a PTY transcript,
# whose byte stream otherwise retains text that the terminal deleted.
typeset -gi _shell_sense_test_interrupt_erase_count=0
functions[_shell_sense_test_original_erase_edit_display]=$functions[_shell_sense_erase_edit_display]
_shell_sense_erase_edit_display() {
  (( _shell_sense_test_interrupt_erase_count++ ))
  _shell_sense_test_original_erase_edit_display "$@"
}
typeset -gi _shell_sense_test_key_dispatch_count=0
functions[_shell_sense_test_original_key_dispatch]=$functions[_shell_sense_key_dispatch]
_shell_sense_key_dispatch() {
  (( _shell_sense_test_key_dispatch_count++ ))
  _shell_sense_test_original_key_dispatch "$@"
}

# Observe line-finish after shell-sense. This verifies lifecycle state directly;
# raw PTY transcripts retain bytes that were subsequently erased and therefore
# cannot by themselves distinguish live screen content from scrollback.
typeset -g _shell_sense_test_finish_postdisplay=unobserved
_shell_sense_test_line_finish_observer() {
  _shell_sense_test_finish_postdisplay=$POSTDISPLAY
}
autoload -Uz add-zle-hook-widget
add-zle-hook-widget line-finish _shell_sense_test_line_finish_observer

_shell_sense_test_state() {
  local handler=
  local -i render_aligned=1 render_width=0 detail_duplicates=0 scrollbar_flush=1
  local -i index row_first=1 row_last=0
  local render_line
  if (( $#_shell_sense_render_lines )); then
    render_width=${#_shell_sense_render_lines[1]}
    for render_line in "${_shell_sense_render_lines[@]}"; do
      (( ${#render_line} == render_width )) || render_aligned=0
    done
  fi
  for (( index = 1; index <= $#_shell_sense_item_labels; index++ )); do
    [[ -n $_shell_sense_item_details[index] &&
       $_shell_sense_item_labels[index] == *"$_shell_sense_item_details[index]"* ]] &&
      (( detail_duplicates++ ))
  done
  row_last=$_shell_sense_render_menu_lines
  if [[ $_shell_sense_border != none ]]; then
    (( row_first++, row_last-- ))
  fi
  if (( _shell_sense_show_scrollbar &&
        _shell_sense_view_total > row_last - row_first + 1 )); then
    for (( index = row_first; index <= row_last; index++ )); do
      render_line=$_shell_sense_render_lines[index]
      if [[ $_shell_sense_documentation_placement == side &&
            _shell_sense_documentation_width > 0 ]]; then
        if [[ $_shell_sense_border == none ]]; then
          [[ $render_line[_shell_sense_menu_width] == $_shell_sense_scrollbar_character ]] || scrollbar_flush=0
        else
          [[ $render_line[$(( _shell_sense_menu_width - 1 ))] == $_shell_sense_scrollbar_character ]] || scrollbar_flush=0
        fi
      elif [[ $_shell_sense_border == none ]]; then
        [[ $render_line[-1] == $_shell_sense_scrollbar_character ]] || scrollbar_flush=0
      else
        [[ $render_line[-2] == $_shell_sense_scrollbar_character ]] || scrollbar_flush=0
      fi
    done
  fi
  (( _shell_sense_read_fd >= 0 )) && {
    handler=$(zle -F -L $_shell_sense_read_fd 2>/dev/null)
    _shell_sense_fd_callback $_shell_sense_read_fd
  }
  zle -I
  print -r -- "<STATE>ready=$_shell_sense_ready configured=$_shell_sense_configured read=$_shell_sense_read_fd write=$_shell_sense_write_fd worker=$_shell_sense_worker_pid fifo=$_shell_sense_fifo_in log=$_shell_sense_log_file request=$_shell_sense_active_request items=$#_shell_sense_item_ids captured=${(j:|:)_shell_sense_capture_words} kinds=${(j:|:)_shell_sense_item_kinds} flags=${(j:|:)_shell_sense_capture_flags} prefixes=${(j:|:)_shell_sense_capture_prefixes} selected=$_shell_sense_selected source=${_shell_sense_item_acceptance_sources[_shell_sense_selected]-} identity=${_shell_sense_item_acceptance_identities[_shell_sense_selected]-} serial=$_shell_sense_capture_serial apply=${_shell_sense_last_apply_status-unset} aligned=$render_aligned width=$render_width duplicates=$detail_duplicates flush=$scrollbar_flush doc-place=${_shell_sense_documentation_placement:-none} interrupt-key=$_shell_sense_interrupt_key_enabled terminal-interrupt=$_shell_sense_terminal_interrupt_disabled dispatches=$_shell_sense_test_key_dispatch_count erase=$_shell_sense_test_interrupt_erase_count buffer=${(qqq)BUFFER} handler=${(qqq)handler} error=${_shell_sense_last_error-}</STATE>"
  BUFFER=
  CURSOR=0
  zle accept-line
}
zle -N _shell_sense_test_state
bindkey '^X^G' _shell_sense_test_state

_shell_sense_test_ghost_state() {
  local stored=${_shell_sense_item_ghosts[_shell_sense_selected]-}
  _shell_sense_current_ghost
  print -r -- "<GHOST>value=${(qqq)REPLY} stored=${(qqq)stored} stale=$_shell_sense_popup_stale visible=$_shell_sense_popup_visible selected=$_shell_sense_selected cursor=$CURSOR length=$#BUFFER</GHOST>"
  zle -R
}
zle -N _shell_sense_test_ghost_state
bindkey '^X^H' _shell_sense_test_ghost_state

_shell_sense_test_documentation_state() {
  local text=${(j: :)_shell_sense_documentation_lines}
  print -r -- "<DOC>placement=${_shell_sense_documentation_placement:-none} text=${(qqq)text}</DOC>"
  zle -R
}
zle -N _shell_sense_test_documentation_state
bindkey '^X^D' _shell_sense_test_documentation_state

PS1='<SENSE-PROMPT>'
