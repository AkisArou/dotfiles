autoload -Uz compinit
compinit -u
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%d'
source "$SHELL_SENSE_TEST_ROOT/shell/zsh/provider.zsh"
_shell_sense_zsh_init

_shell_sense_test_completion() {
  local -a descriptions=(
    '--all — stage modified and deleted files'
    '--amend — replace the previous commit'
  )
  compadd -J options -X 'command options' -d descriptions -- --all --amend
}
compdef _shell_sense_test_completion sense-test

_shell_sense_fuzzy_test_completion() {
  compadd -- reload reset-failed restart
}
compdef _shell_sense_fuzzy_test_completion sense-verb

_shell_sense_conformance_completion() {
  local -a descriptions=(
    'restart:restart services'
    '--recursive:list subdirectories recursively'
  )
  _describe -t values 'conformance values' descriptions
}
compdef _shell_sense_conformance_completion shell-sense-conformance

_shell_sense_value_completion() {
  _arguments '--color=[output color]:color:(auto always never)'
}
compdef _shell_sense_value_completion shell-sense-value

compdef _directories shell-sense-path

_shell_sense_test_widget() {
  local original_buffer=$BUFFER original_cursor=$CURSOR
  zle .shell-sense-zsh-capture
  BUFFER=$original_buffer
  CURSOR=$original_cursor

  if [[ $BUFFER == shell-sense-conformance* || $BUFFER == shell-sense-value* ||
        $BUFFER == shell-sense-path* ]]; then
    local -i index
    for (( index = 1; index <= $#_shell_sense_capture_words; index++ )); do
      print -r -- "<CONFORMANCE>$_shell_sense_capture_words[index]|${_shell_sense_capture_kinds[index]:-text}|$_shell_sense_capture_resource_paths[index]</CONFORMANCE>"
    done
    zle kill-whole-line
    zle accept-line
    return
  fi

  if [[ $BUFFER == sense-verb* ]]; then
    print -r -- "<FUZZY-COUNT>$#_shell_sense_capture_words</FUZZY-COUNT>"
    _shell_sense_apply_serial=$_shell_sense_capture_serial
    _shell_sense_apply_index=1
    zle .shell-sense-zsh-apply
    print -r -- "<FUZZY-BUFFER>$BUFFER</FUZZY-BUFFER>"
    zle kill-whole-line
    zle accept-line
    return
  fi

  if [[ $BUFFER == ls* ]]; then
    local -i option_index=${_shell_sense_capture_displays[(I)-a]}
    print -r -- "<LS-WORD>$_shell_sense_capture_words[option_index]</LS-WORD>"
    print -r -- "<LS-DISPLAY>$_shell_sense_capture_displays[option_index]</LS-DISPLAY>"
    print -r -- "<LS-DESC>$_shell_sense_capture_descriptions[option_index]</LS-DESC>"
    _shell_sense_apply_serial=$_shell_sense_capture_serial
    _shell_sense_apply_index=$option_index
    zle .shell-sense-zsh-apply
    print -r -- "<LS-BUFFER>$BUFFER</LS-BUFFER>"
    zle kill-whole-line
    zle accept-line
    return
  fi

  if [[ $BUFFER == cd* ]]; then
    local -i directory_index=${_shell_sense_capture_kinds[(i)directory]}
    print -r -- "<PATH-RESOURCE>$_shell_sense_capture_resource_paths[directory_index]</PATH-RESOURCE>"
    _shell_sense_apply_serial=$_shell_sense_capture_serial
    _shell_sense_apply_index=$directory_index
    zle .shell-sense-zsh-apply
    print -r -- "<PATH-BUFFER>$BUFFER</PATH-BUFFER>"
    zle kill-whole-line
    zle accept-line
    return
  fi

  print -r -- "<COUNT>$#_shell_sense_capture_words</COUNT>"
  print -r -- "<WORD1>$_shell_sense_capture_words[1]</WORD1>"
  print -r -- "<WORD2>$_shell_sense_capture_words[2]</WORD2>"
  print -r -- "<DESC2>$_shell_sense_capture_descriptions[2]</DESC2>"
  print -r -- "<GROUP2>$_shell_sense_capture_groups[2]</GROUP2>"
  print -r -- "<EXPL2>$_shell_sense_capture_explanations[2]</EXPL2>"

  _shell_sense_apply_serial=$_shell_sense_capture_serial
  _shell_sense_apply_index=2
  zle .shell-sense-zsh-apply
  print -r -- "<BUFFER>$BUFFER</BUFFER>"
  zle kill-whole-line
  zle accept-line
}
zle -N _shell_sense_test_widget
bindkey '^I' _shell_sense_test_widget

PS1='<SENSE-PROMPT>'
