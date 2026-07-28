autoload -Uz compinit
compinit -u
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%d'
source "$ZSH_SENSE_TEST_ROOT/shell/capture.zsh"
_zsh_sense_portable_init

_zsh_sense_test_completion() {
  local -a descriptions=(
    '--all — stage modified and deleted files'
    '--amend — replace the previous commit'
  )
  compadd -J options -X 'command options' -d descriptions -- --all --amend
}
compdef _zsh_sense_test_completion sense-test

_zsh_sense_fuzzy_test_completion() {
  compadd -- reload reset-failed restart
}
compdef _zsh_sense_fuzzy_test_completion sense-verb

_zsh_sense_test_widget() {
  local original_buffer=$BUFFER original_cursor=$CURSOR
  zle .zsh-sense-portable-capture
  BUFFER=$original_buffer
  CURSOR=$original_cursor

  if [[ $BUFFER == sense-verb* ]]; then
    print -r -- "<FUZZY-COUNT>$#_zsh_sense_capture_words</FUZZY-COUNT>"
    _zsh_sense_apply_serial=$_zsh_sense_capture_serial
    _zsh_sense_apply_index=1
    zle .zsh-sense-portable-apply
    print -r -- "<FUZZY-BUFFER>$BUFFER</FUZZY-BUFFER>"
    zle kill-whole-line
    zle accept-line
    return
  fi

  if [[ $BUFFER == ls* ]]; then
    local -i option_index=${_zsh_sense_capture_displays[(I)-a]}
    print -r -- "<LS-WORD>$_zsh_sense_capture_words[option_index]</LS-WORD>"
    print -r -- "<LS-DISPLAY>$_zsh_sense_capture_displays[option_index]</LS-DISPLAY>"
    print -r -- "<LS-DESC>$_zsh_sense_capture_descriptions[option_index]</LS-DESC>"
    _zsh_sense_apply_serial=$_zsh_sense_capture_serial
    _zsh_sense_apply_index=$option_index
    zle .zsh-sense-portable-apply
    print -r -- "<LS-BUFFER>$BUFFER</LS-BUFFER>"
    zle kill-whole-line
    zle accept-line
    return
  fi

  print -r -- "<COUNT>$#_zsh_sense_capture_words</COUNT>"
  print -r -- "<WORD1>$_zsh_sense_capture_words[1]</WORD1>"
  print -r -- "<WORD2>$_zsh_sense_capture_words[2]</WORD2>"
  print -r -- "<DESC2>$_zsh_sense_capture_descriptions[2]</DESC2>"
  print -r -- "<GROUP2>$_zsh_sense_capture_groups[2]</GROUP2>"
  print -r -- "<EXPL2>$_zsh_sense_capture_explanations[2]</EXPL2>"

  _zsh_sense_apply_serial=$_zsh_sense_capture_serial
  _zsh_sense_apply_index=2
  zle .zsh-sense-portable-apply
  print -r -- "<BUFFER>$BUFFER</BUFFER>"
  zle kill-whole-line
  zle accept-line
}
zle -N _zsh_sense_test_widget
bindkey '^I' _zsh_sense_test_widget

PS1='<SENSE-PROMPT>'
