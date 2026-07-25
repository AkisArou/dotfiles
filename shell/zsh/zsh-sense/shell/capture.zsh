# Portable Zsh completion capture backend for zsh-sense.
#
# This is a permanent compatibility backend, not the native fast path. It
# uses documented completion APIs and therefore cannot observe a completion
# function that explicitly invokes `builtin compadd`. The planned native
# interception backend will cover that case; until then such calls are an
# explicit portable-mode limitation.

typeset -gi _zsh_sense_capture_serial=0
typeset -g _zsh_sense_capture_matcher='r:|?=**'
typeset -gi _zsh_sense_capture_fuzzy_min_chars=3
typeset -gi _zsh_sense_capture_call_count=0
typeset -ga _zsh_sense_capture_words=()
typeset -ga _zsh_sense_capture_displays=()
typeset -ga _zsh_sense_capture_descriptions=()
typeset -ga _zsh_sense_capture_groups=()
typeset -ga _zsh_sense_capture_explanations=()
typeset -ga _zsh_sense_capture_prefixes=()
typeset -ga _zsh_sense_capture_suffixes=()
typeset -ga _zsh_sense_capture_iprefixes=()
typeset -ga _zsh_sense_capture_isuffixes=()
typeset -ga _zsh_sense_capture_flags=()
typeset -ga _zsh_sense_capture_kinds=()
typeset -ga _zsh_sense_capture_calls=()
typeset -ga _zsh_sense_capture_positions=()
typeset -gi _zsh_sense_fast_command_handled=0
typeset -gi _zsh_sense_capture_is_fast_command=0
typeset -g _zsh_sense_fast_command_prefix=
typeset -g _zsh_sense_fast_command_suffix=
typeset -g _zsh_sense_fast_command_iprefix=
typeset -g _zsh_sense_fast_command_isuffix=
typeset -g _zsh_sense_apply_serial=
typeset -gi _zsh_sense_apply_index=0
typeset -gA _zsh_sense_describe_details=()

_zsh_sense_portable_fuzzy_matcher_active() {
  emulate -L zsh

  [[ -n $_zsh_sense_capture_matcher ]] || return 1
  local query=${PREFIX:-}
  query=${query##*/}
  (( $#query >= _zsh_sense_capture_fuzzy_min_chars ))
}

_zsh_sense_capture_reset() {
  emulate -L zsh

  local call
  for call in {1..$_zsh_sense_capture_call_count}; do
    unset "_zsh_sense_capture_args_$call"
  done
  (( _zsh_sense_capture_serial++ ))
  _zsh_sense_capture_call_count=0
  _zsh_sense_capture_words=()
  _zsh_sense_capture_displays=()
  _zsh_sense_capture_descriptions=()
  _zsh_sense_capture_groups=()
  _zsh_sense_capture_explanations=()
  _zsh_sense_capture_prefixes=()
  _zsh_sense_capture_suffixes=()
  _zsh_sense_capture_iprefixes=()
  _zsh_sense_capture_isuffixes=()
  _zsh_sense_capture_flags=()
  _zsh_sense_capture_kinds=()
  _zsh_sense_capture_calls=()
  _zsh_sense_capture_positions=()
  _zsh_sense_capture_is_fast_command=0
  _zsh_sense_fast_command_prefix=
  _zsh_sense_fast_command_suffix=
  _zsh_sense_fast_command_iprefix=
  _zsh_sense_fast_command_isuffix=
}

# Command names are an indexed completion source. Dispatching the universal
# completion system for the first word makes Zsh walk every command provider
# and can take seconds in a large interactive shell. Zsh already maintains the
# hashes below, and `compadd -A` filters them without populating or rendering
# the native completion list.
_zsh_sense_fast_command_capture() {
  emulate -L zsh
  setopt localoptions no_aliases extended_glob

  _zsh_sense_fast_command_handled=0
  (( CURRENT == 1 )) || return 1
  [[ -n $PREFIX && $PREFIX != */* ]] || return 1

  local -a external_hits builtin_hits alias_hits function_hits reserved_hits matcher
  if _zsh_sense_portable_fuzzy_matcher_active; then
    matcher=( -M "$_zsh_sense_capture_matcher" )
  fi
  # `compadd -k` reads associative-array keys in place. Expanding every hash
  # into a temporary array here copied the entire command table five times on
  # every first-word edit—the exact hot path for typing `cd`, `ls`, etc.
  builtin compadd -A alias_hits "${matcher[@]}" -k aliases
  builtin compadd -A function_hits "${matcher[@]}" -k functions
  builtin compadd -A builtin_hits "${matcher[@]}" -k builtins
  builtin compadd -A reserved_hits "${matcher[@]}" -k reswords
  builtin compadd -A external_hits "${matcher[@]}" -k commands

  _zsh_sense_capture_reset
  _zsh_sense_fast_command_handled=1
  _zsh_sense_capture_is_fast_command=1
  _zsh_sense_fast_command_prefix=$PREFIX
  _zsh_sense_fast_command_suffix=$SUFFIX
  _zsh_sense_fast_command_iprefix=$IPREFIX
  _zsh_sense_fast_command_isuffix=$ISUFFIX
  _zsh_sense_capture_call_count=1
  typeset -ga _zsh_sense_capture_args_1
  _zsh_sense_capture_args_1=()

  local -A seen=()
  local -a category_hits
  local category word kind
  local -i position=0
  for category in alias function builtin reserved external; do
    case $category in
      alias)
        category_hits=( "${alias_hits[@]}" )
        kind=alias
        ;;
      function)
        category_hits=( "${function_hits[@]}" )
        kind=function
        ;;
      builtin)
        category_hits=( "${builtin_hits[@]}" )
        kind=builtin
        ;;
      reserved)
        category_hits=( "${reserved_hits[@]}" )
        kind=text
        ;;
      external)
        category_hits=( "${external_hits[@]}" )
        kind=command
        ;;
    esac
    for word in "${category_hits[@]}"; do
      # This widget shares ZLE's event loop. Treat queued terminal input as a
      # cancellation token and return a deliberately partial (therefore
      # stale) capture; the edit waiting in ZLE will request the next
      # generation. Check at a small fixed cadence so a large command hash
      # cannot delay a fast typist.
      if (( position > 0 && position % 16 == 0 &&
            ( PENDING > 0 || KEYS_QUEUED_COUNT > 0 ) )); then
        compstate[insert]=
        compstate[list]=
        return 0
      fi
      (( $+seen[$word] )) && continue
      seen[$word]=1
      (( position++ ))
      _zsh_sense_capture_words+=( "$word" )
      _zsh_sense_capture_kinds+=( "$kind" )
    done
  done

  compstate[insert]=
  compstate[list]=
  return 0
}

_zsh_sense_portable_compadd() {
  emulate -L zsh
  setopt localoptions no_aliases extended_glob

  local -A output_options display_option
  local -a groups explanations messages file_option replay
  local -a hits displays source_displays capture_matcher
  local group explanation flags= description separator
  local return_status index word display call args_name

  zparseopts -E O:=output_options A:=output_options D:=output_options \
    d:=display_option X+:=explanations x+:=messages \
    J:=groups V:=groups f=file_option

  # -A/-O/-D calls ask compadd to return information to the caller and do not
  # add ordinary candidates. Capturing those calls would alter their meaning.
  if (( $#output_options )); then
    builtin compadd "$@"
    return
  fi

  if (( $#display_option == 1 )); then
    source_displays=( "${(@P)${(v)display_option}}" )
  fi
  displays=( "${source_displays[@]}" )
  (( $#groups >= 2 )) && group=$groups[2]
  (( $#explanations >= 2 )) && explanation=$explanations[2]
  (( $#file_option )) && flags+=f

  # Retain only insertion-affecting options. Candidate arrays, display arrays,
  # explanations, and filtering parameters are deliberately excluded because
  # the selected word has already passed those operations.
  zparseopts -a replay P: p: i: I: S: s: W: M+: r: R: f q e Q n U C l o:: 1 2
  replay=( "${(@)replay:#--}" )
  if _zsh_sense_portable_fuzzy_matcher_active; then
    capture_matcher=( -M "$_zsh_sense_capture_matcher" )
    replay+=( "${capture_matcher[@]}" )
  fi

  builtin compadd -A hits -D displays "${capture_matcher[@]}" "$@"
  return_status=$?
  if (( ! $#hits )); then
    (( $#messages >= 2 )) && builtin compadd -x "$messages[2]"
    return $return_status
  fi

  (( call = ++_zsh_sense_capture_call_count ))
  args_name="_zsh_sense_capture_args_$call"
  typeset -ga "$args_name"
  set -A "$args_name" "${replay[@]}"

  for index in {1..$#hits}; do
    word=$hits[index]
    display=$displays[index]
    [[ -n $display ]] || display=$source_displays[index]
    [[ -n $display ]] || display=$word
    description=
    if [[ $display == "$word"* ]]; then
      description=${display#$word}
      description=${description##[[:space:]]#}
      description=${description#'—'}
      description=${description#'–'}
      description=${description#-}
      description=${description##[[:space:]]#}
      separator=${description%%[[:space:]]*}
      if (( $#separator <= 3 )) && [[ $separator != [[:alnum:]]* ]]; then
        description=${description#$separator}
        description=${description##[[:space:]]#}
      fi
      display=$word
    fi
    [[ -n $description ]] || description=${_zsh_sense_describe_details[$word]-}
    _zsh_sense_capture_words+=( "$word" )
    _zsh_sense_capture_displays+=( "$display" )
    _zsh_sense_capture_descriptions+=( "$description" )
    _zsh_sense_capture_groups+=( "$group" )
    _zsh_sense_capture_explanations+=( "$explanation" )
    _zsh_sense_capture_prefixes+=( "$PREFIX" )
    _zsh_sense_capture_suffixes+=( "$SUFFIX" )
    _zsh_sense_capture_iprefixes+=( "$IPREFIX" )
    _zsh_sense_capture_isuffixes+=( "$ISUFFIX" )
    _zsh_sense_capture_flags+=( "$flags" )
    _zsh_sense_capture_kinds+=( '' )
    _zsh_sense_capture_calls+=( "$call" )
    _zsh_sense_capture_positions+=( "$index" )
  done

  # Keep Zsh's completion state valid while preventing this compatibility
  # layer from inventing its own matching or quoting behavior.
  builtin compadd "$@"
}

# `_describe` receives documented `completion:description` arrays before
# `compdescribe` flattens them into internal listing state. `compadd -D` does
# not expose those per-item descriptions on every path (notably `_arguments`),
# so retain the structure while the original `_describe` executes and let the
# `compadd` wrapper correlate it with the actual matches.
_zsh_sense_capture_describe_metadata() {
  emulate -L zsh
  setopt localoptions no_aliases extended_glob
  _zsh_sense_describe_details=()

  local -a arguments=( "$@" ) specifications insertions
  local token name description word
  local -i cursor=1 index separator position escaped

  # Skip `_describe` options; only `-t` consumes an argument.
  while (( cursor <= $#arguments )) && [[ $arguments[cursor] == -* ]]; do
    token=$arguments[cursor]
    [[ $token == -- ]] && { (( cursor++ )); break; }
    if [[ $token == -t ]]; then
      (( cursor += 2 ))
    else
      (( cursor++ ))
    fi
  done
  # The first non-option is the heading, followed by name1.
  (( cursor++ ))

  while (( cursor <= $#arguments )); do
    name=$arguments[cursor]
    (( cursor++ ))
    if [[ $name == \(*\) ]]; then
      specifications=( ${(z)name[2,-2]} )
    else
      specifications=( "${(@P)name}" )
    fi

    insertions=()
    if (( cursor <= $#arguments )) && [[ $arguments[cursor] != -* && $arguments[cursor] != -- ]]; then
      name=$arguments[cursor]
      (( cursor++ ))
      if [[ $name == \(*\) ]]; then
        insertions=( ${(z)name[2,-2]} )
      else
        insertions=( "${(@P)name}" )
      fi
    fi

    for (( index = 1; index <= $#specifications; index++ )); do
      token=$specifications[index]
      separator=0
      escaped=0
      for (( position = 1; position <= $#token; position++ )); do
        if (( escaped )); then
          escaped=0
        elif [[ $token[position] == \\ ]]; then
          escaped=1
        elif [[ $token[position] == : ]]; then
          separator=$position
          break
        fi
      done
      (( separator )) || continue
      if (( $#insertions >= index )); then
        word=$insertions[index]
      else
        word=$token[1,$(( separator - 1 ))]
        word=${word//\\:/:}
        word=${word//\\\\/\\}
      fi
      description=$token[$(( separator + 1 )),-1]
      description=${description//\\:/:}
      [[ -n $word && -n $description ]] && _zsh_sense_describe_details[$word]=$description
    done

    # Remaining arguments in this group are `compadd` options. The next `--`
    # starts another documented name1/name2/options group.
    while (( cursor <= $#arguments )) && [[ $arguments[cursor] != -- ]]; do
      (( cursor++ ))
    done
    (( cursor <= $#arguments )) && (( cursor++ ))
  done
}

_zsh_sense_portable_describe() {
  _zsh_sense_capture_describe_metadata "$@"
  _zsh_sense_original_describe "$@"
  local return_status=$?
  _zsh_sense_describe_details=()
  return $return_status
}

# `_main_complete` obtains its global matching policy through this documented
# style lookup before completion functions run. Intercepting only that lookup
# lets built-in helpers such as `_path_files` see the same candidate-universe
# matcher as ordinary `compadd` calls, without mutating the user's persistent
# zstyles or replacing any command-specific completion function.
_zsh_sense_portable_zstyle() {
  emulate -L zsh
  setopt localoptions no_aliases

  if [[ $1 == -a && $3 == matcher-list ]]; then
    local destination=$4
    typeset -ga "$destination"
    if _zsh_sense_portable_fuzzy_matcher_active; then
      set -A "$destination" "$_zsh_sense_capture_matcher"
    else
      # The user's ordinary matcher-list may itself be broad. Keep the
      # automatic short-query universe prefix-strict regardless, then let the
      # Rust ranker provide fuzzy ordering over that bounded universe.
      set -A "$destination" ''
    fi
    return 0
  fi
  builtin zstyle "$@"
}

_zsh_sense_portable_capture() {
  local had_compadd=$+functions[compadd]
  local saved_compadd=${functions[compadd]-}
  local had_zstyle=$+functions[zstyle]
  local saved_zstyle=${functions[zstyle]-}
  autoload -Uz +X _describe 2>/dev/null
  local had_describe=$+functions[_describe]
  local saved_describe=${functions[_describe]-}
  local return_status=0

  _zsh_sense_capture_reset
  functions[compadd]=$functions[_zsh_sense_portable_compadd]
  functions[zstyle]=$functions[_zsh_sense_portable_zstyle]
  if (( had_describe )); then
    functions[_zsh_sense_original_describe]=$saved_describe
    functions[_describe]=$functions[_zsh_sense_portable_describe]
  fi
  {
    _main_complete "$@" || return_status=$?
  } always {
    if (( had_compadd )); then
      functions[compadd]=$saved_compadd
    else
      unfunction compadd 2>/dev/null
    fi
    if (( had_zstyle )); then
      functions[zstyle]=$saved_zstyle
    else
      unfunction zstyle 2>/dev/null
    fi
    if (( had_describe )); then
      functions[_describe]=$saved_describe
      unfunction _zsh_sense_original_describe 2>/dev/null
    fi
    _zsh_sense_describe_details=()
  }

  # The ordinary widget that invokes this completion widget also restores its
  # BUFFER/CURSOR snapshot. These assignments stop normal listing/menu state.
  compstate[insert]=
  compstate[list]=
  return $return_status
}

_zsh_sense_portable_apply() {
  emulate -L zsh

  [[ $_zsh_sense_apply_serial == $_zsh_sense_capture_serial ]] || return 1
  (( _zsh_sense_apply_index >= 1 && _zsh_sense_apply_index <= $#_zsh_sense_capture_words )) || return 1

  local word=$_zsh_sense_capture_words[_zsh_sense_apply_index]
  if (( _zsh_sense_capture_is_fast_command )); then
    PREFIX=$_zsh_sense_fast_command_prefix
    SUFFIX=$_zsh_sense_fast_command_suffix
    IPREFIX=$_zsh_sense_fast_command_iprefix
    ISUFFIX=$_zsh_sense_fast_command_isuffix
    builtin compadd -- "$word" || return 1
    compstate[list]=
    compstate[insert]='1'
    [[ $RBUFFER == ' '* ]] || compstate[insert]+=' '
    return 0
  fi
  local call=$_zsh_sense_capture_calls[_zsh_sense_apply_index]
  local args_name="_zsh_sense_capture_args_$call"
  local -a replay=( "${(@P)args_name}" )

  PREFIX=$_zsh_sense_capture_prefixes[_zsh_sense_apply_index]
  SUFFIX=$_zsh_sense_capture_suffixes[_zsh_sense_apply_index]
  IPREFIX=$_zsh_sense_capture_iprefixes[_zsh_sense_apply_index]
  ISUFFIX=$_zsh_sense_capture_isuffixes[_zsh_sense_apply_index]
  builtin compadd "${replay[@]}" -- "$word" || return 1
  compstate[list]=
  compstate[insert]='1'
  [[ $RBUFFER == ' '* ]] || compstate[insert]+=' '
}

_zsh_sense_portable_init() {
  emulate -L zsh

  (( $+functions[_main_complete] )) || autoload -Uz _main_complete
  (( $+builtins[zparseopts] )) || zmodload zsh/zutil
  zle -C .zsh-sense-portable-capture complete-word _zsh_sense_portable_capture
  zle -C .zsh-sense-portable-apply complete-word _zsh_sense_portable_apply
  zle -C .zsh-sense-fast-command-capture complete-word _zsh_sense_fast_command_capture
}
