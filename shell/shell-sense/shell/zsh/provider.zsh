# Zsh completion provider for Shell Sense.
#
# This provider uses documented completion APIs and therefore cannot observe a
# completion function that explicitly invokes `builtin compadd`. Zsh exports
# no supported interception hook for that path, so such calls are an explicit
# provider limitation rather than a reason to modify Zsh's builtin table.

typeset -gi _shell_sense_capture_serial=0
typeset -g _shell_sense_capture_matcher='r:|?=**'
typeset -gi _shell_sense_capture_fuzzy_min_chars=3
typeset -gi _shell_sense_capture_call_count=0
typeset -ga _shell_sense_capture_words=()
typeset -ga _shell_sense_capture_displays=()
typeset -ga _shell_sense_capture_descriptions=()
typeset -ga _shell_sense_capture_groups=()
typeset -ga _shell_sense_capture_explanations=()
typeset -ga _shell_sense_capture_prefixes=()
typeset -ga _shell_sense_capture_suffixes=()
typeset -ga _shell_sense_capture_iprefixes=()
typeset -ga _shell_sense_capture_isuffixes=()
typeset -ga _shell_sense_capture_flags=()
typeset -ga _shell_sense_capture_kinds=()
typeset -ga _shell_sense_capture_resource_paths=()
typeset -ga _shell_sense_capture_calls=()
typeset -ga _shell_sense_capture_positions=()
typeset -gi _shell_sense_fast_command_handled=0
typeset -gi _shell_sense_capture_is_fast_command=0
typeset -g _shell_sense_fast_command_prefix=
typeset -g _shell_sense_fast_command_suffix=
typeset -g _shell_sense_fast_command_iprefix=
typeset -g _shell_sense_fast_command_isuffix=
typeset -g _shell_sense_apply_serial=
typeset -gi _shell_sense_apply_index=0
typeset -gA _shell_sense_describe_details=()
typeset -ga _shell_sense_native_context_words=()
typeset -gi _shell_sense_native_context_current=-1

_shell_sense_zsh_fuzzy_matcher_active() {
  emulate -L zsh

  [[ -n $_shell_sense_capture_matcher ]] || return 1
  local query=${PREFIX:-}
  query=${query##*/}
  (( $#query >= _shell_sense_capture_fuzzy_min_chars ))
}

_shell_sense_display_without_description() {
  emulate -L zsh
  setopt localoptions extended_glob

  local display=$1 description=$2 label separator
  REPLY=$display
  [[ -n $description && $display == *"$description" ]] || return 0
  label=${display%"$description"}
  label=${label%%[[:space:]]#}
  separator=${label##*[[:space:]]}
  case $separator in
    --|—|–|-|:)
      label=${label%$separator}
      label=${label%%[[:space:]]#}
      ;;
  esac
  [[ -n $label ]] && REPLY=$label
}

_shell_sense_capture_reset() {
  emulate -L zsh

  local call
  for call in {1..$_shell_sense_capture_call_count}; do
    unset "_shell_sense_capture_args_$call"
  done
  (( _shell_sense_capture_serial++ ))
  _shell_sense_capture_call_count=0
  _shell_sense_capture_words=()
  _shell_sense_capture_displays=()
  _shell_sense_capture_descriptions=()
  _shell_sense_capture_groups=()
  _shell_sense_capture_explanations=()
  _shell_sense_capture_prefixes=()
  _shell_sense_capture_suffixes=()
  _shell_sense_capture_iprefixes=()
  _shell_sense_capture_isuffixes=()
  _shell_sense_capture_flags=()
  _shell_sense_capture_kinds=()
  _shell_sense_capture_resource_paths=()
  _shell_sense_capture_calls=()
  _shell_sense_capture_positions=()
  _shell_sense_capture_is_fast_command=0
  _shell_sense_fast_command_prefix=
  _shell_sense_fast_command_suffix=
  _shell_sense_fast_command_iprefix=
  _shell_sense_fast_command_isuffix=
}

# Command names are an indexed completion source. Dispatching the universal
# completion system for the first word makes Zsh walk every command provider
# and can take seconds in a large interactive shell. Zsh already maintains the
# hashes below, and `compadd -A` filters them without populating or rendering
# the native completion list.
_shell_sense_fast_command_capture() {
  emulate -L zsh
  setopt localoptions no_aliases extended_glob

  _shell_sense_fast_command_handled=0
  (( CURRENT == 1 )) || return 1
  [[ -n $PREFIX && $PREFIX != */* ]] || return 1

  local -a external_hits builtin_hits alias_hits function_hits reserved_hits matcher
  if _shell_sense_zsh_fuzzy_matcher_active; then
    matcher=( -M "$_shell_sense_capture_matcher" )
  fi
  # `compadd -k` reads associative-array keys in place. Expanding every hash
  # into a temporary array here copied the entire command table five times on
  # every first-word edit—the exact hot path for typing `cd`, `ls`, etc.
  builtin compadd -A alias_hits "${matcher[@]}" -k aliases
  builtin compadd -A function_hits "${matcher[@]}" -k functions
  builtin compadd -A builtin_hits "${matcher[@]}" -k builtins
  builtin compadd -A reserved_hits "${matcher[@]}" -k reswords
  builtin compadd -A external_hits "${matcher[@]}" -k commands

  _shell_sense_capture_reset
  _shell_sense_native_context_words=( "${words[@]}" )
  _shell_sense_native_context_current=$(( CURRENT - 1 ))
  _shell_sense_fast_command_handled=1
  _shell_sense_capture_is_fast_command=1
  _shell_sense_fast_command_prefix=$PREFIX
  _shell_sense_fast_command_suffix=$SUFFIX
  _shell_sense_fast_command_iprefix=$IPREFIX
  _shell_sense_fast_command_isuffix=$ISUFFIX
  _shell_sense_capture_call_count=1
  typeset -ga _shell_sense_capture_args_1
  _shell_sense_capture_args_1=()

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
      _shell_sense_capture_words+=( "$word" )
      _shell_sense_capture_kinds+=( "$kind" )
    done
  done

  compstate[insert]=
  compstate[list]=
  return 0
}

_shell_sense_zsh_compadd() {
  emulate -L zsh
  setopt localoptions no_aliases extended_glob

  local -A output_options display_option
  local -a groups explanations messages file_option replay
  local -a hits displays source_displays capture_matcher
  local group explanation flags= description separator kind candidate_path path_prefix resource_path
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
  (( ${replay[(I)-f]} )) && flags=f
  if _shell_sense_zsh_fuzzy_matcher_active; then
    capture_matcher=( -M "$_shell_sense_capture_matcher" )
    replay+=( "${capture_matcher[@]}" )
  fi

  builtin compadd -A hits -D displays "${capture_matcher[@]}" "$@"
  return_status=$?
  if (( ! $#hits )); then
    (( $#messages >= 2 )) && builtin compadd -x "$messages[2]"
    return $return_status
  fi

  (( call = ++_shell_sense_capture_call_count ))
  args_name="_shell_sense_capture_args_$call"
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
    [[ -n $description ]] || description=${_shell_sense_describe_details[$word]-}
    # Zsh may expose a transformed insertion and a presentation-only display.
    # For `ls -l`, for example, the insertion is `-la` while the display is
    # `-a -- list entries…`. Use the structured description as an exact suffix
    # boundary instead of assuming that display starts with insertion.
    _shell_sense_display_without_description "$display" "$description"
    display=$REPLY
    candidate_path=$word
    if [[ $PREFIX == */* && $word != /* ]]; then
      path_prefix=${PREFIX%/*}/
      [[ $word == "$path_prefix"* ]] || candidate_path="$path_prefix$word"
    fi
    kind=
    if [[ $flags == *f* ]]; then
      # `_path_files` can add files and directories through the same `-f`
      # compadd call. Check each candidate rather than inferring its kind from
      # prose in the description; a remaining file-marked match is a file.
      if [[ -d $candidate_path ]]; then kind=directory; else kind=file; fi
    elif [[ $word == -* ]]; then
      kind=option
    elif [[ -d $candidate_path ]]; then
      kind=directory
    elif [[ ${description:l} == 'local directory' ||
            ${description:l} == 'remote directory' ||
            ${description:l} == directory ]]; then
      kind=directory
    fi
    # Some native file groups describe every entry as "file" even though
    # `_path_files` returned a directory. Keep the native candidate and edit,
    # but present the more specific kind we just established from that path.
    if [[ $kind == directory && ${description:l} == file ]]; then
      description=directory
    fi
    resource_path=
    if [[ $kind == file || $kind == directory || $kind == symlink ]]; then
      resource_path=$candidate_path
      if [[ $resource_path == '~' ]]; then
        resource_path=$HOME
      elif [[ $resource_path == '~/'* ]]; then
        resource_path=$HOME/${resource_path#\~/}
      elif [[ $resource_path != /* ]]; then
        resource_path=$PWD/$resource_path
      fi
    fi
    _shell_sense_capture_words+=( "$word" )
    _shell_sense_capture_displays+=( "$display" )
    _shell_sense_capture_descriptions+=( "$description" )
    _shell_sense_capture_groups+=( "$group" )
    _shell_sense_capture_explanations+=( "$explanation" )
    _shell_sense_capture_prefixes+=( "$PREFIX" )
    _shell_sense_capture_suffixes+=( "$SUFFIX" )
    _shell_sense_capture_iprefixes+=( "$IPREFIX" )
    _shell_sense_capture_isuffixes+=( "$ISUFFIX" )
    _shell_sense_capture_flags+=( "$flags" )
    _shell_sense_capture_kinds+=( "$kind" )
    _shell_sense_capture_resource_paths+=( "$resource_path" )
    _shell_sense_capture_calls+=( "$call" )
    _shell_sense_capture_positions+=( "$index" )
  done

  # Keep Zsh's completion state valid while preventing the capture layer from
  # inventing its own matching or quoting behavior.
  builtin compadd "$@"
}

# `_describe` receives documented `completion:description` arrays before
# `compdescribe` flattens them into internal listing state. `compadd -D` does
# not expose those per-item descriptions on every path (notably `_arguments`),
# so retain the structure while the original `_describe` executes and let the
# `compadd` wrapper correlate it with the actual matches.
_shell_sense_capture_describe_metadata() {
  emulate -L zsh
  setopt localoptions no_aliases extended_glob
  _shell_sense_describe_details=()

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
      [[ -n $word && -n $description ]] && _shell_sense_describe_details[$word]=$description
    done

    # Remaining arguments in this group are `compadd` options. The next `--`
    # starts another documented name1/name2/options group.
    while (( cursor <= $#arguments )) && [[ $arguments[cursor] != -- ]]; do
      (( cursor++ ))
    done
    (( cursor <= $#arguments )) && (( cursor++ ))
  done
}

_shell_sense_zsh_describe() {
  _shell_sense_capture_describe_metadata "$@"
  _shell_sense_original_describe "$@"
  local return_status=$?
  _shell_sense_describe_details=()
  return $return_status
}

# `_main_complete` obtains its global matching policy through this documented
# style lookup before completion functions run. Intercepting only that lookup
# lets built-in helpers such as `_path_files` see the same candidate-universe
# matcher as ordinary `compadd` calls, without mutating the user's persistent
# zstyles or replacing any command-specific completion function.
_shell_sense_zsh_zstyle() {
  emulate -L zsh
  setopt localoptions no_aliases

  if [[ $1 == -a && $3 == matcher-list ]]; then
    local destination=$4
    typeset -ga "$destination"
    if _shell_sense_zsh_fuzzy_matcher_active; then
      set -A "$destination" "$_shell_sense_capture_matcher"
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

_shell_sense_zsh_capture() {
  local had_compadd=$+functions[compadd]
  local saved_compadd=${functions[compadd]-}
  local had_zstyle=$+functions[zstyle]
  local saved_zstyle=${functions[zstyle]-}
  autoload -Uz +X _describe 2>/dev/null
  local had_describe=$+functions[_describe]
  local saved_describe=${functions[_describe]-}
  local return_status=0

  _shell_sense_capture_reset
  functions[compadd]=$functions[_shell_sense_zsh_compadd]
  functions[zstyle]=$functions[_shell_sense_zsh_zstyle]
  if (( had_describe )); then
    functions[_shell_sense_original_describe]=$saved_describe
    functions[_describe]=$functions[_shell_sense_zsh_describe]
  fi
  {
    _main_complete "$@" || return_status=$?
    _shell_sense_native_context_words=( "${words[@]}" )
    _shell_sense_native_context_current=$(( CURRENT - 1 ))
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
      unfunction _shell_sense_original_describe 2>/dev/null
    fi
    _shell_sense_describe_details=()
  }

  # The ordinary widget that invokes this completion widget also restores its
  # BUFFER/CURSOR snapshot. These assignments stop normal listing/menu state.
  compstate[insert]=
  compstate[list]=
  return $return_status
}

_shell_sense_zsh_apply() {
  emulate -L zsh

  [[ $_shell_sense_apply_serial == $_shell_sense_capture_serial ]] || return 1
  (( _shell_sense_apply_index >= 1 && _shell_sense_apply_index <= $#_shell_sense_capture_words )) || return 1

  local word=$_shell_sense_capture_words[_shell_sense_apply_index]
  if (( _shell_sense_capture_is_fast_command )); then
    PREFIX=$_shell_sense_fast_command_prefix
    SUFFIX=$_shell_sense_fast_command_suffix
    IPREFIX=$_shell_sense_fast_command_iprefix
    ISUFFIX=$_shell_sense_fast_command_isuffix
    builtin compadd -- "$word" || return 1
    compstate[list]=
    compstate[insert]='1'
    [[ $RBUFFER == ' '* ]] || compstate[insert]+=' '
    return 0
  fi
  local call=$_shell_sense_capture_calls[_shell_sense_apply_index]
  local args_name="_shell_sense_capture_args_$call"
  local -a replay=( "${(@P)args_name}" )
  local apply_word=$word

  PREFIX=$_shell_sense_capture_prefixes[_shell_sense_apply_index]
  SUFFIX=$_shell_sense_capture_suffixes[_shell_sense_apply_index]
  IPREFIX=$_shell_sense_capture_iprefixes[_shell_sense_apply_index]
  ISUFFIX=$_shell_sense_capture_isuffixes[_shell_sense_apply_index]
  if [[ $_shell_sense_capture_flags[_shell_sense_apply_index] == *f* && $PREFIX == */* &&
        ${replay[(I)-p]} == 0 && ${replay[(I)-P]} == 0 ]]; then
    # `_path_files` exposes a basename match while PREFIX still contains the
    # already typed parent path. Replaying only the basename makes compadd try
    # to match `dotfiles/` against `alacritty`, which it correctly rejects.
    # Reconstitute the candidate exactly as it appeared in the command word
    # when the original call did not already carry an explicit prefix option;
    # compadd still owns quoting, suffix handling, and directory slash logic.
    local path_prefix=${PREFIX%/*}/
    [[ $word == "$path_prefix"* ]] || apply_word="$path_prefix$word"
  fi
  builtin compadd "${replay[@]}" -- "$apply_word" || return 1
  compstate[list]=
  compstate[insert]='1'
  [[ $RBUFFER == ' '* ]] || compstate[insert]+=' '
}

_shell_sense_zsh_init() {
  emulate -L zsh

  (( $+functions[_main_complete] )) || autoload -Uz _main_complete
  (( $+builtins[zparseopts] )) || zmodload zsh/zutil
  zle -C .shell-sense-zsh-capture complete-word _shell_sense_zsh_capture
  zle -C .shell-sense-zsh-apply complete-word _shell_sense_zsh_apply
  zle -C .shell-sense-fast-command-capture complete-word _shell_sense_fast_command_capture
}
