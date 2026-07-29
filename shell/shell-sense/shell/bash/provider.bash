# Native Bash programmable-completion provider for Shell Sense.
#
# Candidate authority stays inside the live Bash process. Registered
# compspecs, bash-completion functions, and Bash's own `compgen` actions are
# the only mechanisms allowed to populate the result arrays below.

declare -ga _shell_sense_bash_candidates=()
declare -ga _shell_sense_bash_insertions=()
declare -ga _shell_sense_bash_descriptions=()
declare -ga _shell_sense_bash_kinds=()
declare -ga _shell_sense_bash_resource_paths=()
declare -ga _shell_sense_bash_append_spaces=()
declare -ga _shell_sense_bash_acceptance_identities=()
declare -ga _shell_sense_bash_words=()
declare -gi _shell_sense_bash_cword=0
declare -gi _shell_sense_bash_replace_char_start=0
declare -gi _shell_sense_bash_replace_char_end=0
declare -g _shell_sense_bash_comp_line=
declare -g _shell_sense_bash_command=
declare -g _shell_sense_bash_query_mode=exact
declare -g _shell_sense_bash_dequoted_word=
declare -g _shell_sense_bash_open_quote=

_shell_sense_bash_byte_length() {
  local LC_ALL=C
  _shell_sense_bash_byte_count=${#1}
}

_shell_sense_bash_is_wordbreak() {
  [[ $COMP_WORDBREAKS == *"$1"* ]]
}

_shell_sense_bash_is_assignment_word() {
  [[ $1 =~ ^[a-zA-Z_][a-zA-Z0-9_]*(\+)?= ]]
}

_shell_sense_bash_assignment_prefix() {
  local segment=$1 quote="" character="" token=""
  local -i offset=0 index=0 escaped=0 length=${#segment} next_start=0
  _shell_sense_bash_assignment_prefix_length=0
  while ((offset < length)); do
    quote=""
    escaped=0
    index=$offset
    for ((; index < length; index++)); do
      character=${segment:index:1}
      if ((escaped)); then
        escaped=0
        continue
      fi
      if [[ $quote != "'" && $character == \\ ]]; then
        escaped=1
        continue
      fi
      if [[ -n $quote ]]; then
        [[ $character == "$quote" ]] && quote=""
        continue
      fi
      case $character in
        "'"|'"'|'`') quote=$character ;;
        ' '|$'\t'|$'\n') break ;;
      esac
    done
    token=${segment:offset:index-offset}
    _shell_sense_bash_is_assignment_word "$token" || break
    next_start=$index
    while ((next_start < length)); do
      character=${segment:next_start:1}
      [[ $character == ' ' || $character == $'\t' || $character == $'\n' ]] || break
      ((next_start += 1))
    done
    ((next_start < length)) || break
    offset=$next_start
    _shell_sense_bash_assignment_prefix_length=$offset
  done
}

# Decode one lexical completion word without evaluating expansions. The raw
# COMP_WORDS value is still passed to programmable-completion functions; the
# decoded form is used only by Bash's built-in compgen actions.
_shell_sense_bash_dequote_word() {
  local word=$1 quote="" character="" next=""
  local -i escaped=0 index length=${#word}
  _shell_sense_bash_dequoted_word=
  _shell_sense_bash_open_quote=
  for ((index = 0; index < length; index++)); do
    character=${word:index:1}
    if ((escaped)); then
      _shell_sense_bash_dequoted_word+=$character
      escaped=0
      continue
    fi
    if [[ -n $quote ]]; then
      if [[ $character == "$quote" ]]; then
        quote=
      elif [[ $quote == "'" ]]; then
        _shell_sense_bash_dequoted_word+=$character
      elif [[ $character == \\ ]]; then
        next=${word:index+1:1}
        if [[ $next == '$' || $next == '`' || $next == '"' || $next == \\ || $next == $'\n' ]]; then
          escaped=1
        else
          _shell_sense_bash_dequoted_word+=$character
        fi
      else
        _shell_sense_bash_dequoted_word+=$character
      fi
      continue
    fi
    case $character in
      "'"|'"'|'`') quote=$character ;;
      \\) escaped=1 ;;
      *) _shell_sense_bash_dequoted_word+=$character ;;
    esac
  done
  [[ $quote == "'" || $quote == '"' ]] && _shell_sense_bash_open_quote=$quote
  return 0
}

_shell_sense_bash_quote_filename() {
  local candidate=$1 quote=$2
  local -i directory=$3
  if [[ $quote == "'" && $candidate != *"'"* ]]; then
    _shell_sense_bash_quoted_filename="'$candidate"
    ((directory)) || _shell_sense_bash_quoted_filename+="'"
  elif [[ $quote == '"' ]]; then
    local escaped=$candidate
    escaped=${escaped//\\/\\\\}
    escaped=${escaped//\"/\\\"}
    escaped=${escaped//\$/\\\$}
    escaped=${escaped//\`/\\\`}
    _shell_sense_bash_quoted_filename="\"$escaped"
    ((directory)) || _shell_sense_bash_quoted_filename+='"'
  else
    printf -v _shell_sense_bash_quoted_filename '%q' "$candidate"
  fi
}

# Reconstruct the public completion variables for an arbitrary Readline line.
# Bash does not publish an API for this operation. This scanner deliberately
# implements only Bash's lexical completion boundary: quoting, escaping,
# command separators, whitespace, and the active COMP_WORDBREAKS value. It
# does not evaluate or expand any part of the command line.
_shell_sense_bash_context() {
  local line=$1
  local -i point=$2
  local -i length=${#line}
  (( point >= 0 && point <= length )) || return 1

  local quote="" character="" previous=""
  local -i escaped=0 segment_start=0 index
  for ((index = 0; index < point; index++)); do
    character=${line:index:1}
    if (( escaped )); then
      escaped=0
      continue
    fi
    if [[ $quote != "'" && $character == \\ ]]; then
      escaped=1
      continue
    fi
    if [[ -n $quote ]]; then
      [[ $character == "$quote" ]] && quote=
      continue
    fi
    case $character in
      "'"|'"'|'`') quote=$character ;;
      '|'|'&'|';'|$'\n'|'(') segment_start=$((index + 1)) ;;
    esac
    previous=$character
  done

  while (( segment_start < point )); do
    character=${line:segment_start:1}
    [[ $character == ' ' || $character == $'\t' ]] || break
    ((segment_start += 1))
  done

  _shell_sense_bash_comp_line=${line:segment_start:point-segment_start}
  local segment=$_shell_sense_bash_comp_line
  _shell_sense_bash_assignment_prefix "$segment"
  if ((_shell_sense_bash_assignment_prefix_length > 0)); then
    segment_start=$((segment_start + _shell_sense_bash_assignment_prefix_length))
    segment=${segment:_shell_sense_bash_assignment_prefix_length}
    _shell_sense_bash_comp_line=$segment
  fi
  local -i segment_length=${#segment}
  _shell_sense_bash_words=()
  local token="" token_start=0
  local -a token_starts=()
  quote=
  escaped=0
  for ((index = 0; index < segment_length; index++)); do
    character=${segment:index:1}
    if (( escaped )); then
      token+=$character
      escaped=0
      continue
    fi
    if [[ $quote != "'" && $character == \\ ]]; then
      token+=$character
      escaped=1
      continue
    fi
    if [[ -n $quote ]]; then
      token+=$character
      [[ $character == "$quote" ]] && quote=
      continue
    fi
    case $character in
      "'"|'"'|'`')
        [[ -n $token ]] || token_start=$index
        token+=$character
        quote=$character
        ;;
      ' '|$'\t'|$'\n')
        if [[ -n $token ]]; then
          _shell_sense_bash_words+=("$token")
          token_starts+=("$token_start")
          token=
        fi
        ;;
      *)
        if _shell_sense_bash_is_wordbreak "$character"; then
          if [[ -n $token ]]; then
            _shell_sense_bash_words+=("$token")
            token_starts+=("$token_start")
            token=
          fi
          _shell_sense_bash_words+=("$character")
          token_starts+=("$index")
        else
          [[ -n $token ]] || token_start=$index
          token+=$character
        fi
        ;;
    esac
  done
  if [[ -n $token ]]; then
    _shell_sense_bash_words+=("$token")
    token_starts+=("$token_start")
  elif (( segment_length == 0 )) || [[ ${segment: -1} == ' ' || ${segment: -1} == $'\t' || ${segment: -1} == $'\n' ]]; then
    _shell_sense_bash_words+=("")
    token_starts+=("$segment_length")
  fi

  if ((${#_shell_sense_bash_words[@]} == 0)); then
    _shell_sense_bash_words=("")
    token_starts=(0)
  fi

  _shell_sense_bash_cword=$((${#_shell_sense_bash_words[@]} - 1))
  _shell_sense_bash_replace_char_start=$((segment_start + token_starts[_shell_sense_bash_cword]))
  _shell_sense_bash_replace_char_end=$point

  _shell_sense_bash_dequote_word "${_shell_sense_bash_words[0]}"
  _shell_sense_bash_command=$_shell_sense_bash_dequoted_word
}

_shell_sense_bash_structural_prefix() {
  local token=$1
  if [[ $token == */* ]]; then
    printf '%s' "${token%/*}/"
  elif [[ $token == *=* ]]; then
    printf '%s' "${token%=*}="
  elif [[ $token == --* ]]; then
    printf '%s' -- '--'
  elif [[ $token == -* ]]; then
    printf '%s' '-'
  elif [[ $token == +* ]]; then
    printf '%s' '+'
  elif [[ $token == \$* ]]; then
    printf '%s' '$'
  elif [[ $token == \~* ]]; then
    printf '%s' '~'
  fi
}

_shell_sense_bash_find_compspec() {
  local command=$1 rendered=
  if [[ -z $_shell_sense_bash_comp_line ]]; then
    rendered=$(builtin complete -p -E 2>/dev/null) || rendered=
  elif (( _shell_sense_bash_cword == 0 )); then
    rendered=$(builtin complete -p -I 2>/dev/null) || rendered=
  fi
  if [[ -z $rendered ]] && ! rendered=$(builtin complete -p -- "$command" 2>/dev/null); then
    local basename=${command##*/}
    [[ $basename == "$command" ]] || rendered=$(builtin complete -p -- "$basename" 2>/dev/null) || rendered=
  fi
  if [[ -z $rendered ]] && declare -F _completion_loader >/dev/null; then
    _completion_loader "$command" >/dev/null 2>&1 || true
    rendered=$(builtin complete -p -- "$command" 2>/dev/null) || rendered=
  fi
  if [[ -z $rendered ]]; then
    rendered=$(builtin complete -p -D 2>/dev/null) || rendered=
  fi
  printf '%s' "$rendered"
}

declare -ga _shell_sense_bash_compopt_options=()
_shell_sense_bash_compopt_proxy() {
  if (($# == 0)); then
    local configured
    for configured in "${_shell_sense_bash_compopt_options[@]}"; do
      printf 'compopt -o %q\n' "$configured"
    done
    return 0
  fi

  local -a arguments=("$@")
  local -a changes=()
  local operation="enable" option=""
  while (($#)); do
    case $1 in
      -o)
        operation=enable
        option=${2-}
        (($# >= 2)) || return 2
        shift 2
        ;;
      +o)
        operation=disable
        option=${2-}
        (($# >= 2)) || return 2
        shift 2
        ;;
      *)
        builtin compopt "${arguments[@]}"
        return
        ;;
    esac
    changes+=("$operation" "$option")
  done

  local -i index
  for ((index = 0; index < ${#changes[@]}; index += 2)); do
    operation=${changes[index]}
    option=${changes[index+1]}
    if [[ $operation == enable ]]; then
      [[ " ${_shell_sense_bash_compopt_options[*]} " == *" $option "* ]] || _shell_sense_bash_compopt_options+=("$option")
    else
      local -a retained=()
      local value
      for value in "${_shell_sense_bash_compopt_options[@]}"; do
        [[ $value == "$option" ]] || retained+=("$value")
      done
      _shell_sense_bash_compopt_options=("${retained[@]}")
    fi
  done
}

_shell_sense_bash_expand_filter() {
  local filter=$1 current=$2 character="" next=""
  local -i index length=${#filter}
  _shell_sense_bash_expanded_filter=""
  for ((index = 0; index < length; index++)); do
    character=${filter:index:1}
    next=${filter:index+1:1}
    if [[ $character == \\ && $next == '&' ]]; then
      _shell_sense_bash_expanded_filter+='&'
      ((index += 1))
    elif [[ $character == '&' ]]; then
      _shell_sense_bash_expanded_filter+=$current
    else
      _shell_sense_bash_expanded_filter+=$character
    fi
  done
}

_shell_sense_bash_default_candidates() {
  local current=$1
  if (( _shell_sense_bash_cword == 0 )); then
    builtin compgen -A command -- "$current"
  elif [[ $current == \$* ]]; then
    builtin compgen -A variable -- "${current#\$}" | while IFS= read -r value; do printf '$%s\n' "$value"; done
  elif [[ $current == \~* && $current != */* ]]; then
    builtin compgen -A user -- "${current#\~}" | while IFS= read -r value; do printf '~%s\n' "$value"; done
  else
    builtin compgen -f -- "$current"
  fi
}

_shell_sense_bash_generate() {
  local -i retry_count=${1:-0}
  local current=${_shell_sense_bash_words[_shell_sense_bash_cword]}
  _shell_sense_bash_dequote_word "$current"
  local generation_current=$_shell_sense_bash_dequoted_word
  local open_quote=$_shell_sense_bash_open_quote
  local previous=
  ((_shell_sense_bash_cword > 0)) && previous=${_shell_sense_bash_words[_shell_sense_bash_cword-1]}
  local rendered
  rendered=$(_shell_sense_bash_find_compspec "$_shell_sense_bash_command")

  local -a generated=() static_arguments=() options=()
  local function_name="" command_name="" filter_pattern="" prefix="" suffix=""
  if [[ -n $rendered ]]; then
    local -a spec=()
    # `complete -p` is emitted by Bash itself using reusable shell quoting.
    # Evaluating only that generated argument vector cannot execute text from
    # the editable command line.
    eval "spec=( ${rendered#complete } )"
    local -i index=0
    local argument
    while ((index < ${#spec[@]})); do
      argument=${spec[index]}
      case $argument in
        -o)
          options+=("${spec[index+1]}")
          index=$((index + 2))
          ;;
        -F)
          function_name=${spec[index+1]}
          index=$((index + 2))
          ;;
        -C)
          command_name=${spec[index+1]}
          index=$((index + 2))
          ;;
        -X)
          filter_pattern=${spec[index+1]}
          index=$((index + 2))
          ;;
        -P)
          prefix=${spec[index+1]}
          index=$((index + 2))
          ;;
        -S)
          suffix=${spec[index+1]}
          index=$((index + 2))
          ;;
        -A|-G|-W)
          static_arguments+=("$argument" "${spec[index+1]}")
          index=$((index + 2))
          ;;
        -a|-b|-c|-d|-e|-f|-g|-j|-k|-s|-u|-v)
          static_arguments+=("$argument")
          index=$((index + 1))
          ;;
        -D|-E|-I)
          index=$((index + 1))
          ;;
        *)
          index=$((index + 1))
          ;;
      esac
    done

    if ((${#static_arguments[@]})); then
      mapfile -t generated < <(builtin compgen "${static_arguments[@]}" -- "$generation_current")
    fi

    if [[ -n $function_name ]] && declare -F "$function_name" >/dev/null; then
      local saved_compopt=
      saved_compopt=$(declare -f compopt 2>/dev/null) || true
      compopt() { _shell_sense_bash_compopt_proxy "$@"; }
      _shell_sense_bash_compopt_options=("${options[@]}")
      local -a COMPREPLY=()
      local COMP_LINE=$_shell_sense_bash_comp_line
      local COMP_POINT=${#_shell_sense_bash_comp_line}
      local -a COMP_WORDS=("${_shell_sense_bash_words[@]}")
      local COMP_CWORD=$_shell_sense_bash_cword
      local COMP_TYPE=9 COMP_KEY=9
      local -i function_status=0
      "$function_name" "$_shell_sense_bash_command" "$current" "$previous" || function_status=$?
      generated+=("${COMPREPLY[@]}")
      options=("${_shell_sense_bash_compopt_options[@]}")
      unset -f compopt
      [[ -z $saved_compopt ]] || eval "$saved_compopt"
      if ((function_status == 124 && retry_count == 0)); then
        _shell_sense_bash_generate 1
        return
      fi
    fi

    if [[ -n $command_name ]]; then
      local command_output=
      printf -v command_output ' %q %q %q' "$_shell_sense_bash_command" "$current" "$previous"
      command_output=$command_name$command_output
      mapfile -t command_candidates < <(eval "$command_output")
      generated+=("${command_candidates[@]}")
    fi
  else
    mapfile -t generated < <(_shell_sense_bash_default_candidates "$generation_current")
    options+=(filenames)
  fi

  if ((${#generated[@]} == 0)); then
    if [[ " ${options[*]} " == *' dirnames '* ]]; then
      mapfile -t generated < <(builtin compgen -d -- "$generation_current")
      [[ " ${options[*]} " == *' filenames '* ]] || options+=(filenames)
    elif [[ " ${options[*]} " == *' default '* || " ${options[*]} " == *' bashdefault '* ]]; then
      mapfile -t generated < <(_shell_sense_bash_default_candidates "$generation_current")
      [[ " ${options[*]} " == *' filenames '* ]] || options+=(filenames)
    fi
  elif [[ " ${options[*]} " == *' plusdirs '* ]]; then
    mapfile -t directory_candidates < <(builtin compgen -d -- "$generation_current")
    generated+=("${directory_candidates[@]}")
    [[ " ${options[*]} " == *' filenames '* ]] || options+=(filenames)
  fi

  local candidate pattern negate=0
  if [[ -n $filter_pattern ]]; then
    _shell_sense_bash_expand_filter "$filter_pattern" "$current"
    pattern=$_shell_sense_bash_expanded_filter
    if [[ $pattern == '!'* ]]; then
      negate=1
      pattern=${pattern:1}
    fi
  fi
  local filenames=0 fullquote=0 nospace=0 noquote=0
  [[ " ${options[*]} " == *' filenames '* ]] && filenames=1
  [[ " ${options[*]} " == *' fullquote '* ]] && fullquote=1
  [[ " ${options[*]} " == *' nospace '* ]] && nospace=1
  [[ " ${options[*]} " == *' noquote '* ]] && noquote=1

  local -A seen=()
  local insertion kind append_space identity path raw_candidate resource_path
  local -i directory
  local -i ordinal=${#_shell_sense_bash_candidates[@]}
  for candidate in "${generated[@]}"; do
    [[ -n $candidate ]] || continue
    if [[ -n $filter_pattern ]]; then
      # The right-hand side is intentionally a native Bash glob pattern.
      if [[ $candidate == $pattern ]]; then
        ((negate)) || continue
      else
        ((negate)) && continue
      fi
    fi
    raw_candidate=$candidate
    kind=text
    append_space=1
    directory=0
    path=${raw_candidate/#\~/$HOME}
    if ((filenames)) && [[ -d $path ]]; then
      [[ $raw_candidate == */ ]] || raw_candidate+=/
      kind=directory
      append_space=0
      directory=1
    elif ((filenames)); then
      kind="file"
    elif [[ $candidate == -* ]]; then
      kind=option
    elif ((_shell_sense_bash_cword == 0)); then
      kind="command"
    fi
    resource_path=
    if [[ $kind == file || $kind == directory || $kind == symlink ]]; then
      resource_path=$path
      [[ $resource_path == /* ]] || resource_path=$PWD/$resource_path
    fi
    candidate=$prefix$raw_candidate$suffix
    [[ -z ${seen[$candidate]+present} ]] || continue
    seen[$candidate]=1
    insertion=$candidate
    if (((filenames || fullquote) && !noquote)); then
      _shell_sense_bash_quote_filename "$candidate" "$open_quote" "$directory"
      insertion=$_shell_sense_bash_quoted_filename
    fi
    ((nospace)) && append_space=0
    [[ $candidate == */ || $candidate == *= ]] && append_space=0
    identity="$_shell_sense_bash_replace_char_start:$_shell_sense_bash_replace_char_end:$ordinal"
    _shell_sense_bash_candidates+=("$candidate")
    _shell_sense_bash_descriptions+=("")
    _shell_sense_bash_kinds+=("$kind")
    _shell_sense_bash_resource_paths+=("$resource_path")
    _shell_sense_bash_append_spaces+=("$append_space")
    _shell_sense_bash_acceptance_identities+=("$identity")
    _shell_sense_bash_insertions+=("$insertion")
    ordinal=$((ordinal + 1))
  done
}

_shell_sense_bash_collect() {
  local line=$1
  local -i point=$2 fuzzy_min_chars=$3
  _shell_sense_bash_candidates=()
  _shell_sense_bash_insertions=()
  _shell_sense_bash_descriptions=()
  _shell_sense_bash_kinds=()
  _shell_sense_bash_resource_paths=()
  _shell_sense_bash_append_spaces=()
  _shell_sense_bash_acceptance_identities=()
  _shell_sense_bash_query_mode=exact

  _shell_sense_bash_context "$line" "$point" || return
  local -a exact_words=("${_shell_sense_bash_words[@]}")
  local exact_line=$_shell_sense_bash_comp_line
  local current=${_shell_sense_bash_words[_shell_sense_bash_cword]}
  _shell_sense_bash_generate

  if ((${#_shell_sense_bash_candidates[@]} == 0 && ${#current} >= fuzzy_min_chars)); then
    local retained
    retained=$(_shell_sense_bash_structural_prefix "$current")
    _shell_sense_bash_words=("${exact_words[@]}")
    _shell_sense_bash_words[_shell_sense_bash_cword]=$retained
    local -i prefix_length=$((${#exact_line} - ${#current}))
    _shell_sense_bash_comp_line=${exact_line:0:prefix_length}$retained
    _shell_sense_bash_query_mode=broad
    _shell_sense_bash_generate
  fi
}
