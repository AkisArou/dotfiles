#!/usr/bin/env bash
set -euo pipefail

project_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
source "$project_root/shell/bash/provider.bash"
source "$project_root/shell/bash/client.bash"

fail() {
  printf 'Bash provider test failed: %s\n' "$1" >&2
  exit 1
}

assert_context() {
  local line=$1 expected_line=$2 expected_cword=$3 expected_words=$4
  _shell_sense_bash_context "$line" "${#line}"
  [[ $_shell_sense_bash_comp_line == "$expected_line" ]] || fail "COMP_LINE for $line"
  [[ $_shell_sense_bash_cword == "$expected_cword" ]] || fail "COMP_CWORD for $line"
  local joined=
  printf -v joined '<%s>' "${_shell_sense_bash_words[@]}"
  [[ $joined == "$expected_words" ]] || fail "COMP_WORDS for $line: $joined"
}

# These expected values are captured from real Readline completion calls in
# tests/fixtures/bash-context-dump.bash.
assert_context 'shell-sense-test alpha be' 'shell-sense-test alpha be' 2 '<shell-sense-test><alpha><be>'
assert_context 'shell-sense-test "foo b' 'shell-sense-test "foo b' 1 '<shell-sense-test><"foo b>'
assert_context 'shell-sense-test --color=au' 'shell-sense-test --color=au' 3 '<shell-sense-test><--color><=><au>'
assert_context 'echo x | shell-sense-test r' 'shell-sense-test r' 1 '<shell-sense-test><r>'
assert_context 'FOO=bar shell-sense-test r' 'shell-sense-test r' 1 '<shell-sense-test><r>'
assert_context 'FOO="a b" shell-sense-test r' 'shell-sense-test r' 1 '<shell-sense-test><r>'

PROMPT_COMMAND='original-prompt-hook'
_shell_sense_bash_install_prompt_hook
[[ $(declare -p PROMPT_COMMAND) == 'declare -a '* ]] || fail 'scalar PROMPT_COMMAND normalization'
[[ ${PROMPT_COMMAND[0]} == _shell_sense_bash_before_prompt &&
   ${PROMPT_COMMAND[1]} == original-prompt-hook ]] || fail 'scalar PROMPT_COMMAND preservation'
PROMPT_COMMAND=(first-prompt-hook second-prompt-hook)
_shell_sense_bash_install_prompt_hook
[[ ${PROMPT_COMMAND[0]} == _shell_sense_bash_before_prompt &&
   ${PROMPT_COMMAND[1]} == first-prompt-hook &&
   ${PROMPT_COMMAND[2]} == second-prompt-hook ]] || fail 'array PROMPT_COMMAND preservation'

complete -W 'restart reset-failed rescue reload' shell-sense-test
_shell_sense_bash_collect 'shell-sense-test rstart' 23 3
[[ $_shell_sense_bash_query_mode == broad ]] || fail 'broad query mode'
[[ " ${_shell_sense_bash_candidates[*]} " == *' restart '* ]] || fail 'fuzzy native candidate'

_shell_sense_conformance_completion() {
  if [[ $2 == --* ]]; then
    COMPREPLY=(--recursive)
  else
    COMPREPLY=(restart)
  fi
}
complete -F _shell_sense_conformance_completion shell-sense-conformance

_shell_sense_value_completion() {
  COMPREPLY=(auto always never)
}
complete -F _shell_sense_value_completion shell-sense-value
complete -o filenames -d shell-sense-path

_shell_sense_test_broad_context() {
  if [[ $COMP_LINE == 'shell-sense-context ' && $COMP_POINT == 20 &&
        $COMP_CWORD == 1 && ${COMP_WORDS[1]} == '' ]]; then
    COMPREPLY=(restart)
  fi
}
complete -F _shell_sense_test_broad_context shell-sense-context
_shell_sense_bash_collect 'shell-sense-context rstart' 26 3
[[ ${_shell_sense_bash_candidates[0]} == restart ]] || fail 'broad programmable-completion context'

_shell_sense_test_function() {
  COMPREPLY=(alpha beta)
  compopt -o nospace
}
complete -F _shell_sense_test_function shell-sense-function
_shell_sense_bash_collect 'shell-sense-function al' 23 3
[[ ${_shell_sense_bash_candidates[0]} == alpha ]] || fail 'function candidate'
[[ ${_shell_sense_bash_append_spaces[0]} == 0 ]] || fail 'function compopt metadata'

_shell_sense_test_command() {
  printf '%s\n' command-alpha command-beta
}
complete -C _shell_sense_test_command shell-sense-command
_shell_sense_bash_collect 'shell-sense-command command-a' 29 3
[[ ${_shell_sense_bash_candidates[0]} == command-alpha ]] || fail 'command candidate'

_shell_sense_test_lazy() {
  _shell_sense_test_loaded() {
    COMPREPLY=(loaded-candidate)
  }
  complete -F _shell_sense_test_loaded shell-sense-lazy
  return 124
}
complete -F _shell_sense_test_lazy shell-sense-lazy
_shell_sense_bash_collect 'shell-sense-lazy loa' 20 3
[[ ${_shell_sense_bash_candidates[0]} == loaded-candidate ]] || fail 'lazy compspec retry'

complete -s shell-sense-service
_shell_sense_bash_collect 'shell-sense-service s' 21 3
((${#_shell_sense_bash_candidates[@]} > 0)) || fail 'service action'

complete -W 'alpha beta gamma' -X 'b*' shell-sense-filter
line='shell-sense-filter '
_shell_sense_bash_collect "$line" "${#line}" 3
[[ " ${_shell_sense_bash_candidates[*]} " == *' alpha '* ]] || fail 'filter retained candidate'
[[ " ${_shell_sense_bash_candidates[*]} " != *' beta '* ]] || fail 'filter removed candidate'

complete -W 'alpha literal&' -X '*\&*' shell-sense-literal-filter
line='shell-sense-literal-filter '
_shell_sense_bash_collect "$line" "${#line}" 3
[[ " ${_shell_sense_bash_candidates[*]} " == *' alpha '* ]] || fail 'escaped filter retained other candidate'
[[ " ${_shell_sense_bash_candidates[*]} " != *' literal& '* ]] || fail 'escaped filter ampersand'

complete -W 'alpha' -P pre- -S=-post shell-sense-affix
line='shell-sense-affix '
_shell_sense_bash_collect "$line" "${#line}" 3
[[ ${_shell_sense_bash_candidates[0]} == pre-alpha=-post ]] || fail 'prefix and suffix'

fixture_root=$(mktemp -d /tmp/shell-sense-bash-provider.XXXXXX)
cleanup() {
  rm -rf -- "$fixture_root"
}
trap cleanup EXIT
mkdir -- "$fixture_root/dotfiles" "$fixture_root/dotfiles/nvim" "$fixture_root/space directory"
ln -s dotfiles/nvim "$fixture_root/linked-dir"
touch -- "$fixture_root/space file"
pushd "$fixture_root" >/dev/null

while IFS=$'\t' read -r case_id line zsh_label fish_label expected_label zsh_kind fish_kind expected_kind resource; do
  [[ -n $case_id && $case_id != \#* ]] || continue
  _shell_sense_bash_collect "$line" "${#line}" 3
  found=0
  for index in "${!_shell_sense_bash_candidates[@]}"; do
    if [[ ${_shell_sense_bash_candidates[index]} == "$expected_label" ]]; then
      found=1
      [[ ${_shell_sense_bash_kinds[index]} == "$expected_kind" ]] || fail "conformance kind: $case_id"
      if [[ $resource == - ]]; then
        [[ -z ${_shell_sense_bash_resource_paths[index]} ]] || fail "unexpected conformance resource: $case_id"
      else
        [[ ${_shell_sense_bash_resource_paths[index]%/} == "$fixture_root/$resource" ]] || fail "conformance resource: $case_id"
      fi
    fi
  done
  ((found)) || fail "conformance candidate: $case_id"
done < "$project_root/tests/conformance/cases.tsv"

line='cd dotfil'
_shell_sense_bash_collect "$line" "${#line}" 3
[[ " ${_shell_sense_bash_candidates[*]} " == *' dotfiles/ '* ]] || fail 'directory candidate'
for index in "${!_shell_sense_bash_candidates[@]}"; do
  if [[ ${_shell_sense_bash_candidates[index]} == dotfiles/ ]]; then
    [[ ${_shell_sense_bash_resource_paths[index]} == "$fixture_root/dotfiles" ]] || fail 'typed directory resource'
  fi
done
line='cd dotfiles/nv'
_shell_sense_bash_collect "$line" "${#line}" 3
[[ " ${_shell_sense_bash_candidates[*]} " == *' dotfiles/nvim/ '* ]] || fail 'nested directory candidate'
for index in "${!_shell_sense_bash_candidates[@]}"; do
  if [[ ${_shell_sense_bash_candidates[index]} == dotfiles/nvim/ ]]; then
    [[ ${_shell_sense_bash_resource_paths[index]} == "$fixture_root/dotfiles/nvim" ]] || fail 'nested typed directory resource'
  fi
done

complete -o dirnames -W '' shell-sense-dirnames
line='shell-sense-dirnames dot'
_shell_sense_bash_collect "$line" "${#line}" 3
[[ " ${_shell_sense_bash_candidates[*]} " == *' dotfiles/ '* ]] || fail 'dirnames fallback'

complete -o plusdirs -W 'special' shell-sense-plusdirs
line='shell-sense-plusdirs '
_shell_sense_bash_collect "$line" "${#line}" 3
[[ " ${_shell_sense_bash_candidates[*]} " == *' special '* ]] || fail 'plusdirs static candidate'
[[ " ${_shell_sense_bash_candidates[*]} " == *' dotfiles/ '* ]] || fail 'plusdirs directory candidate'

complete -o default -W 'unrelated' shell-sense-default
line='shell-sense-default spa'
_shell_sense_bash_collect "$line" "${#line}" 3
[[ " ${_shell_sense_bash_candidates[*]} " == *' space file '* ]] || fail 'default fallback'

complete -G 'space*' shell-sense-glob
line='shell-sense-glob '
_shell_sense_bash_collect "$line" "${#line}" 3
[[ " ${_shell_sense_bash_candidates[*]} " == *' space file '* ]] || fail 'glob action'

line='cd "spa'
_shell_sense_bash_collect "$line" "${#line}" 3
[[ " ${_shell_sense_bash_candidates[*]} " == *' space file '* ]] || fail 'quoted filename candidate'
for index in "${!_shell_sense_bash_candidates[@]}"; do
  if [[ ${_shell_sense_bash_candidates[index]} == 'space file' ]]; then
    [[ ${_shell_sense_bash_insertions[index]} == '"space file"' ]] || fail 'quoted file insertion'
  elif [[ ${_shell_sense_bash_candidates[index]} == 'space directory/' ]]; then
    [[ ${_shell_sense_bash_insertions[index]} == '"space directory/' ]] || fail 'open quoted directory insertion'
  fi
done

complete -o filenames -o noquote -f shell-sense-noquote
line='shell-sense-noquote spa'
_shell_sense_bash_collect "$line" "${#line}" 3
for index in "${!_shell_sense_bash_candidates[@]}"; do
  if [[ ${_shell_sense_bash_candidates[index]} == 'space file' ]]; then
    [[ ${_shell_sense_bash_insertions[index]} == 'space file' ]] || fail 'noquote insertion'
  fi
done

complete -o filenames -d -P path= shell-sense-affixed-directory
line='shell-sense-affixed-directory '
_shell_sense_bash_collect "$line" "${#line}" 3
found_affixed_directory=0
for index in "${!_shell_sense_bash_candidates[@]}"; do
  if [[ ${_shell_sense_bash_candidates[index]} == 'path=dotfiles/' ]]; then
    found_affixed_directory=1
    [[ ${_shell_sense_bash_kinds[index]} == directory ]] || fail 'affixed directory kind'
    [[ ${_shell_sense_bash_append_spaces[index]} == 0 ]] || fail 'affixed directory spacing'
  fi
done
((found_affixed_directory)) || fail 'affixed directory candidate'

complete -o fullquote -W '"two words"' shell-sense-fullquote
line='shell-sense-fullquote '
_shell_sense_bash_collect "$line" "${#line}" 3
[[ ${_shell_sense_bash_candidates[0]} == 'two words' ]] || fail 'fullquote candidate'
[[ ${_shell_sense_bash_insertions[0]} == 'two\ words' ]] || fail 'fullquote insertion'

complete -I -W 'initial-native'
line='initial-n'
_shell_sense_bash_collect "$line" "${#line}" 3
[[ ${_shell_sense_bash_candidates[0]} == initial-native ]] || fail 'initial-word compspec'
complete -r -I

complete -E -W 'empty-native'
line=''
_shell_sense_bash_collect "$line" "${#line}" 3
[[ ${_shell_sense_bash_candidates[0]} == empty-native ]] || fail 'empty-line compspec'
complete -r -E

complete -D -W 'default-native'
line='unregistered-command '
_shell_sense_bash_collect "$line" "${#line}" 3
[[ ${_shell_sense_bash_candidates[0]} == default-native ]] || fail 'default compspec'
complete -r -D
popd >/dev/null

printf 'Bash native provider tests passed\n'
