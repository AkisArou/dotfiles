#!/usr/bin/env zsh

emulate -L zsh
setopt errexit nounset pipefail no_aliases

typeset -gr project_root=${0:A:h:h}
typeset -gA seen_cases=()
typeset case_id line zsh_label fish_label bash_label zsh_kind fish_kind bash_kind resource
while IFS=$'\t' read -r case_id line zsh_label fish_label bash_label zsh_kind fish_kind bash_kind resource; do
  [[ -n $case_id && $case_id != \#* ]] || continue
  (( $#line && $#zsh_label && $#fish_label && $#bash_label && $#zsh_kind &&
     $#fish_kind && $#bash_kind && $#resource )) || {
    print -u2 -- "malformed native conformance case: $case_id"
    return 1
  }
  (( ! $+seen_cases[$case_id] )) || {
    print -u2 -- "duplicate native conformance case: $case_id"
    return 1
  }
  seen_cases[$case_id]=1
done < "$project_root/tests/conformance/cases.tsv"
(( $#seen_cases )) || {
  print -u2 -- 'native conformance suite has no cases'
  return 1
}

typeset -A seen_capabilities=()
typeset capability zsh_status fish_status bash_status reason capability_status
while IFS=$'\t' read -r capability zsh_status fish_status bash_status reason; do
  [[ -n $capability && $capability != \#* ]] || continue
  (( ! $+seen_capabilities[$capability] )) || {
    print -u2 -- "duplicate native capability: $capability"
    return 1
  }
  for capability_status in "$zsh_status" "$fish_status" "$bash_status"; do
    [[ $capability_status == required || $capability_status == unsupported ||
       $capability_status == limited ]] || {
      print -u2 -- "invalid native capability status for $capability: $capability_status"
      return 1
    }
  done
  [[ -n $reason ]] || {
    print -u2 -- "native capability has no rationale: $capability"
    return 1
  }
  seen_capabilities[$capability]=1
done < "$project_root/tests/conformance/capabilities.tsv"
(( $#seen_capabilities )) || {
  print -u2 -- 'native capability matrix is empty'
  return 1
}

zsh "$project_root/tests/zsh-capture.zsh"
fish --no-config "$project_root/tests/fish-provider.fish"
bash "$project_root/tests/bash-provider.bash"

print -r -- "native-conformance-ok cases=$#seen_cases capabilities=$#seen_capabilities"
