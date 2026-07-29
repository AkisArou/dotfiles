#!/usr/bin/env zsh

emulate -L zsh
setopt errexit nounset pipefail

zmodload zsh/zpty
typeset -gx SHELL_SENSE_TEST_ROOT=${0:A:h:h}
typeset output
typeset conformance_root
conformance_root=$(mktemp -d /tmp/shell-sense-zsh-conformance.XXXXXX)
command mkdir -- "$conformance_root/dotfiles" "$conformance_root/dotfiles/nvim" \
  "$conformance_root/space directory"
command ln -s dotfiles/nvim "$conformance_root/linked-dir"

cleanup() {
  zpty -d sense-worker 2>/dev/null || true
  command unlink -- "$conformance_root/linked-dir" 2>/dev/null || true
  command rmdir -- "$conformance_root/dotfiles/nvim" \
    "$conformance_root/dotfiles" "$conformance_root/space directory" \
    "$conformance_root" 2>/dev/null || true
}
trap cleanup EXIT

zpty sense-worker zsh -f
zpty -r sense-worker output '*%*' || {
  print -u2 -- 'initial Zsh prompt did not appear'
  return 1
}
zpty -w sense-worker ". ${(q)SHELL_SENSE_TEST_ROOT}/tests/fixtures/zsh-capture-init.zsh"
zpty -r -m sense-worker output '*<SENSE-PROMPT>*' || {
  print -u2 -- 'test initialization prompt did not appear'
  return 1
}

zpty -n -w sense-worker $'sense-test --a\t'
zpty -r -m sense-worker output '*<BUFFER>*</BUFFER>*<SENSE-PROMPT>*' || {
  print -u2 -- 'Zsh capture widget did not finish'
  return 1
}

local expected
for expected in \
  '<COUNT>2</COUNT>' \
  '<WORD1>--all</WORD1>' \
  '<WORD2>--amend</WORD2>' \
  '<DESC2>replace the previous commit</DESC2>' \
  '<GROUP2>options</GROUP2>' \
  '<EXPL2>command options</EXPL2>' \
  '<BUFFER>sense-test --amend </BUFFER>'; do
  if [[ $output != *$expected* ]]; then
    print -u2 -- "missing expected capture output: $expected"
    print -u2 -r -- "$output"
    return 1
  fi
done

zpty -n -w sense-worker $'cd cra\t'
zpty -r -m sense-worker output '*<PATH-BUFFER>*</PATH-BUFFER>*<SENSE-PROMPT>*' || {
  print -u2 -- 'Zsh path-resource capture did not finish'
  return 1
}
for expected in \
  "<PATH-RESOURCE>$SHELL_SENSE_TEST_ROOT/crates</PATH-RESOURCE>" \
  '<PATH-BUFFER>cd crates/</PATH-BUFFER>'; do
  [[ $output == *$expected* ]] || {
    print -u2 -- "missing expected path metadata: $expected"
    print -u2 -r -- "$output"
    return 1
  }
done

zpty -n -w sense-worker $'sense-verb rstart\t'
zpty -r -m sense-worker output '*<FUZZY-BUFFER>*</FUZZY-BUFFER>*<SENSE-PROMPT>*' || {
  print -u2 -- 'Zsh fuzzy acceptance did not finish'
  return 1
}
for expected in '<FUZZY-COUNT>1</FUZZY-COUNT>' '<FUZZY-BUFFER>sense-verb restart </FUZZY-BUFFER>'; do
  [[ $output == *$expected* ]] || {
    print -u2 -- "missing expected fuzzy output: $expected"
    print -u2 -r -- "$output"
    return 1
  }
done

zpty -w sense-worker "cd -- ${(q)conformance_root}"
zpty -r -m sense-worker output '*<SENSE-PROMPT>*' || {
  print -u2 -- 'Zsh conformance fixture directory was not entered'
  return 1
}

local case_id line expected_label fish_label bash_label expected_kind fish_kind bash_kind resource
while IFS=$'\t' read -r case_id line expected_label fish_label bash_label expected_kind fish_kind bash_kind resource; do
  [[ -n $case_id && $case_id != \#* ]] || continue
  zpty -n -w sense-worker "${line}"$'\t'
  zpty -r -m sense-worker output '*<SENSE-PROMPT>*' || {
    print -u2 -- "Zsh conformance case did not finish: $case_id"
    return 1
  }
  local expected_record="<CONFORMANCE>${expected_label}|${expected_kind}|"
  [[ $output == *$expected_record* ]] || {
    print -u2 -- "Zsh conformance case failed: $case_id"
    print -u2 -r -- "$output"
    return 1
  }
  if [[ $resource != - ]]; then
    [[ $output == *"|${conformance_root}/${resource}</CONFORMANCE>"* ]] || {
      print -u2 -- "Zsh conformance resource failed: $case_id"
      print -u2 -r -- "$output"
      return 1
    }
  else
    [[ $output == *"<CONFORMANCE>${expected_label}|${expected_kind}|</CONFORMANCE>"* ]] || {
      print -u2 -- "Zsh conformance emitted an unexpected resource: $case_id"
      print -u2 -r -- "$output"
      return 1
    }
  fi
done < "$SHELL_SENSE_TEST_ROOT/tests/conformance/cases.tsv"

# Native capture must retain a large provider result without imposing a UI
# limit or manufacturing a second source of candidates.
zpty -n -w sense-worker $'shell-sense-large \t'
zpty -r -m sense-worker output '*<LARGE-LAST>*</LARGE-LAST>*<SENSE-PROMPT>*' || {
  print -u2 -- 'Zsh large-provider capture did not finish'
  return 1
}
for expected in '<LARGE-COUNT>4096</LARGE-COUNT>' '<LARGE-LAST>4096</LARGE-LAST>'; do
  [[ $output == *$expected* ]] || {
    print -u2 -- "Zsh large-provider capture is missing: $expected"
    return 1
  }
done

zpty -n -w sense-worker $'ls -l\t'
zpty -r -m sense-worker output '*<LS-DESC>*</LS-DESC>*<SENSE-PROMPT>*' || {
  print -u2 -- 'Zsh standard option-description capture did not finish'
  return 1
}
for expected in \
  '<LS-WORD>-la</LS-WORD>' \
  '<LS-DISPLAY>-a</LS-DISPLAY>' \
  '<LS-DESC>list entries starting with .</LS-DESC>' \
  '<LS-BUFFER>ls -la</LS-BUFFER>'; do
  [[ $output == *$expected* ]] || {
    print -u2 -- "missing expected standard option metadata: $expected"
    print -u2 -r -- "$output"
    return 1
  }
done

print -r -- 'zsh-capture-ok'
