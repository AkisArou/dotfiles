#!/usr/bin/env zsh

emulate -L zsh
setopt errexit nounset pipefail

zmodload zsh/zpty
typeset -gx ZSH_SENSE_TEST_ROOT=${0:A:h:h}
typeset output

zpty sense-worker zsh -f
zpty -r sense-worker output '*%*' || {
  print -u2 -- 'initial Zsh prompt did not appear'
  return 1
}
zpty -w sense-worker ". ${(q)ZSH_SENSE_TEST_ROOT}/tests/fixtures/portable-capture-init.zsh"
zpty -r -m sense-worker output '*<SENSE-PROMPT>*' || {
  print -u2 -- 'test initialization prompt did not appear'
  return 1
}

zpty -n -w sense-worker $'sense-test --a\t'
zpty -r -m sense-worker output '*<BUFFER>*</BUFFER>*<SENSE-PROMPT>*' || {
  print -u2 -- 'portable capture widget did not finish'
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

zpty -n -w sense-worker $'sense-verb rstart\t'
zpty -r -m sense-worker output '*<FUZZY-BUFFER>*</FUZZY-BUFFER>*<SENSE-PROMPT>*' || {
  print -u2 -- 'portable fuzzy acceptance did not finish'
  return 1
}
for expected in '<FUZZY-COUNT>1</FUZZY-COUNT>' '<FUZZY-BUFFER>sense-verb restart </FUZZY-BUFFER>'; do
  [[ $output == *$expected* ]] || {
    print -u2 -- "missing expected fuzzy output: $expected"
    print -u2 -r -- "$output"
    return 1
  }
done

zpty -n -w sense-worker $'ls -l\t'
zpty -r -m sense-worker output '*<LS-DESC>*</LS-DESC>*<SENSE-PROMPT>*' || {
  print -u2 -- 'portable standard option-description capture did not finish'
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

zpty -d sense-worker
print -r -- 'portable-capture-ok'
