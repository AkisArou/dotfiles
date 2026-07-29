#!/usr/bin/env zsh

emulate -L zsh
setopt errexit nounset pipefail no_aliases

typeset -gr project_root=${0:A:h:h}
typeset -gr release_binary="$project_root/target/release/shell-sense"

[[ -x $release_binary ]] || {
  print -u2 -- "build $release_binary before running this test"
  return 1
}

SHELL_SENSE_TEST_BINARY="$release_binary" \
SHELL_SENSE_CONFIG__LOGGING__LEVEL=trace \
SHELL_SENSE_LATENCY_SAMPLES=${SHELL_SENSE_LATENCY_SAMPLES:-25} \
SHELL_SENSE_PIPELINE_BUDGET_MS=${SHELL_SENSE_PIPELINE_BUDGET_MS:-30.0} \
SHELL_SENSE_TERMINAL_OBSERVATION_BUDGET_MS=${SHELL_SENSE_TERMINAL_OBSERVATION_BUDGET_MS:-75.0} \
SHELL_SENSE_ASSERT_TIMING_TRACES=1 \
SHELL_SENSE_REPORT_TIMINGS=1 \
  zsh "$project_root/tests/live-client.zsh"
