#!/usr/bin/env zsh

emulate -L zsh
setopt errexit nounset pipefail no_aliases

typeset -gr project_root=${0:A:h:h}
typeset -gr release_binary="$project_root/target/release/shell-sense"
typeset -g package_root
package_root=$(mktemp -d /tmp/shell-sense-release-package.XXXXXX)
typeset -gr bin_dir="$package_root/bin"
typeset -gr data_dir="$package_root/share/Shell Sense's data"
typeset -gr installed_binary="$bin_dir/shell-sense"

cleanup() {
  command rm -rf -- "$package_root"
}
trap cleanup EXIT

fail() {
  print -u2 -- "$1"
  return 1
}

[[ -x $release_binary ]] || fail "build $release_binary before running this test"

"$release_binary" install --bin-dir "$bin_dir" --data-dir "$data_dir" >/dev/null
[[ -x $installed_binary ]] || fail 'the installed executable is not executable'
command cmp -s -- "$release_binary" "$installed_binary" ||
  fail 'the installed executable differs from the release artifact'
[[ $("$installed_binary" --version) == $("$release_binary" --version) ]] ||
  fail 'the installed executable reports a different version'

typeset -a assets=(
  config.example.toml
  lua/blink-cmp-shell-sense/init.lua
  shell/bash/client.bash
  shell/bash/provider.bash
  shell/bash/shell-sense.bash
  shell/fish/client.fish
  shell/fish/provider.fish
  shell/fish/shell-sense.fish
  shell/zsh/client.zsh
  shell/zsh/provider.zsh
  shell/zsh/shell-sense.plugin.zsh
)
typeset asset
for asset in "${assets[@]}"; do
  [[ -f $data_dir/$asset ]] || fail "the installed asset tree is missing $asset"
  command cmp -s -- "$project_root/$asset" "$data_dir/$asset" ||
    fail "the embedded asset differs from the source tree: $asset"
done

# A reinstall owns and replaces only the runtime data tree. It must remove
# stale application files while leaving user configuration elsewhere intact.
command mkdir -p -- "$package_root/config/shell-sense"
print -r -- 'user-owned = true' >"$package_root/config/shell-sense/config.toml"
command touch -- "$data_dir/stale-owned-file"
"$release_binary" install --bin-dir "$bin_dir" --data-dir "$data_dir" >/dev/null
[[ ! -e $data_dir/stale-owned-file ]] || fail 'a reinstall retained a stale runtime asset'
[[ $(<"$package_root/config/shell-sense/config.toml") == 'user-owned = true' ]] ||
  fail 'a reinstall modified user configuration outside the runtime tree'

"$installed_binary" config check --path "$data_dir/config.example.toml" >/dev/null
typeset schema_output
schema_output=$("$installed_binary" config schema)
[[ $schema_output == *'"title": "Config"'* ]] ||
  fail 'the installed executable did not emit its configuration schema'

# The generated source commands must survive whitespace and a literal quote in
# the installed path and load successfully in every supported shell.
command mkdir -p -- "$package_root/empty-zdotdir"
ZDOTDIR="$package_root/empty-zdotdir" zsh -f -n "$data_dir/shell/zsh/shell-sense.plugin.zsh"
bash -n "$data_dir/shell/bash/shell-sense.bash"
fish -n "$data_dir/shell/fish/shell-sense.fish"
typeset shell init_output source_line path_test_line
for shell in zsh fish bash; do
  init_output=$("$installed_binary" init "$shell" --data-dir "$data_dir")
  source_line=${init_output##*$'\n'}
  [[ $source_line == source\ * ]] || fail "init $shell did not emit a source command"
  path_test_line="test -f ${source_line#source }"
  case $shell in
    zsh) ZDOTDIR="$package_root/empty-zdotdir" zsh -f -c "$path_test_line" ;;
    fish) fish --no-config -c "$path_test_line" ;;
    bash) bash --noprofile --norc -c "$path_test_line" ;;
  esac
done

# The installed Blink source is independently loadable from its runtime tree.
if (( $+commands[nvim] )); then
  SHELL_SENSE_INSTALLED_DATA="$data_dir" nvim --clean -u NONE --headless \
    -c "lua vim.opt.runtimepath:prepend(vim.env.SHELL_SENSE_INSTALLED_DATA); assert(type(require('blink-cmp-shell-sense').new) == 'function')" \
    -c 'qa!'
fi

print -r -- 'release-package-ok'
