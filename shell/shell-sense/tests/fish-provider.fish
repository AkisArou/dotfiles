#!/usr/bin/env fish

set -l project_root (path resolve (path dirname (path dirname (status filename))))
source "$project_root/shell/fish/provider.fish"
source "$project_root/shell/fish/client.fish"

function fail --argument-names message
    printf 'fish provider test failed: %s\n' "$message" >&2
    exit 1
end

__shell_sense_fish_byte_length ''
test $_shell_sense_fish_byte_count -eq 0; or fail 'empty byte length'
__shell_sense_fish_byte_length 'λ'
test $_shell_sense_fish_byte_count -eq 2; or fail 'UTF-8 byte length'

__shell_sense_fish_encode_message test '' 'λ'
test "$_shell_sense_fish_encoded" = '4:test,1:2,0:,2:λ,'; or fail 'netstring encoding'

complete -c shell-sense-fixture -a alpha -d 'first candidate'
complete -c shell-sense-fixture -a restart -d 'restart services'
__shell_sense_fish_collect 'shell-sense-fixture rstart' 3
contains -- restart $_shell_sense_fish_labels; or fail 'native fuzzy candidate'
set -l restart_index (contains -i -- restart $_shell_sense_fish_labels)
test "$_shell_sense_fish_descriptions[$restart_index]" = 'restart services'; or fail 'native description'

complete -c shell-sense-flags -l recursive -d 'list subdirectories recursively'
__shell_sense_fish_collect 'shell-sense-flags --recusr' 3
contains -- --recursive $_shell_sense_fish_labels; or fail 'broadened native option'
test "$_shell_sense_fish_query_mode" = broad; or fail 'broad query mode'

complete -c shell-sense-conformance -a restart -d 'restart services'
complete -c shell-sense-conformance -l recursive -d 'list subdirectories recursively'
complete -c shell-sense-short -s a
complete -c shell-sense-short -s b
complete -c shell-sense-combined -a '\-ab'
complete -c shell-sense-user -a custom-native
complete -c shell-sense-value -l color -a 'auto always never' -d 'color value'
complete -c shell-sense-path -a '(__fish_complete_directories)'

set -l fixture_root (mktemp -d /tmp/shell-sense-fish-provider.XXXXXX); or fail 'temporary directory'
function cleanup --on-event fish_exit
    command rm -rf -- "$fixture_root"
end
command mkdir -- "$fixture_root/dotfiles" "$fixture_root/dotfiles/nvim" "$fixture_root/space directory"
command ln -s dotfiles/nvim "$fixture_root/linked-dir"
pushd "$fixture_root" >/dev/null

while read -l record
    set -l fields (string split \t -- "$record")
    set -l case_id $fields[1]
    string match -qr '^#|^$' -- "$case_id"; and continue
    test (count $fields) -eq 9; or fail 'malformed shared conformance row'
    set -l line $fields[2]
    set -l expected_label $fields[4]
    set -l expected_kind $fields[7]
    set -l resource $fields[9]
    __shell_sense_fish_collect "$line" 3
    contains -- "$expected_label" $_shell_sense_fish_labels; or fail "conformance candidate: $case_id"
    set -l candidate_index (contains -i -- "$expected_label" $_shell_sense_fish_labels)
    test "$_shell_sense_fish_kinds[$candidate_index]" = "$expected_kind"; or fail "conformance kind: $case_id"
    set -l actual_resource (string replace -r '/$' '' -- "$_shell_sense_fish_resource_paths[$candidate_index]")
    if test "$resource" = '-'
        test -z "$actual_resource"; or fail "unexpected conformance resource: $case_id"
    else
        test "$actual_resource" = "$fixture_root/$resource"; or fail "conformance resource: $case_id"
    end
end < "$project_root/tests/conformance/cases.tsv"

__shell_sense_fish_collect 'cd dotfil' 3
contains -- dotfiles/ $_shell_sense_fish_labels; or fail 'native directory candidate'
set -l dotfiles_index (contains -i -- dotfiles/ $_shell_sense_fish_labels)
test "$_shell_sense_fish_resource_paths[$dotfiles_index]" = "$fixture_root/dotfiles/"; or fail 'typed directory resource'
__shell_sense_fish_collect 'cd dotfiles/nv' 3
contains -- dotfiles/nvim/ $_shell_sense_fish_labels; or fail 'nested native directory candidate'
set -l nvim_index (contains -i -- dotfiles/nvim/ $_shell_sense_fish_labels)
test "$_shell_sense_fish_resource_paths[$nvim_index]" = "$fixture_root/dotfiles/nvim/"; or fail 'nested typed directory resource'
popd >/dev/null

printf 'fish native provider tests passed\n'
