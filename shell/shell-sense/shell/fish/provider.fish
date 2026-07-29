# Native Fish completion adapter for Shell Sense.
#
# This file intentionally contains candidate generation only. The active Fish
# process calls `complete -C`; no helper Fish process or fallback source is
# allowed to contribute candidates.

function __shell_sense_fish_structural_prefix --argument-names token
    set -l path_prefix (string replace -r '^(.*/)[^/]*$' '$1' -- "$token")
    if test "$path_prefix" != "$token"
        printf %s "$path_prefix"
        return
    end

    set -l value_prefix (string replace -r '^(.*=)[^=]*$' '$1' -- "$token")
    if test "$value_prefix" != "$token"
        printf %s "$value_prefix"
        return
    end

    if string match -q -- '--*' "$token"
        printf '%s' '--'
    else if string match -q -- '-*' "$token"
        printf '%s' '-'
    else if string match -q -- '+*' "$token"
        printf '%s' '+'
    else if string match -q -- '\$*' "$token"
        printf '%s' '$'
    else if string match -q -- '~*' "$token"
        printf '%s' '~'
    end
end

function __shell_sense_fish_kind --argument-names insertion description
    if string match -q -- '*/' "$insertion"; or string match -qi '*directory*' "$description"; or string match -qi '*folder*' "$description"
        set -g _shell_sense_fish_candidate_kind directory
    else if string match -qi '*symlink*' "$description"
        set -g _shell_sense_fish_candidate_kind symlink
    else if string match -qi '*file*' "$description"
        set -g _shell_sense_fish_candidate_kind file
    else if string match -q -- '-*' "$insertion"; or string match -qi '*option*' "$description"
        set -g _shell_sense_fish_candidate_kind option
    else if string match -qi '*builtin*' "$description"
        set -g _shell_sense_fish_candidate_kind builtin
    else if string match -qi '*function*' "$description"
        set -g _shell_sense_fish_candidate_kind function
    else if string match -qi '*command*' "$description"
        set -g _shell_sense_fish_candidate_kind command
    else if string match -qi '*variable*' "$description"
        set -g _shell_sense_fish_candidate_kind variable
    else if string match -qi '*user*' "$description"
        set -g _shell_sense_fish_candidate_kind user
    else if string match -qi '*host*' "$description"
        set -g _shell_sense_fish_candidate_kind host
    else if string match -qi '*process*' "$description"
        set -g _shell_sense_fish_candidate_kind process
    else if string match -qi '*job*' "$description"
        set -g _shell_sense_fish_candidate_kind job
    else if string match -qi '*service*' "$description"
        set -g _shell_sense_fish_candidate_kind service
    else
        set -g _shell_sense_fish_candidate_kind text
    end
end

function __shell_sense_fish_append_space --argument-names insertion
    if string match -q -- '*/' "$insertion"; or string match -q -- '*=' "$insertion"
        set -g _shell_sense_fish_candidate_append_space 0
    else
        set -g _shell_sense_fish_candidate_append_space 1
    end
end

# Populate parallel global arrays from Fish's native completion engine.
# `line_prefix` is the command buffer cut at the cursor. Fish's documented
# `commandline --input` path supplies native tokenization for broad queries.
function __shell_sense_fish_collect --argument-names line_prefix fuzzy_min_chars
    set -g _shell_sense_fish_insertions
    set -g _shell_sense_fish_labels
    set -g _shell_sense_fish_descriptions
    set -g _shell_sense_fish_kinds
    set -g _shell_sense_fish_resource_paths
    set -g _shell_sense_fish_query_mode exact

    set -l token (commandline --input="$line_prefix" -ct)
    set -l output (complete -C "$line_prefix" --escape)
    if not set -q output[1]
        if test (string length -- "$token") -ge "$fuzzy_min_chars"
            set -l retained (__shell_sense_fish_structural_prefix "$token")
            set -l token_pattern (string escape --style=regex -- "$token")
            set -l query_prefix (string replace -r -- "$token_pattern"'$' '' "$line_prefix")
            set output (complete -C "$query_prefix$retained" --escape)
            set -g _shell_sense_fish_query_mode broad
        end
    end

    for record in $output
        set -l fields (string split -m 1 \t -- "$record")
        set -l insertion $fields[1]
        set -l description
        if set -q fields[2]
            set description $fields[2]
        end
        set -l label (string unescape -- "$insertion")
        set -a _shell_sense_fish_insertions "$insertion"
        set -a _shell_sense_fish_labels "$label"
        set -a _shell_sense_fish_descriptions "$description"
        __shell_sense_fish_kind "$insertion" "$description"
        set -a _shell_sense_fish_kinds "$_shell_sense_fish_candidate_kind"
        set -l resource_path
        if contains -- "$_shell_sense_fish_candidate_kind" file directory symlink
            set resource_path "$label"
            set -l structural_prefix (__shell_sense_fish_structural_prefix "$token")
            if string match -q -- '*/' "$structural_prefix"; and not string match -q -- "$structural_prefix*" "$resource_path"
                set resource_path "$structural_prefix$resource_path"
            end
            if test "$resource_path" = '~'
                set resource_path "$HOME"
            else if string match -q -- '~/*' "$resource_path"
                set resource_path "$HOME/"(string replace -r '^~/' '' -- "$resource_path")
            else if not string match -q -- '/*' "$resource_path"
                set resource_path "$PWD/$resource_path"
            end
        end
        set -a _shell_sense_fish_resource_paths "$resource_path"
    end
end
