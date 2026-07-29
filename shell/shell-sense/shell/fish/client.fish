# Fish line-editor client for Shell Sense.

set -g _shell_sense_fish_ready 0
set -g _shell_sense_fish_configured 0
set -g _shell_sense_fish_worker_pid 0
set -g _shell_sense_fish_request 0
set -g _shell_sense_fish_generation 0
set -g _shell_sense_fish_active_request 0
set -g _shell_sense_fish_active_generation 0
set -g _shell_sense_fish_active_buffer ''
set -g _shell_sense_fish_active_cursor 0
set -g _shell_sense_fish_after_accept 1
set -g _shell_sense_fish_activation_mode continuous
set -g _shell_sense_fish_activation_characters
set -g _shell_sense_fish_immediate_characters
set -g _shell_sense_fish_activation_events
set -g _shell_sense_fish_binding_states
set -g _shell_sense_fish_binding_keys
set -g _shell_sense_fish_binding_actions
set -g _shell_sense_fish_popup_lines 0
set -g _shell_sense_fish_popup_visible 0
set -g _shell_sense_fish_external_presentation 0
set -g _shell_sense_fish_selected 1
set -g _shell_sense_fish_total 0
set -g _shell_sense_fish_window_start 0
set -g _shell_sense_fish_fuzzy_min_chars 3
set -g _shell_sense_fish_max_rows 10
set -g _shell_sense_fish_max_width 140
set -g _shell_sense_fish_min_width 24
set -g _shell_sense_fish_popup_enabled 1
set -g _shell_sense_fish_padding 1
set -g _shell_sense_fish_show_descriptions 1
set -g _shell_sense_fish_show_scrollbar 1
set -g _shell_sense_fish_scrollbar_character '▐'
set -g _shell_sense_fish_indicator_mode icon
set -g _shell_sense_fish_selected_marker ''
set -g _shell_sense_fish_style_reset (printf '\e[0m')
set -g _shell_sense_fish_style_menu (printf '\e[38;2;187;187;187;48;2;32;32;32m')
set -g _shell_sense_fish_style_label (printf '\e[38;2;187;187;187m')
set -g _shell_sense_fish_style_detail (printf '\e[38;2;128;128;128m')
set -g _shell_sense_fish_style_kind (printf '\e[38;2;128;128;128m')
set -g _shell_sense_fish_style_selected (printf '\e[48;2;52;59;65m')
set -g _shell_sense_fish_style_match (printf '\e[38;2;24;162;254;1m')
set -g _shell_sense_fish_style_scrollbar_thumb (printf '\e[38;2;187;187;187m')
set -g _shell_sense_fish_style_scrollbar_gutter (printf '\e[38;2;52;59;65m')
set -g _shell_sense_fish_style_documentation (printf '\e[38;2;212;212;212;48;2;32;32;32m')
set -g _shell_sense_fish_style_documentation_border (printf '\e[38;2;212;212;212;48;2;32;32;32m')
set -g _shell_sense_fish_style_documentation_heading (printf '\e[38;2;24;162;254;48;2;32;32;32;1m')
set -g _shell_sense_fish_style_documentation_code (printf '\e[38;2;206;145;120;48;2;32;32;32m')
set -g _shell_sense_fish_style_documentation_quote (printf '\e[38;2;128;128;128;48;2;32;32;32m')
set -g _shell_sense_fish_kind_style_names
set -g _shell_sense_fish_kind_style_values
set -g _shell_sense_fish_view_ids
set -g _shell_sense_fish_view_labels
set -g _shell_sense_fish_view_label_cells
set -g _shell_sense_fish_view_kinds
set -g _shell_sense_fish_view_details
set -g _shell_sense_fish_view_detail_cells
set -g _shell_sense_fish_view_matches
set -g _shell_sense_fish_view_max_label_cells 0
set -g _shell_sense_fish_view_max_described_cells 0
set -g _shell_sense_fish_view_revision 0
set -g _shell_sense_fish_view_building 0
set -g _shell_sense_fish_view_ready 0
set -g _shell_sense_fish_menu_width 0
set -g _shell_sense_fish_border none
set -g _shell_sense_fish_documentation_item ''
set -g _shell_sense_fish_documentation_placement ''
set -g _shell_sense_fish_documentation_width 0
set -g _shell_sense_fish_documentation_expected 0
set -g _shell_sense_fish_documentation_truncated 0
set -g _shell_sense_fish_documentation_kinds
set -g _shell_sense_fish_documentation_cells
set -g _shell_sense_fish_documentation_lines

function __shell_sense_fish_reset_documentation
    set -g _shell_sense_fish_documentation_item ''
    set -g _shell_sense_fish_documentation_placement ''
    set -g _shell_sense_fish_documentation_width 0
    set -g _shell_sense_fish_documentation_expected 0
    set -g _shell_sense_fish_documentation_truncated 0
    set -g _shell_sense_fish_documentation_kinds
    set -g _shell_sense_fish_documentation_cells
    set -g _shell_sense_fish_documentation_lines
end

function __shell_sense_fish_netstring --argument-names value
    __shell_sense_fish_byte_length "$value"
    set -g _shell_sense_fish_netstring_value "$_shell_sense_fish_byte_count:$value,"
end

# Fish exposes character and terminal-cell lengths, while the shared protocol
# deliberately uses UTF-8 byte offsets. URL escaping maps every non-ASCII byte
# to one three-character `%HH` sequence, so removing two cells per sequence
# gives the original byte length.
function __shell_sense_fish_byte_length --argument-names value
    set -l escaped (string escape --style=url -- "$value")
    set -l percent_count (count (string match -a -r '%' -- "$escaped"))
    set -g _shell_sense_fish_byte_count (math (string length -- "$escaped") - 2 \* $percent_count)
end

function __shell_sense_fish_encode_message
    set -l command $argv[1]
    set -e argv[1]
    __shell_sense_fish_netstring "$command"
    set -l wire "$_shell_sense_fish_netstring_value"
    __shell_sense_fish_netstring (count $argv)
    set wire "$wire$_shell_sense_fish_netstring_value"
    for field in $argv
        __shell_sense_fish_netstring "$field"
        set wire "$wire$_shell_sense_fish_netstring_value"
    end
    set -g _shell_sense_fish_encoded "$wire"
end

function __shell_sense_fish_append
    printf '%s' "$argv[1]" >"$_shell_sense_fish_input_fifo"
end

function __shell_sense_fish_send
    test $_shell_sense_fish_worker_pid -gt 0; or return 1
    command kill -0 $_shell_sense_fish_worker_pid 2>/dev/null; or return 1
    __shell_sense_fish_encode_message $argv
    __shell_sense_fish_append "$_shell_sense_fish_encoded"
end

function __shell_sense_fish_decode_field --argument-names encoded
    string unescape -- "$encoded" | string collect
end

function __shell_sense_fish_ansi_style --argument-names specification
    set -l codes
    for component in (string split ',' -- "$specification")
        if string match -qr '^fg=#[0-9a-fA-F]{6}$' -- "$component"
            set -l hex (string sub -s 5 -- "$component")
            set -a codes 38 2 (math "0x"(string sub -s 1 -l 2 "$hex")) (math "0x"(string sub -s 3 -l 2 "$hex")) (math "0x"(string sub -s 5 -l 2 "$hex"))
            continue
        else if string match -qr '^bg=#[0-9a-fA-F]{6}$' -- "$component"
            set -l hex (string sub -s 5 -- "$component")
            set -a codes 48 2 (math "0x"(string sub -s 1 -l 2 "$hex")) (math "0x"(string sub -s 3 -l 2 "$hex")) (math "0x"(string sub -s 5 -l 2 "$hex"))
            continue
        end
        switch $component
            case bold
                set -a codes 1
            case dim faint
                set -a codes 2
            case italic
                set -a codes 3
            case underline
                set -a codes 4
        end
    end
    if set -q codes[1]
        printf '\e[%sm' (string join ';' $codes)
    end
end

function __shell_sense_fish_apply_style --argument-names name specification
    set -l ansi (__shell_sense_fish_ansi_style "$specification")
    switch $name
        case menu
            set -g _shell_sense_fish_style_menu "$ansi"
        case label
            set -g _shell_sense_fish_style_label "$ansi"
        case detail
            set -g _shell_sense_fish_style_detail "$ansi"
        case kind
            set -g _shell_sense_fish_style_kind "$ansi"
        case selected
            set -g _shell_sense_fish_style_selected "$ansi"
        case label-match
            set -g _shell_sense_fish_style_match "$ansi"
        case scrollbar-thumb
            set -g _shell_sense_fish_style_scrollbar_thumb "$ansi"
        case scrollbar-gutter
            set -g _shell_sense_fish_style_scrollbar_gutter "$ansi"
        case documentation
            set -g _shell_sense_fish_style_documentation "$ansi"
        case documentation-border
            set -g _shell_sense_fish_style_documentation_border "$ansi"
        case documentation-heading
            set -g _shell_sense_fish_style_documentation_heading "$ansi"
        case documentation-code
            set -g _shell_sense_fish_style_documentation_code "$ansi"
        case documentation-quote
            set -g _shell_sense_fish_style_documentation_quote "$ansi"
    end
end

function __shell_sense_fish_apply_kind_style --argument-names kind specification
    set -l ansi (__shell_sense_fish_ansi_style "$specification")
    set -l index (contains -i -- "$kind" $_shell_sense_fish_kind_style_names)
    if test -n "$index"
        set _shell_sense_fish_kind_style_values[$index] "$ansi"
    else
        set -a _shell_sense_fish_kind_style_names "$kind"
        set -a _shell_sense_fish_kind_style_values "$ansi"
    end
end

function __shell_sense_fish_kind_style --argument-names kind
    set -l index (contains -i -- "$kind" $_shell_sense_fish_kind_style_names)
    if test -n "$index"
        set -g _shell_sense_fish_resolved_kind_style "$_shell_sense_fish_kind_style_values[$index]"
    else
        set -g _shell_sense_fish_resolved_kind_style "$_shell_sense_fish_style_kind"
    end
end

function __shell_sense_fish_clear_popup
    if test $_shell_sense_fish_popup_lines -le 0
        set -g _shell_sense_fish_popup_visible 0
        return
    end
    printf '\e[?2026h\e7'
    for row in (seq $_shell_sense_fish_popup_lines)
        printf '\e[B\r\e[2K'
    end
    printf '\e8\e[?2026l'
    set -g _shell_sense_fish_popup_lines 0
    set -g _shell_sense_fish_popup_visible 0
end

function __shell_sense_fish_kind_icon --argument-names kind
    set -g _shell_sense_fish_icon ''
    if test "$_shell_sense_fish_indicator_mode" = text
        set -g _shell_sense_fish_icon '['(string sub -s 1 -l 1 -- "$kind")']'
        return
    else if test "$_shell_sense_fish_indicator_mode" = none
        return
    end
    set -l icon
    switch $kind
        case directory
            set icon '󰉋'
        case file symlink
            set icon '󰈔'
        case option
            set icon '󰘳'
        case command builtin function alias subcommand
            set icon '󰆍'
        case variable
            set icon '󰫧'
        case user
            set icon '󰀄'
        case host
            set icon '󰒋'
        case process job service
            set icon '󰐊'
        case '*'
            set icon '󰦨'
    end
    if test "$_shell_sense_fish_indicator_mode" = both
        set -g _shell_sense_fish_icon "$icon ["(string sub -s 1 -l 1 -- "$kind")']'
    else
        set -g _shell_sense_fish_icon "$icon"
    end
end

function __shell_sense_fish_render_label --argument-names label ranges selected_style
    if test -z "$ranges"
        printf '%s%s%s%s' "$selected_style" "$_shell_sense_fish_style_label" "$label" "$_shell_sense_fish_style_reset"
        return
    end
    set -l cursor 0
    for range in (string split ',' -- "$ranges")
        set -l bounds (string split ':' -- "$range")
        test (count $bounds) -eq 2; or continue
        set -l start $bounds[1]
        set -l finish $bounds[2]
        if test $start -gt $cursor
            printf '%s%s%s' "$selected_style" "$_shell_sense_fish_style_label" (string sub -s (math $cursor + 1) -l (math $start - $cursor) -- "$label")
        end
        if test $finish -gt $start
            printf '%s%s%s' "$selected_style" "$_shell_sense_fish_style_match" (string sub -s (math $start + 1) -l (math $finish - $start) -- "$label")
        end
        set cursor $finish
    end
    if test $cursor -lt (string length -- "$label")
        printf '%s%s%s' "$selected_style" "$_shell_sense_fish_style_label" (string sub -s (math $cursor + 1) -- "$label")
    end
    printf '%s' "$_shell_sense_fish_style_reset"
end

function __shell_sense_fish_documentation_row_count
    set -g _shell_sense_fish_documentation_row_count (count $_shell_sense_fish_documentation_lines)
    if test $_shell_sense_fish_documentation_row_count -gt 0; and test "$_shell_sense_fish_border" != none
        set -g _shell_sense_fish_documentation_row_count (math $_shell_sense_fish_documentation_row_count + 2)
    end
end

function __shell_sense_fish_render_documentation_row --argument-names row
    set -l width $_shell_sense_fish_documentation_width
    set -l border_cells 0
    set -l top_left '╭' top_right '╮' bottom_left '╰' bottom_right '╯' horizontal '─' vertical '│'
    switch $_shell_sense_fish_border
        case sharp
            set top_left '┌'; set top_right '┐'; set bottom_left '└'; set bottom_right '┘'
        case ascii
            set top_left '+'; set top_right '+'; set bottom_left '+'; set bottom_right '+'; set horizontal '-'; set vertical '|'
        case none
            set top_left ''; set top_right ''; set bottom_left ''; set bottom_right ''; set horizontal ''; set vertical ''
    end
    if test "$_shell_sense_fish_border" != none
        set border_cells 2
        if test $row -eq 1
            printf '%s%s%s%s%s%s' "$_shell_sense_fish_style_documentation" "$_shell_sense_fish_style_documentation_border" "$top_left" (string repeat -n (math $width - 2) -- "$horizontal") "$top_right" "$_shell_sense_fish_style_reset"
            return
        else if test $row -eq $_shell_sense_fish_documentation_row_count
            printf '%s%s%s%s%s%s' "$_shell_sense_fish_style_documentation" "$_shell_sense_fish_style_documentation_border" "$bottom_left" (string repeat -n (math $width - 2) -- "$horizontal") "$bottom_right" "$_shell_sense_fish_style_reset"
            return
        end
        set row (math $row - 1)
    end
    set -l text $_shell_sense_fish_documentation_lines[$row]
    set -l cells $_shell_sense_fish_documentation_cells[$row]
    set -l content_width (math $width - $border_cells - 2 \* $_shell_sense_fish_padding)
    set -l fill (math $content_width - $cells)
    test $fill -ge 0; or set fill 0
    set -l text_style $_shell_sense_fish_style_documentation
    switch $_shell_sense_fish_documentation_kinds[$row]
        case heading
            set text_style $_shell_sense_fish_style_documentation_heading
        case code
            set text_style $_shell_sense_fish_style_documentation_code
        case quote separator
            set text_style $_shell_sense_fish_style_documentation_quote
    end
    printf '%s%s%*s%s%s%s%*s%*s%s%s' \
        "$_shell_sense_fish_style_documentation" "$vertical" $_shell_sense_fish_padding '' \
        "$text_style" "$text" "$_shell_sense_fish_style_documentation" $fill '' \
        $_shell_sense_fish_padding '' "$vertical" "$_shell_sense_fish_style_reset"
end

function __shell_sense_fish_render_popup
    set -l previous_popup_lines $_shell_sense_fish_popup_lines
    set -l item_count (count $_shell_sense_fish_view_labels)
    if test $_shell_sense_fish_popup_enabled -ne 1; or test $_shell_sense_fish_external_presentation -eq 1; or test $item_count -eq 0
        __shell_sense_fish_clear_popup
        return
    end
    set -l row_count $item_count
    test $row_count -gt $_shell_sense_fish_max_rows; and set row_count $_shell_sense_fish_max_rows
    set -l first 1
    if test $_shell_sense_fish_selected -gt $row_count
        set first (math $_shell_sense_fish_selected - $row_count + 1)
    end
    if test (math $first + $row_count - 1) -gt $item_count
        set first (math $item_count - $row_count + 1)
    end
    set -l last (math $first + $row_count - 1)

    set -l columns $COLUMNS
    if test -z "$columns"; or test $columns -lt 20
        set columns 80
    end
    set -l content_width $_shell_sense_fish_view_max_label_cells
    test $_shell_sense_fish_show_descriptions -eq 1; and set content_width $_shell_sense_fish_view_max_described_cells
    set -l marker_cells 0
    test -z "$_shell_sense_fish_selected_marker"; or set marker_cells (math (string length -- "$_shell_sense_fish_selected_marker") + 1)
    set -l indicator_cells 0
    switch $_shell_sense_fish_indicator_mode
        case icon
            set indicator_cells 2
        case text
            set indicator_cells 4
        case both
            set indicator_cells 6
    end
    set -l width (math $content_width + $marker_cells + $indicator_cells + 2 \* $_shell_sense_fish_padding)
    test $_shell_sense_fish_show_scrollbar -eq 1; and test $_shell_sense_fish_total -gt $row_count; and set width (math $width + 1)
    if test $_shell_sense_fish_menu_width -gt 0
        set width $_shell_sense_fish_menu_width
    else
        test $width -lt $_shell_sense_fish_min_width; and set width $_shell_sense_fish_min_width
        test $width -gt $_shell_sense_fish_max_width; and set width $_shell_sense_fish_max_width
    end
    test $width -gt $columns; and set width $columns

    __shell_sense_fish_documentation_row_count
    set -l documentation_rows $_shell_sense_fish_documentation_row_count

    set -l thumb 0
    if test $_shell_sense_fish_show_scrollbar -eq 1; and test $_shell_sense_fish_total -gt $row_count
        if test $_shell_sense_fish_total -le 1
            set thumb 1
        else
            set thumb (math "round(($_shell_sense_fish_window_start + $_shell_sense_fish_selected - 1) * ($row_count - 1) / ($_shell_sense_fish_total - 1)) + 1")
        end
    end

    printf '\e[?2026h\e7'
    for popup_line in (seq $previous_popup_lines)
        printf '\e[B\r\e[2K'
    end
    printf '\e8'
    set -l row 0
    set -l scrollbar_cells 0
    test $thumb -eq 0; or set scrollbar_cells 1
    for index in (seq $first $last)
        set row (math $row + 1)
        set -l selected_style
        set -l marker
        if test $index -eq $_shell_sense_fish_selected
            set selected_style $_shell_sense_fish_style_selected
            set marker $_shell_sense_fish_selected_marker
        end
        __shell_sense_fish_kind_icon $_shell_sense_fish_view_kinds[$index]
        set -l icon "$_shell_sense_fish_icon"
        __shell_sense_fish_kind_style $_shell_sense_fish_view_kinds[$index]
        set -l kind_style "$_shell_sense_fish_resolved_kind_style"
        set -l detail
        if test $_shell_sense_fish_show_descriptions -eq 1
            set detail $_shell_sense_fish_view_details[$index]
        end
        set -l spaces (math $width - 2 \* $_shell_sense_fish_padding - $marker_cells - $indicator_cells - $scrollbar_cells - $_shell_sense_fish_view_label_cells[$index])
        test -z "$detail"; or set spaces (math $spaces - $_shell_sense_fish_view_detail_cells[$index])
        test $spaces -ge 0; or set spaces 0

        set -l row_style "$_shell_sense_fish_style_menu$selected_style"
        printf '\e[B\r%s%*s' "$row_style" $_shell_sense_fish_padding ''
        test $marker_cells -eq 0; or printf '%-*s' $marker_cells "$marker"
        test $indicator_cells -eq 0; or printf '%s%-*s%s' "$kind_style" $indicator_cells "$icon" "$row_style"
        __shell_sense_fish_render_label $_shell_sense_fish_view_labels[$index] $_shell_sense_fish_view_matches[$index] "$row_style"
        printf '%s%*s' "$row_style" $spaces ''
        if test -n "$detail"
            printf '%s%s%s%s' "$row_style" "$_shell_sense_fish_style_detail" "$detail" "$_shell_sense_fish_style_reset"
        end
        printf '%s%*s' "$row_style" $_shell_sense_fish_padding ''
        if test $thumb -gt 0
            if test $row -eq $thumb
                printf '%s%s' "$_shell_sense_fish_style_scrollbar_thumb" "$_shell_sense_fish_scrollbar_character"
            else
                printf '%s ' "$_shell_sense_fish_style_scrollbar_gutter"
            end
        end
        printf '%s' "$_shell_sense_fish_style_reset"
        if test "$_shell_sense_fish_documentation_placement" = side; and test $row -le $documentation_rows
            printf ' '
            __shell_sense_fish_render_documentation_row $row
        end
        printf '\e[K'
    end
    if test "$_shell_sense_fish_documentation_placement" = side
        for documentation_row in (seq (math $row_count + 1) $documentation_rows)
            printf '\e[B\r%s%*s%s ' "$_shell_sense_fish_style_menu" $width '' "$_shell_sense_fish_style_reset"
            __shell_sense_fish_render_documentation_row $documentation_row
            printf '\e[K'
        end
    else if test "$_shell_sense_fish_documentation_placement" = below
        for documentation_row in (seq $documentation_rows)
            printf '\e[B\r'
            __shell_sense_fish_render_documentation_row $documentation_row
            printf '\e[K'
        end
    end
    printf '\e8\e[?2026l'
    if test "$_shell_sense_fish_documentation_placement" = side
        set -g _shell_sense_fish_popup_lines (math max $row_count, $documentation_rows)
    else
        set -g _shell_sense_fish_popup_lines (math $row_count + $documentation_rows)
    end
    set -g _shell_sense_fish_popup_visible 1
end

function __shell_sense_fish_capture --argument-names request generation request_buffer request_cursor
    set -l current_buffer (commandline -b)
    set -l current_prefix (commandline -bc)
    __shell_sense_fish_byte_length "$current_prefix"
    set -l current_cursor_bytes $_shell_sense_fish_byte_count
    if test "$request" != "$_shell_sense_fish_active_request"; or test "$generation" != "$_shell_sense_fish_active_generation"; or test "$request_buffer" != "$current_buffer"; or test "$request_cursor" != "$current_cursor_bytes"
        __shell_sense_fish_encode_message shell-capture-begin "$request" "$generation"
        set -l batch "$_shell_sense_fish_encoded"
        __shell_sense_fish_encode_message capture-end "$request" "$generation"
        __shell_sense_fish_append "$batch$_shell_sense_fish_encoded"
        return
    end

    set -l token_prefix (commandline -ct)
    set -l token (commandline -t)
    set -l cursor_bytes $current_cursor_bytes
    __shell_sense_fish_byte_length "$token_prefix"
    set -l token_prefix_bytes $_shell_sense_fish_byte_count
    __shell_sense_fish_byte_length "$token"
    set -l token_bytes $_shell_sense_fish_byte_count
    set -l replace_start (math $cursor_bytes - $token_prefix_bytes)
    set -l replace_end (math $replace_start + $token_bytes)
    __shell_sense_fish_collect "$current_prefix" $_shell_sense_fish_fuzzy_min_chars

    set -l context_words (commandline --input="$current_prefix" -opc)
    set -a context_words (commandline --input="$current_prefix" -ct)
    set -l context_total (count $context_words)
    set -l context_current (math $context_total - 1)

    __shell_sense_fish_encode_message shell-capture-begin "$request" "$generation"
    set -l batch "$_shell_sense_fish_encoded"
    __shell_sense_fish_encode_message context-begin "$request" "$generation" "$context_current" "$context_total"
    set batch "$batch$_shell_sense_fish_encoded"
    set -l context_first 1
    while test $context_first -le $context_total
        set -l context_last (math $context_first + 59)
        test $context_last -le $context_total; or set context_last $context_total
        set -l context_count (math $context_last - $context_first + 1)
        __shell_sense_fish_encode_message context-chunk "$request" "$generation" (math $context_first - 1) "$context_count" $context_words[$context_first..$context_last]
        set batch "$batch$_shell_sense_fish_encoded"
        set context_first (math $context_last + 1)
    end
    __shell_sense_fish_encode_message context-end "$request" "$generation"
    set batch "$batch$_shell_sense_fish_encoded"
    set -l batch_count 0
    for index in (seq (count $_shell_sense_fish_insertions))
        __shell_sense_fish_append_space "$_shell_sense_fish_insertions[$index]"
        __shell_sense_fish_encode_message shell-candidate \
            "$request" "$generation" \
            "$_shell_sense_fish_insertions[$index]" "$_shell_sense_fish_labels[$index]" \
            "$_shell_sense_fish_descriptions[$index]" '' \
            "$replace_start" "$replace_end" "$_shell_sense_fish_kinds[$index]" \
            (math $index - 1) "$_shell_sense_fish_candidate_append_space" 0 "$index" \
            "$_shell_sense_fish_resource_paths[$index]"
        set batch "$batch$_shell_sense_fish_encoded"
        set batch_count (math $batch_count + 1)
        if test $batch_count -ge 64
            __shell_sense_fish_append "$batch"
            set batch ''
            set batch_count 0
        end
    end
    __shell_sense_fish_encode_message capture-end "$request" "$generation"
    __shell_sense_fish_append "$batch$_shell_sense_fish_encoded"
end

function __shell_sense_fish_dispatch --argument-names command
    set -e argv[1]
    switch $command
        case ready
            set -g _shell_sense_fish_ready 1
        case config
            test (count $argv) -ge 20; or return
            set -g _shell_sense_fish_activation_mode $argv[1]
            set -g _shell_sense_fish_popup_enabled $argv[4]
            set -g _shell_sense_fish_max_rows $argv[5]
            set -g _shell_sense_fish_after_accept $argv[3]
            set -g _shell_sense_fish_max_width $argv[6]
            set -g _shell_sense_fish_min_width $argv[7]
            set -g _shell_sense_fish_padding $argv[8]
            set -g _shell_sense_fish_border $argv[10]
            set -g _shell_sense_fish_show_scrollbar $argv[13]
            set -g _shell_sense_fish_show_descriptions $argv[15]
            set -g _shell_sense_fish_indicator_mode $argv[17]
            set -g _shell_sense_fish_selected_marker $argv[18]
            set -g _shell_sense_fish_fuzzy_min_chars $argv[20]
            set -l cursor 21
            set -l character_count $argv[$cursor]
            set cursor (math $cursor + 1)
            set -g _shell_sense_fish_activation_characters
            for character_index in (seq $character_count)
                set -a _shell_sense_fish_activation_characters $argv[$cursor]
                set cursor (math $cursor + 1)
            end
            set -l immediate_count $argv[$cursor]
            set cursor (math $cursor + 1)
            set -g _shell_sense_fish_immediate_characters
            for immediate_index in (seq $immediate_count)
                set -a _shell_sense_fish_immediate_characters $argv[$cursor]
                set cursor (math $cursor + 1)
            end
            set -l event_count $argv[$cursor]
            set cursor (math $cursor + 1)
            set -g _shell_sense_fish_activation_events
            for event_index in (seq $event_count)
                set -a _shell_sense_fish_activation_events $argv[$cursor]
                set cursor (math $cursor + 1)
            end
        case popup-option
            if test "$argv[1]" = scrollbar-character
                set -g _shell_sense_fish_scrollbar_character $argv[2]
            end
        case style
            __shell_sense_fish_apply_style $argv[1] $argv[2]
        case kind-style
            __shell_sense_fish_apply_kind_style $argv[1] $argv[2]
        case keybinding
            set -a _shell_sense_fish_binding_states $argv[1]
            set -a _shell_sense_fish_binding_keys $argv[2]
            set -a _shell_sense_fish_binding_actions $argv[3]
        case config-end
            set -g _shell_sense_fish_configured 1
        case presentation
            test (count $argv) -eq 1; or return
            set -g _shell_sense_fish_external_presentation $argv[1]
            if test $_shell_sense_fish_external_presentation -eq 1
                __shell_sense_fish_clear_popup
                commandline -f repaint
            else
                __shell_sense_fish_render_popup
            end
        case capture-request
            __shell_sense_fish_capture $argv[1] $argv[2] $argv[3] $argv[4]
        case view-begin
            test "$argv[2]" = "$_shell_sense_fish_active_request"; and test "$argv[3]" = "$_shell_sense_fish_active_generation"; or return
            set -g _shell_sense_fish_view_building 1
            set -g _shell_sense_fish_view_revision $argv[4]
            set -g _shell_sense_fish_menu_width 0
            __shell_sense_fish_reset_documentation
            set -g _shell_sense_fish_selected (math 0$argv[5] + 1)
            set -g _shell_sense_fish_total $argv[10]
            set -g _shell_sense_fish_window_start $argv[11]
            set -g _shell_sense_fish_view_max_label_cells $argv[13]
            set -g _shell_sense_fish_view_max_described_cells $argv[14]
            set -g _shell_sense_fish_view_ids
            set -g _shell_sense_fish_view_labels
            set -g _shell_sense_fish_view_label_cells
            set -g _shell_sense_fish_view_kinds
            set -g _shell_sense_fish_view_details
            set -g _shell_sense_fish_view_detail_cells
            set -g _shell_sense_fish_view_matches
        case view-chunk
            set -l item_count $argv[3]
            set -l offset 4
            for index in (seq $item_count)
                set -a _shell_sense_fish_view_ids $argv[$offset]
                set -a _shell_sense_fish_view_labels $argv[(math $offset + 1)]
                set -a _shell_sense_fish_view_label_cells $argv[(math $offset + 2)]
                set -a _shell_sense_fish_view_kinds $argv[(math $offset + 3)]
                set -a _shell_sense_fish_view_details $argv[(math $offset + 4)]
                set -a _shell_sense_fish_view_detail_cells $argv[(math $offset + 5)]
                set -a _shell_sense_fish_view_matches $argv[(math $offset + 9)]
                set offset (math $offset + 11)
            end
        case view-layout
            test (count $argv) -eq 4; or return
            test "$argv[1]" = "$_shell_sense_fish_active_request"; and test "$argv[2]" = "$_shell_sense_fish_active_generation"; and test "$argv[3]" = "$_shell_sense_fish_view_revision"; or return
            set -g _shell_sense_fish_menu_width $argv[4]
        case documentation-begin
            test (count $argv) -eq 7; or return
            test "$argv[1]" = "$_shell_sense_fish_active_request"; and test "$argv[2]" = "$_shell_sense_fish_active_generation"; or return
            contains -- "$argv[4]" side below; or return
            __shell_sense_fish_reset_documentation
            set -g _shell_sense_fish_documentation_item $argv[3]
            set -g _shell_sense_fish_documentation_placement $argv[4]
            set -g _shell_sense_fish_documentation_width $argv[5]
            set -g _shell_sense_fish_documentation_expected $argv[6]
            set -g _shell_sense_fish_documentation_truncated $argv[7]
        case documentation-chunk
            test (count $argv) -ge 4; or return
            test "$argv[1]" = "$_shell_sense_fish_active_request"; and test "$argv[2]" = "$_shell_sense_fish_active_generation"; and test "$argv[3]" = "$_shell_sense_fish_documentation_item"; or return
            set -l line_count $argv[4]
            test (count $argv) -eq (math 4 + 3 \* $line_count); or return
            set -l offset 5
            for documentation_line in (seq $line_count)
                set -a _shell_sense_fish_documentation_kinds $argv[$offset]
                set -a _shell_sense_fish_documentation_cells $argv[(math $offset + 1)]
                set -a _shell_sense_fish_documentation_lines $argv[(math $offset + 2)]
                set offset (math $offset + 3)
            end
        case documentation-end
            test (count $argv) -eq 3; or return
            test "$argv[1]" = "$_shell_sense_fish_active_request"; and test "$argv[2]" = "$_shell_sense_fish_active_generation"; and test "$argv[3]" = "$_shell_sense_fish_documentation_item"; or return
            test (count $_shell_sense_fish_documentation_lines) -eq $_shell_sense_fish_documentation_expected; or return
            set -l selected_id $_shell_sense_fish_view_ids[$_shell_sense_fish_selected]
            if test "$selected_id" != "$_shell_sense_fish_documentation_item"
                __shell_sense_fish_reset_documentation
                return
            end
            if test $_shell_sense_fish_view_building -eq 0
                __shell_sense_fish_render_popup
            end
        case documentation-clear
            test (count $argv) -eq 2; or return
            test "$argv[1]" = "$_shell_sense_fish_active_request"; and test "$argv[2]" = "$_shell_sense_fish_active_generation"; or return
            __shell_sense_fish_reset_documentation
            if test $_shell_sense_fish_view_building -eq 0
                __shell_sense_fish_render_popup
            end
        case view-end
            set -g _shell_sense_fish_view_building 0
            __shell_sense_fish_render_popup
            set -g _shell_sense_fish_view_ready 1
        case accept-fish
            set -l request $argv[1]
            set -l generation $argv[2]
            set -l item_id $argv[3]
            if test "$request" != "$_shell_sense_fish_active_request"; or test "$generation" != "$_shell_sense_fish_active_generation"
                __shell_sense_fish_send selection-finished "$request" "$generation" "$item_id" 0
                return
            end
            set -l current_buffer (commandline -b)
            set -l current_prefix (commandline -bc)
            __shell_sense_fish_byte_length "$current_prefix"
            if test "$current_buffer" != "$_shell_sense_fish_active_buffer"; or test $_shell_sense_fish_byte_count != $_shell_sense_fish_active_cursor
                __shell_sense_fish_send selection-finished "$request" "$generation" "$item_id" 0
                return
            end
            __shell_sense_fish_clear_popup
            set -l insertion $argv[4]
            test "$argv[7]" = 1; and set insertion "$insertion "
            commandline -rt -- "$insertion"
            commandline -f repaint
            __shell_sense_fish_send selection-finished "$request" "$generation" "$item_id" 1
            if test $_shell_sense_fish_after_accept -eq 1
                __shell_sense_fish_request after-accept
            end
        case request-cancelled
            if test "$argv[1]" = "$_shell_sense_fish_active_request"; and test "$argv[2]" = "$_shell_sense_fish_active_generation"
                __shell_sense_fish_clear_popup
            end
        case error
            set -g _shell_sense_fish_last_error "$argv[1]: $argv[2]"
    end
end

function __shell_sense_fish_drain
    test -f "$_shell_sense_fish_output_mailbox"; or return
    set -l records
    while read -l record
        set -a records "$record"
    end <"$_shell_sense_fish_output_mailbox"
    set -l count (count $records)
    test $count -gt 0; or return
    kill -USR2 $_shell_sense_fish_worker_pid 2>/dev/null
    set -l tab (printf '\t')
    for record in $records
        set -l encoded (string split "$tab" -- "$record")
        test (count $encoded) -ge 2; or continue
        set -l field_count $encoded[2]
        test (count $encoded) -eq (math $field_count + 2); or continue
        set -l decoded
        for field in $encoded[3..]
            if test -z "$field"
                set -a decoded ''
            else
                set -a decoded (__shell_sense_fish_decode_field "$field")
            end
        end
        __shell_sense_fish_dispatch $encoded[1] $decoded
    end
end

function __shell_sense_fish_signal --on-signal USR1
    __shell_sense_fish_drain
end

function __shell_sense_fish_request --argument-names trigger
    test $_shell_sense_fish_ready -eq 1; or return
    test "$_shell_sense_fish_activation_mode" != disabled; or return
    set -g _shell_sense_fish_request (math $_shell_sense_fish_request + 1)
    set -g _shell_sense_fish_generation (math $_shell_sense_fish_generation + 1)
    set -g _shell_sense_fish_active_request $_shell_sense_fish_request
    set -g _shell_sense_fish_active_generation $_shell_sense_fish_generation
    set -l buffer (commandline -b)
    set -l prefix (commandline -bc)
    __shell_sense_fish_byte_length "$prefix"
    set -l cursor $_shell_sense_fish_byte_count
    set -g _shell_sense_fish_active_buffer "$buffer"
    set -g _shell_sense_fish_active_cursor $cursor
    set -g _shell_sense_fish_view_ready 0
    set -l columns $COLUMNS
    test -n "$columns"; or set columns 80
    set -l rows $LINES
    test -n "$rows"; or set rows 24
    __shell_sense_fish_send complete \
        "$_shell_sense_fish_active_request" "$_shell_sense_fish_active_generation" '' \
        "$buffer" "$cursor" "$PWD" "$fish_bind_mode" "$columns" "$rows" \
        "$trigger" 0
end

function __shell_sense_fish_cancel
    if test $_shell_sense_fish_active_request -gt 0
        __shell_sense_fish_send cancel $_shell_sense_fish_active_request $_shell_sense_fish_active_generation
    end
    __shell_sense_fish_clear_popup
end

function __shell_sense_fish_insert
    __shell_sense_fish_clear_popup
    set -l character "$fish_key"
    commandline -i -- "$character"
    set -e fish_key
    __shell_sense_fish_after_edit insert "$character"
end

function __shell_sense_fish_backspace
    __shell_sense_fish_clear_popup
end

function __shell_sense_fish_after_edit --argument-names event character
    switch $_shell_sense_fish_activation_mode
        case continuous
        case hybrid
            if not contains -- "$event" $_shell_sense_fish_activation_events; and not contains -- "$character" $_shell_sense_fish_activation_characters
                return
            end
        case '*'
            return
    end
    if test -n "$character"; and contains -- "$character" $_shell_sense_fish_immediate_characters
        __shell_sense_fish_request trigger-character
    else
        __shell_sense_fish_request automatic
    end
end

function __shell_sense_fish_accept
    if test $_shell_sense_fish_popup_visible -ne 1
        commandline -f end-of-line
        return
    end
    set -l current_buffer (commandline -b)
    set -l current_prefix (commandline -bc)
    __shell_sense_fish_byte_length "$current_prefix"
    if test "$current_buffer" != "$_shell_sense_fish_active_buffer"; or test $_shell_sense_fish_byte_count != $_shell_sense_fish_active_cursor
        __shell_sense_fish_cancel
        return
    end
    set -l id $_shell_sense_fish_view_ids[$_shell_sense_fish_selected]
    __shell_sense_fish_send select $_shell_sense_fish_active_request $_shell_sense_fish_active_generation "$id"
end

function __shell_sense_fish_binding_action --argument-names state key
    set -g _shell_sense_fish_resolved_action ''
    for index in (seq (count $_shell_sense_fish_binding_keys))
        if test "$_shell_sense_fish_binding_states[$index]" = "$state"; and test "$_shell_sense_fish_binding_keys[$index]" = "$key"
            set -g _shell_sense_fish_resolved_action $_shell_sense_fish_binding_actions[$index]
            return
        end
    end
end

function __shell_sense_fish_fallback --argument-names key
    switch $key
        case tab
            commandline -f complete
        case ctrl-c
            commandline -f cancel-commandline
        case enter
            commandline -f execute
        case ctrl-e end
            commandline -f end-of-line
        case ctrl-n
            commandline -f down-or-search
        case ctrl-p
            commandline -f up-or-search
        case ctrl-d
            commandline -f delete-or-exit
        case ctrl-u
            commandline -f backward-kill-line
        case right
            commandline -f forward-char
        case escape
            commandline -f cancel
    end
end

function __shell_sense_fish_key --argument-names key
    set -l state closed
    test $_shell_sense_fish_popup_visible -eq 1; and set state popup
    __shell_sense_fish_binding_action $state $key
    switch $_shell_sense_fish_resolved_action
        case trigger
            __shell_sense_fish_request manual
        case accept
            __shell_sense_fish_accept
        case execute
            __shell_sense_fish_execute
            commandline -f execute
        case interrupt
            __shell_sense_fish_interrupt
            commandline -f cancel-commandline
        case next
            __shell_sense_fish_navigate next down-or-search
        case previous
            __shell_sense_fish_navigate previous up-or-search
        case page-down
            __shell_sense_fish_navigate page-down delete-or-exit
        case page-up
            __shell_sense_fish_navigate page-up backward-kill-line
        case dismiss
            __shell_sense_fish_cancel
        case '*'
            __shell_sense_fish_fallback $key
    end
end

function __shell_sense_fish_navigate --argument-names action fallback
    if test $_shell_sense_fish_popup_visible -ne 1
        commandline -f $fallback
        return
    end
    __shell_sense_fish_send navigate $_shell_sense_fish_active_request $_shell_sense_fish_active_generation "$action"
end

function __shell_sense_fish_interrupt
    __shell_sense_fish_cancel
end

function __shell_sense_fish_execute
    __shell_sense_fish_clear_popup
end

function __shell_sense_fish_bindings
    bind '' get-key __shell_sense_fish_insert
    for sequence in space \; \| \& \> \< \)
        bind $sequence __shell_sense_fish_clear_popup self-insert expand-abbr '__shell_sense_fish_after_edit insert'
    end
    bind backspace __shell_sense_fish_backspace backward-delete-char '__shell_sense_fish_after_edit backspace'
    set -l configured_keys
    for key in $_shell_sense_fish_binding_keys
        contains -- "$key" $configured_keys; or set -a configured_keys "$key"
    end
    for key in $configured_keys
        bind $key "__shell_sense_fish_key $key"
    end
end

function __shell_sense_fish_start
    status is-interactive; or return
    if test $_shell_sense_fish_worker_pid -gt 0
        if command kill -0 $_shell_sense_fish_worker_pid 2>/dev/null
            test $_shell_sense_fish_ready -eq 1; and test $_shell_sense_fish_configured -eq 1; and return
        end
        __shell_sense_fish_reset_transport
    end
    set -l root (path dirname (path dirname "$_shell_sense_fish_plugin_dir"))
    set -l command_line
    if set -q SHELL_SENSE_COMMAND
        set command_line (string split ' ' -- "$SHELL_SENSE_COMMAND")
    else if type -q shell-sense
        set command_line (command -s shell-sense)
    else if test -x "$root/target/release/shell-sense"
        set command_line "$root/target/release/shell-sense"
    else if test -x "$root/target/debug/shell-sense"
        set command_line "$root/target/debug/shell-sense"
    else
        return 1
    end

    set -l runtime_base "$XDG_RUNTIME_DIR"
    test -n "$runtime_base"; or set runtime_base "/tmp/shell-sense-"(id -u)
    set -l runtime_dir "$runtime_base/shell-sense"
    command mkdir -p -m 700 -- "$runtime_dir"; or return 1
    command chmod 700 -- "$runtime_dir"; or return 1
    set -l token "$fish_pid-"(random)"-"(random)
    set -g _shell_sense_fish_input_fifo "$runtime_dir/fish-$token.in"
    set -g _shell_sense_fish_output_mailbox "$runtime_dir/fish-$token.out"
    command mkfifo -m 600 -- "$_shell_sense_fish_input_fifo"; or return 1
    printf '' >"$_shell_sense_fish_output_mailbox"
    command chmod 600 -- "$_shell_sense_fish_input_fifo" "$_shell_sense_fish_output_mailbox"

    set -l state_base "$XDG_STATE_HOME"
    test -n "$state_base"; or set state_base "$HOME/.local/state"
    command mkdir -p -m 700 -- "$state_base/shell-sense"
    set -g _shell_sense_fish_log "$state_base/shell-sense/worker-$fish_pid.log"
    set -l arguments worker --shell fish \
        --shell-executable (status fish-path) --shell-version "$version" \
        --shell-input-fifo "$_shell_sense_fish_input_fifo" \
        --shell-output-mailbox "$_shell_sense_fish_output_mailbox" \
        --shell-process-id "$fish_pid"
    set -q SHELL_SENSE_SOCKET; and set -a arguments --socket "$SHELL_SENSE_SOCKET"
    set -q SHELL_SENSE_CONFIG; and set -a arguments --config "$SHELL_SENSE_CONFIG"
    set -q SHELL_SENSE_PROFILE; and set -a arguments --profile "$SHELL_SENSE_PROFILE"
    command $command_line $arguments </dev/null >>"$_shell_sense_fish_log" 2>&1 &
    set -g _shell_sense_fish_worker_pid $last_pid

    for attempt in (seq 200)
        __shell_sense_fish_drain
        test $_shell_sense_fish_ready -eq 1; and test $_shell_sense_fish_configured -eq 1; and break
        command kill -0 $_shell_sense_fish_worker_pid 2>/dev/null; or break
        sleep 0.005
    end
    test $_shell_sense_fish_ready -eq 1; and test $_shell_sense_fish_configured -eq 1; or return 1
    __shell_sense_fish_bindings
end

function __shell_sense_fish_reset_transport
    __shell_sense_fish_clear_popup
    set -g _shell_sense_fish_ready 0
    set -g _shell_sense_fish_configured 0
    set -g _shell_sense_fish_worker_pid 0
    test -n "$_shell_sense_fish_input_fifo"; and command unlink -- "$_shell_sense_fish_input_fifo" 2>/dev/null
    test -n "$_shell_sense_fish_output_mailbox"; and command unlink -- "$_shell_sense_fish_output_mailbox" 2>/dev/null
    set -g _shell_sense_fish_input_fifo ''
    set -g _shell_sense_fish_output_mailbox ''
end

function __shell_sense_fish_ensure_worker --on-event fish_prompt
    __shell_sense_fish_start
end

function __shell_sense_fish_cleanup --on-event fish_exit
    __shell_sense_fish_clear_popup
    if test $_shell_sense_fish_ready -eq 1
        __shell_sense_fish_send goodbye
    end
    test $_shell_sense_fish_worker_pid -gt 0; and command kill $_shell_sense_fish_worker_pid 2>/dev/null
    test -n "$_shell_sense_fish_input_fifo"; and command unlink -- "$_shell_sense_fish_input_fifo" 2>/dev/null
    test -n "$_shell_sense_fish_output_mailbox"; and command unlink -- "$_shell_sense_fish_output_mailbox" 2>/dev/null
end
