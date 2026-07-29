# Readline client for Shell Sense on Bash 5.2 and newer.

declare -gi _shell_sense_bash_ready=0
declare -gi _shell_sense_bash_configured=0
declare -gi _shell_sense_bash_worker_pid=0
declare -gi _shell_sense_bash_request=0
declare -gi _shell_sense_bash_generation=0
declare -gi _shell_sense_bash_active_request=0
declare -gi _shell_sense_bash_active_generation=0
declare -gi _shell_sense_bash_active_point=0
declare -g _shell_sense_bash_active_buffer=
declare -gi _shell_sense_bash_after_accept=1
declare -g _shell_sense_bash_activation_mode=continuous
declare -gi _shell_sense_bash_fuzzy_min_chars=3
declare -gi _shell_sense_bash_max_rows=10
declare -gi _shell_sense_bash_scrolloff=2
declare -gi _shell_sense_bash_cycle=1
declare -gi _shell_sense_bash_max_width=140
declare -gi _shell_sense_bash_min_width=24
declare -gi _shell_sense_bash_popup_enabled=1
declare -gi _shell_sense_bash_padding=1
declare -gi _shell_sense_bash_show_descriptions=1
declare -gi _shell_sense_bash_show_scrollbar=1
declare -g _shell_sense_bash_scrollbar_character='▐'
declare -gi _shell_sense_bash_documentation_padding=0
declare -gi _shell_sense_bash_show_documentation_scrollbar=1
declare -g _shell_sense_bash_indicator_mode=icon
declare -g _shell_sense_bash_selected_marker=
declare -gi _shell_sense_bash_popup_lines=0
declare -gi _shell_sense_bash_popup_visible=0
declare -gi _shell_sense_bash_external_presentation=0
declare -gi _shell_sense_bash_selected=1
declare -gi _shell_sense_bash_selected_absolute=0
declare -gi _shell_sense_bash_navigation_serial=0
declare -gi _shell_sense_bash_total=0
declare -gi _shell_sense_bash_window_start=0
declare -gi _shell_sense_bash_menu_view_start=0
declare -gi _shell_sense_bash_menu_view_request=0
declare -gi _shell_sense_bash_menu_view_generation=0
declare -gi _shell_sense_bash_view_ready=0
declare -gi _shell_sense_bash_view_max_label_cells=0
declare -gi _shell_sense_bash_view_max_described_cells=0
declare -gi _shell_sense_bash_view_revision=0
declare -gi _shell_sense_bash_view_building=0
declare -gi _shell_sense_bash_menu_width=0
declare -g _shell_sense_bash_border=none
declare -g _shell_sense_bash_documentation_item=
declare -g _shell_sense_bash_documentation_placement=
declare -gi _shell_sense_bash_documentation_width=0
declare -gi _shell_sense_bash_documentation_expected=0
declare -gi _shell_sense_bash_documentation_viewport_rows=0
declare -gi _shell_sense_bash_documentation_offset=0
declare -gi _shell_sense_bash_documentation_total=0
declare -gi _shell_sense_bash_documentation_scrollbar=0
declare -ga _shell_sense_bash_documentation_kinds=()
declare -ga _shell_sense_bash_documentation_cells=()
declare -ga _shell_sense_bash_documentation_lines=()
declare -gi _shell_sense_bash_pending_after_accept=0
declare -gi _shell_sense_bash_wait_fd=-1
declare -gi _shell_sense_bash_bindings_captured=0
declare -g _shell_sense_bash_installed_keymap=
declare -ga _shell_sense_bash_activation_characters=()
declare -ga _shell_sense_bash_immediate_characters=()
declare -ga _shell_sense_bash_activation_events=()
declare -ga _shell_sense_bash_binding_states=()
declare -ga _shell_sense_bash_binding_keys=()
declare -ga _shell_sense_bash_binding_actions=()
declare -ga _shell_sense_bash_view_ids=()
declare -ga _shell_sense_bash_view_labels=()
declare -ga _shell_sense_bash_view_label_cells=()
declare -ga _shell_sense_bash_view_kinds=()
declare -ga _shell_sense_bash_view_details=()
declare -ga _shell_sense_bash_view_detail_cells=()
declare -ga _shell_sense_bash_view_matches=()
declare -gA _shell_sense_bash_original_binding_types=()
declare -gA _shell_sense_bash_original_binding_values=()
declare -g _shell_sense_bash_style_reset=$'\e[0m'
declare -g _shell_sense_bash_style_menu=$'\e[38;2;187;187;187;48;2;32;32;32m'
declare -g _shell_sense_bash_style_label=$'\e[38;2;187;187;187m'
declare -g _shell_sense_bash_style_detail=$'\e[38;2;128;128;128m'
declare -g _shell_sense_bash_style_kind=$'\e[38;2;128;128;128m'
declare -g _shell_sense_bash_style_selected=$'\e[48;2;52;59;65m'
declare -g _shell_sense_bash_style_match=$'\e[38;2;24;162;254;1m'
declare -g _shell_sense_bash_style_scrollbar_thumb=$'\e[38;2;187;187;187m'
declare -g _shell_sense_bash_style_scrollbar_gutter=$'\e[38;2;52;59;65m'
declare -g _shell_sense_bash_style_documentation=$'\e[38;2;212;212;212;48;2;32;32;32m'
declare -g _shell_sense_bash_style_documentation_border=$'\e[38;2;212;212;212;48;2;32;32;32m'
declare -g _shell_sense_bash_style_documentation_heading=$'\e[38;2;24;162;254;48;2;32;32;32;1m'
declare -g _shell_sense_bash_style_documentation_code=$'\e[38;2;206;145;120;48;2;32;32;32m'
declare -g _shell_sense_bash_style_documentation_quote=$'\e[38;2;128;128;128;48;2;32;32;32m'
declare -gA _shell_sense_bash_style_kinds=()

_shell_sense_bash_reset_documentation() {
  _shell_sense_bash_documentation_item=
  _shell_sense_bash_documentation_placement=
  _shell_sense_bash_documentation_width=0
  _shell_sense_bash_documentation_expected=0
  _shell_sense_bash_documentation_viewport_rows=0
  _shell_sense_bash_documentation_offset=0
  _shell_sense_bash_documentation_total=0
  _shell_sense_bash_documentation_scrollbar=0
  _shell_sense_bash_documentation_kinds=()
  _shell_sense_bash_documentation_cells=()
  _shell_sense_bash_documentation_lines=()
}

_shell_sense_bash_netstring() {
  local LC_ALL=C value=$1
  _shell_sense_bash_netstring_value="${#value}:$value,"
}

_shell_sense_bash_encode_message() {
  local command=$1
  shift
  local wire
  _shell_sense_bash_netstring "$command"
  wire=$_shell_sense_bash_netstring_value
  _shell_sense_bash_netstring "$#"
  wire+=$_shell_sense_bash_netstring_value
  local field
  for field in "$@"; do
    _shell_sense_bash_netstring "$field"
    wire+=$_shell_sense_bash_netstring_value
  done
  _shell_sense_bash_encoded=$wire
}

_shell_sense_bash_send() {
  ((_shell_sense_bash_worker_pid > 0)) || return 1
  kill -0 "$_shell_sense_bash_worker_pid" 2>/dev/null || return 1
  _shell_sense_bash_encode_message "$@"
  printf '%s' "$_shell_sense_bash_encoded" >"$_shell_sense_bash_input_fifo"
}

_shell_sense_bash_ansi_style() {
  local specification=$1 component hex
  local -a codes=()
  local IFS=,
  read -r -a components <<<"$specification"
  for component in "${components[@]}"; do
    case $component in
      bold) codes+=(1) ;;
      dim|faint) codes+=(2) ;;
      italic) codes+=(3) ;;
      underline) codes+=(4) ;;
      fg=\#[[:xdigit:]][[:xdigit:]][[:xdigit:]][[:xdigit:]][[:xdigit:]][[:xdigit:]])
        hex=${component#fg=#}
        codes+=(38 2 "$((16#${hex:0:2}))" "$((16#${hex:2:2}))" "$((16#${hex:4:2}))")
        ;;
      bg=\#[[:xdigit:]][[:xdigit:]][[:xdigit:]][[:xdigit:]][[:xdigit:]][[:xdigit:]])
        hex=${component#bg=#}
        codes+=(48 2 "$((16#${hex:0:2}))" "$((16#${hex:2:2}))" "$((16#${hex:4:2}))")
        ;;
    esac
  done
  ((${#codes[@]})) || return
  local joined
  IFS=';'
  joined=${codes[*]}
  printf '\e[%sm' "$joined"
}

_shell_sense_bash_apply_style() {
  local name=$1 ansi
  ansi=$(_shell_sense_bash_ansi_style "$2")
  case $name in
    menu) _shell_sense_bash_style_menu=$ansi ;;
    label) _shell_sense_bash_style_label=$ansi ;;
    detail) _shell_sense_bash_style_detail=$ansi ;;
    kind) _shell_sense_bash_style_kind=$ansi ;;
    selected) _shell_sense_bash_style_selected=$ansi ;;
    label-match) _shell_sense_bash_style_match=$ansi ;;
    scrollbar-thumb) _shell_sense_bash_style_scrollbar_thumb=$ansi ;;
    scrollbar-gutter) _shell_sense_bash_style_scrollbar_gutter=$ansi ;;
    documentation) _shell_sense_bash_style_documentation=$ansi ;;
    documentation-border) _shell_sense_bash_style_documentation_border=$ansi ;;
    documentation-heading) _shell_sense_bash_style_documentation_heading=$ansi ;;
    documentation-code) _shell_sense_bash_style_documentation_code=$ansi ;;
    documentation-quote) _shell_sense_bash_style_documentation_quote=$ansi ;;
  esac
}

_shell_sense_bash_clear_popup() {
  if ((_shell_sense_bash_popup_lines <= 0)); then
    _shell_sense_bash_popup_visible=0
    return
  fi
  printf '\e[?2026h\e7'
  local -i row
  for ((row = 0; row < _shell_sense_bash_popup_lines; row++)); do
    printf '\e[B\r\e[2K'
  done
  printf '\e8\e[?2026l'
  _shell_sense_bash_popup_lines=0
  _shell_sense_bash_popup_visible=0
}

_shell_sense_bash_kind_icon() {
  local kind=$1
  _shell_sense_bash_icon=
  if [[ $_shell_sense_bash_indicator_mode == text ]]; then
    _shell_sense_bash_icon="[${kind:0:1}]"
    return
  elif [[ $_shell_sense_bash_indicator_mode == none ]]; then
    return
  fi
  local icon
  case $kind in
    directory) icon='󰉋' ;;
    file|symlink) icon='󰈔' ;;
    option|option-value) icon='󰌋' ;;
    command|builtin|function|alias|subcommand) icon='󰆍' ;;
    variable) icon='󰫧' ;;
    user) icon='󰀄' ;;
    host) icon='󰒋' ;;
    process|job|service) icon='󰐊' ;;
    *) icon='󰦨' ;;
  esac
  if [[ $_shell_sense_bash_indicator_mode == both ]]; then
    _shell_sense_bash_icon="$icon [${kind:0:1}]"
  else
    _shell_sense_bash_icon=$icon
  fi
}

_shell_sense_bash_render_label() {
  local label=$1 ranges=$2 selected_style=$3
  if [[ -z $ranges ]]; then
    printf '%s%s%s%s' "$selected_style" "$_shell_sense_bash_style_label" "$label" "$_shell_sense_bash_style_reset"
    return
  fi
  local -i cursor=0 start finish
  local range
  local IFS=,
  read -r -a split_ranges <<<"$ranges"
  for range in "${split_ranges[@]}"; do
    start=${range%%:*}
    finish=${range#*:}
    if ((start > cursor)); then
      printf '%s%s%s' "$selected_style" "$_shell_sense_bash_style_label" "${label:cursor:start-cursor}"
    fi
    if ((finish > start)); then
      printf '%s%s%s' "$selected_style" "$_shell_sense_bash_style_match" "${label:start:finish-start}"
    fi
    cursor=$finish
  done
  ((cursor >= ${#label})) || printf '%s%s%s' "$selected_style" "$_shell_sense_bash_style_label" "${label:cursor}"
  printf '%s' "$_shell_sense_bash_style_reset"
}

_shell_sense_bash_documentation_row_count() {
  _shell_sense_bash_documentation_row_count=$_shell_sense_bash_documentation_viewport_rows
  if ((_shell_sense_bash_documentation_row_count > 0)) && [[ $_shell_sense_bash_border != none ]]; then
    ((_shell_sense_bash_documentation_row_count += 2))
  fi
}

_shell_sense_bash_menu_viewport_start_for() {
  local -i selected=$1 start=$2 total=$_shell_sense_bash_total
  local -i rows=$_shell_sense_bash_max_rows scrolloff=$_shell_sense_bash_scrolloff maximum_start=0
  ((rows > total)) && rows=$total
  if ((rows <= 0)); then
    _shell_sense_bash_menu_viewport_start=0
    return
  fi
  ((scrolloff >= rows)) && scrolloff=$((rows - 1))
  maximum_start=$((total - rows))
  ((start < 0)) && start=0
  ((start > maximum_start)) && start=$maximum_start
  ((selected < 0)) && selected=0
  ((selected >= total)) && selected=$((total - 1))

  if ((selected < start + scrolloff)); then
    start=$((selected - scrolloff))
  elif ((selected >= start + rows - scrolloff)); then
    start=$((selected - rows + scrolloff + 1))
  fi
  ((start < 0)) && start=0
  ((start > maximum_start)) && start=$maximum_start
  _shell_sense_bash_menu_viewport_start=$start
}

_shell_sense_bash_update_menu_viewport() {
  _shell_sense_bash_menu_viewport_start_for "$1" "$_shell_sense_bash_menu_view_start"
  _shell_sense_bash_menu_view_start=$_shell_sense_bash_menu_viewport_start
}

_shell_sense_bash_cached_menu_viewport_contains() {
  local -i start=$1 rows=$_shell_sense_bash_max_rows
  ((rows > _shell_sense_bash_total)) && rows=$_shell_sense_bash_total
  local -i cached_end=$((_shell_sense_bash_window_start + ${#_shell_sense_bash_view_ids[@]}))
  ((start >= _shell_sense_bash_window_start && start + rows <= cached_end))
}

_shell_sense_bash_menu_scrollbar_geometry() {
  local -i rows=$1 total=$2 offset=$3 maximum_offset track
  _shell_sense_bash_scrollbar_thumb_rows=0
  _shell_sense_bash_scrollbar_thumb_first=0
  ((rows > 0 && total > rows)) || return
  _shell_sense_bash_scrollbar_thumb_rows=$((rows * rows / total))
  ((_shell_sense_bash_scrollbar_thumb_rows >= 1)) || _shell_sense_bash_scrollbar_thumb_rows=1
  maximum_offset=$((total - rows))
  ((offset < 0)) && offset=0
  ((offset > maximum_offset)) && offset=$maximum_offset
  track=$((rows - _shell_sense_bash_scrollbar_thumb_rows))
  ((track == 0)) ||
    _shell_sense_bash_scrollbar_thumb_first=$(((offset * track + maximum_offset / 2) / maximum_offset))
}

_shell_sense_bash_render_documentation_row() {
  local -i row=$1 width=$_shell_sense_bash_documentation_width border_cells=0
  local top_left=╭ top_right=╮ bottom_left=╰ bottom_right=╯ horizontal=─ vertical=│
  case $_shell_sense_bash_border in
    sharp) top_left=┌; top_right=┐; bottom_left=└; bottom_right=┘ ;;
    ascii) top_left=+; top_right=+; bottom_left=+; bottom_right=+; horizontal=-; vertical='|' ;;
    none) top_left=; top_right=; bottom_left=; bottom_right=; horizontal=; vertical= ;;
  esac
  if [[ $_shell_sense_bash_border != none ]]; then
    border_cells=2
    local rule
    printf -v rule '%*s' "$((width - 2))" ''
    rule=${rule// /$horizontal}
    if ((row == 1)); then
      printf '%s%s%s%s%s%s' "$_shell_sense_bash_style_documentation" "$_shell_sense_bash_style_documentation_border" "$top_left" "$rule" "$top_right" "$_shell_sense_bash_style_reset"
      return
    elif ((row == _shell_sense_bash_documentation_row_count)); then
      printf '%s%s%s%s%s%s' "$_shell_sense_bash_style_documentation" "$_shell_sense_bash_style_documentation_border" "$bottom_left" "$rule" "$bottom_right" "$_shell_sense_bash_style_reset"
      return
    fi
    ((row -= 1))
  fi
  local text=${_shell_sense_bash_documentation_lines[row-1]-}
  local kind=${_shell_sense_bash_documentation_kinds[row-1]-}
  local -i cells=${_shell_sense_bash_documentation_cells[row-1]:-0}
  local -i scrollbar_cells=0
  ((_shell_sense_bash_show_documentation_scrollbar && _shell_sense_bash_documentation_scrollbar)) && scrollbar_cells=1
  local -i content_width=$((width - border_cells - 2 * _shell_sense_bash_documentation_padding - scrollbar_cells))
  local -i fill=$((content_width - cells))
  ((fill >= 0)) || fill=0
  local text_style=$_shell_sense_bash_style_documentation
  case $kind in
    heading) text_style=$_shell_sense_bash_style_documentation_heading ;;
    code) text_style=$_shell_sense_bash_style_documentation_code ;;
    quote|separator) text_style=$_shell_sense_bash_style_documentation_quote ;;
  esac
  printf '%s%s%*s%s%s%s%*s%*s' \
    "$_shell_sense_bash_style_documentation" "$vertical" "$_shell_sense_bash_documentation_padding" '' \
    "$text_style" "$text" "$_shell_sense_bash_style_documentation" "$fill" '' \
    "$_shell_sense_bash_documentation_padding" ''
  if ((scrollbar_cells)); then
    local -i rows=$_shell_sense_bash_documentation_viewport_rows
    local -i total=$_shell_sense_bash_documentation_total
    local -i thumb_rows=$((rows * rows / total))
    ((thumb_rows >= 1)) || thumb_rows=1
    local -i track=$((rows - thumb_rows))
    local -i maximum_offset=$((total - rows))
    local -i thumb_first=0
    ((track == 0)) ||
      thumb_first=$(((_shell_sense_bash_documentation_offset * track + maximum_offset / 2) / maximum_offset))
    if ((row > thumb_first && row <= thumb_first + thumb_rows)); then
      printf '%s%s' "$_shell_sense_bash_style_scrollbar_thumb" "$_shell_sense_bash_scrollbar_character"
    else
      printf '%s%s' "$_shell_sense_bash_style_scrollbar_gutter" "$_shell_sense_bash_scrollbar_character"
    fi
  fi
  printf '%s%s%s' "$_shell_sense_bash_style_documentation" "$vertical" "$_shell_sense_bash_style_reset"
}

_shell_sense_bash_render_popup() {
  local -i previous_popup_lines=$_shell_sense_bash_popup_lines
  local -i item_count=${#_shell_sense_bash_view_labels[@]}
  if ((!_shell_sense_bash_popup_enabled || _shell_sense_bash_external_presentation || item_count == 0)); then
    _shell_sense_bash_clear_popup
    _shell_sense_bash_install_keymap closed
    return
  fi
  local -i row_count=$item_count first last
  ((row_count > _shell_sense_bash_max_rows)) && row_count=$_shell_sense_bash_max_rows
  first=$((_shell_sense_bash_menu_view_start - _shell_sense_bash_window_start))
  ((first >= 0 && first + row_count <= item_count)) || return 1
  last=$((first + row_count - 1))

  local -i columns=${COLUMNS:-80} index
  local -i content_width=$_shell_sense_bash_view_max_label_cells
  ((_shell_sense_bash_show_descriptions)) && content_width=$_shell_sense_bash_view_max_described_cells
  local -i marker_cells=0 indicator_cells=0
  [[ -z $_shell_sense_bash_selected_marker ]] || marker_cells=$((${#_shell_sense_bash_selected_marker} + 1))
  case $_shell_sense_bash_indicator_mode in
    icon) indicator_cells=2 ;;
    text) indicator_cells=4 ;;
    both) indicator_cells=6 ;;
  esac
  local -i width=$((content_width + marker_cells + indicator_cells + 2 * _shell_sense_bash_padding))
  ((_shell_sense_bash_show_scrollbar && _shell_sense_bash_total > row_count)) && width=$((width + 1))
  if ((_shell_sense_bash_menu_width > 0)); then
    width=$_shell_sense_bash_menu_width
  else
    ((width >= _shell_sense_bash_min_width)) || width=$_shell_sense_bash_min_width
    ((width <= _shell_sense_bash_max_width)) || width=$_shell_sense_bash_max_width
  fi
  ((width <= columns)) || width=$columns

  _shell_sense_bash_documentation_row_count
  local -i documentation_rows=$_shell_sense_bash_documentation_row_count

  local -i thumb_first=0 thumb_rows=0
  if ((_shell_sense_bash_show_scrollbar && _shell_sense_bash_total > row_count)); then
    _shell_sense_bash_menu_scrollbar_geometry "$row_count" "$_shell_sense_bash_total" \
      "$_shell_sense_bash_menu_view_start"
    thumb_first=$_shell_sense_bash_scrollbar_thumb_first
    thumb_rows=$_shell_sense_bash_scrollbar_thumb_rows
  fi

  printf '\e[?2026h\e7'
  local -i previous_row
  for ((previous_row = 0; previous_row < previous_popup_lines; previous_row++)); do
    printf '\e[B\r\e[2K'
  done
  printf '\e8'
  local selected_style row_style marker icon detail kind_style scrollbar_style
  local -i spaces row=0 scrollbar_cells=0
  ((thumb_rows == 0)) || scrollbar_cells=1
  for ((index = first; index <= last; index++)); do
    ((row += 1))
    selected_style=
    marker=
    if ((index + 1 == _shell_sense_bash_selected)); then
      selected_style=$_shell_sense_bash_style_selected
      marker=$_shell_sense_bash_selected_marker
    fi
    _shell_sense_bash_kind_icon "${_shell_sense_bash_view_kinds[index]}"
    icon=$_shell_sense_bash_icon
    kind_style=${_shell_sense_bash_style_kinds[${_shell_sense_bash_view_kinds[index]}]-$_shell_sense_bash_style_kind}
    detail=
    ((_shell_sense_bash_show_descriptions)) && detail=${_shell_sense_bash_view_details[index]}
    spaces=$((width - 2 * _shell_sense_bash_padding - marker_cells - indicator_cells - scrollbar_cells - _shell_sense_bash_view_label_cells[index]))
    [[ -z $detail ]] || spaces=$((spaces - _shell_sense_bash_view_detail_cells[index]))
    ((spaces >= 0)) || spaces=0

    row_style=$_shell_sense_bash_style_menu$selected_style
    printf '\e[B\r%s%*s' "$row_style" "$_shell_sense_bash_padding" ''
    ((marker_cells == 0)) || printf '%-*s' "$marker_cells" "$marker"
    ((indicator_cells == 0)) || printf '%s%-*s%s' "$kind_style" "$indicator_cells" "$icon" "$row_style"
    _shell_sense_bash_render_label "${_shell_sense_bash_view_labels[index]}" "${_shell_sense_bash_view_matches[index]}" "$row_style"
    printf '%s%*s' "$row_style" "$spaces" ''
    [[ -z $detail ]] || printf '%s%s%s%s' "$row_style" "$_shell_sense_bash_style_detail" "$detail" "$_shell_sense_bash_style_reset"
    printf '%s%*s' "$row_style" "$_shell_sense_bash_padding" ''
    if ((thumb_rows > 0)); then
      scrollbar_style=$_shell_sense_bash_style_scrollbar_gutter
      if ((row > thumb_first && row <= thumb_first + thumb_rows)); then
        scrollbar_style=$_shell_sense_bash_style_scrollbar_thumb
        printf '%s%s' "$scrollbar_style" "$_shell_sense_bash_scrollbar_character"
      else
        printf '%s ' "$scrollbar_style"
      fi
    fi
    printf '%s' "$_shell_sense_bash_style_reset"
    if [[ $_shell_sense_bash_documentation_placement == side ]] && ((row <= documentation_rows)); then
      printf ' '
      _shell_sense_bash_render_documentation_row "$row"
    fi
    printf '\e[K'
  done
  local -i documentation_row
  if [[ $_shell_sense_bash_documentation_placement == side ]]; then
    for ((documentation_row = row_count + 1; documentation_row <= documentation_rows; documentation_row++)); do
      printf '\e[B\r%s%*s%s ' "$_shell_sense_bash_style_menu" "$width" '' "$_shell_sense_bash_style_reset"
      _shell_sense_bash_render_documentation_row "$documentation_row"
      printf '\e[K'
    done
  elif [[ $_shell_sense_bash_documentation_placement == below ]]; then
    for ((documentation_row = 1; documentation_row <= documentation_rows; documentation_row++)); do
      printf '\e[B\r'
      _shell_sense_bash_render_documentation_row "$documentation_row"
      printf '\e[K'
    done
  fi
  printf '\e8\e[?2026l'
  if [[ $_shell_sense_bash_documentation_placement == side ]]; then
    ((row_count >= documentation_rows)) && _shell_sense_bash_popup_lines=$row_count ||
      _shell_sense_bash_popup_lines=$documentation_rows
  else
    _shell_sense_bash_popup_lines=$((row_count + documentation_rows))
  fi
  _shell_sense_bash_popup_visible=1
  _shell_sense_bash_install_keymap popup
}

_shell_sense_bash_split_record() {
  local rest=$1 tab=$'\t'
  _shell_sense_bash_record_fields=()
  while [[ $rest == *"$tab"* ]]; do
    _shell_sense_bash_record_fields+=("${rest%%"$tab"*}")
    rest=${rest#*"$tab"}
  done
  _shell_sense_bash_record_fields+=("$rest")
}

_shell_sense_bash_dispatch() {
  local command=$1
  shift
  case $command in
    ready) _shell_sense_bash_ready=1 ;;
    config)
      _shell_sense_bash_activation_mode=$1
      _shell_sense_bash_after_accept=$3
      _shell_sense_bash_popup_enabled=$4
      _shell_sense_bash_max_rows=$5
      _shell_sense_bash_max_width=$6
      _shell_sense_bash_min_width=$7
      _shell_sense_bash_padding=$8
      _shell_sense_bash_border=${10}
      _shell_sense_bash_show_scrollbar=${13}
      _shell_sense_bash_show_descriptions=${15}
      _shell_sense_bash_indicator_mode=${17}
      _shell_sense_bash_selected_marker=${18}
      _shell_sense_bash_fuzzy_min_chars=${20}
      local -i cursor=21 count index
      count=${!cursor}; ((cursor += 1))
      _shell_sense_bash_activation_characters=()
      for ((index = 0; index < count; index++, cursor++)); do _shell_sense_bash_activation_characters+=("${!cursor}"); done
      count=${!cursor}; ((cursor += 1))
      _shell_sense_bash_immediate_characters=()
      for ((index = 0; index < count; index++, cursor++)); do _shell_sense_bash_immediate_characters+=("${!cursor}"); done
      count=${!cursor}; ((cursor += 1))
      _shell_sense_bash_activation_events=()
      for ((index = 0; index < count; index++, cursor++)); do _shell_sense_bash_activation_events+=("${!cursor}"); done
      ;;
    popup-option)
      case $1 in
        scrollbar-character) _shell_sense_bash_scrollbar_character=$2 ;;
        scrolloff) _shell_sense_bash_scrolloff=$2 ;;
        cycle) _shell_sense_bash_cycle=$2 ;;
        documentation-padding) _shell_sense_bash_documentation_padding=$2 ;;
        documentation-scrollbar) _shell_sense_bash_show_documentation_scrollbar=$2 ;;
      esac
      ;;
    style) _shell_sense_bash_apply_style "$1" "$2" ;;
    kind-style)
      local kind_style
      kind_style=$(_shell_sense_bash_ansi_style "$2")
      _shell_sense_bash_style_kinds[$1]=$kind_style
      ;;
    keybinding)
      _shell_sense_bash_binding_states+=("$1")
      _shell_sense_bash_binding_keys+=("$2")
      _shell_sense_bash_binding_actions+=("$3")
      ;;
    config-end) _shell_sense_bash_configured=1 ;;
    presentation)
      [[ $# == 1 ]] || return
      _shell_sense_bash_external_presentation=$1
      if ((_shell_sense_bash_external_presentation)); then
        _shell_sense_bash_clear_popup
        _shell_sense_bash_install_keymap closed
      else
        _shell_sense_bash_render_popup
      fi
      ;;
    capture-request) _shell_sense_bash_capture "$1" "$2" "$3" "$4" ;;
    view-begin)
      (($# >= 17)) || return
      [[ $2 == "$_shell_sense_bash_active_request" && $3 == "$_shell_sense_bash_active_generation" ]] || return
      [[ ${15} =~ ^[0-9]+$ && (${16} == replace || ${16} == preserve) && ${17} =~ ^[0-9]+$ ]] || return
      if ((10#${15} < _shell_sense_bash_navigation_serial)); then
        _shell_sense_bash_view_building=0
        return
      fi
      _shell_sense_bash_view_building=1
      _shell_sense_bash_view_revision=$4
      _shell_sense_bash_navigation_serial=${15}
      _shell_sense_bash_menu_width=0
      [[ ${16} == preserve ]] || _shell_sense_bash_reset_documentation
      _shell_sense_bash_selected=$(($5 + 1))
      _shell_sense_bash_selected_absolute=${12}
      _shell_sense_bash_total=${10}
      _shell_sense_bash_window_start=${11}
      _shell_sense_bash_view_max_label_cells=${13}
      _shell_sense_bash_view_max_described_cells=${14}
      _shell_sense_bash_view_ids=()
      _shell_sense_bash_view_labels=()
      _shell_sense_bash_view_label_cells=()
      _shell_sense_bash_view_kinds=()
      _shell_sense_bash_view_details=()
      _shell_sense_bash_view_detail_cells=()
      _shell_sense_bash_view_matches=()
      ;;
    view-chunk)
      [[ $_shell_sense_bash_view_building == 1 &&
         $1 == "$_shell_sense_bash_active_request" &&
         $2 == "$_shell_sense_bash_active_generation" ]] || return
      local -i item_count=$3 offset=4 item
      for ((item = 0; item < item_count; item++, offset += 11)); do
        local -i position=$offset
        _shell_sense_bash_view_ids+=("${!position}")
        ((position += 1)); _shell_sense_bash_view_labels+=("${!position}")
        ((position += 1)); _shell_sense_bash_view_label_cells+=("${!position}")
        ((position += 1)); _shell_sense_bash_view_kinds+=("${!position}")
        ((position += 1)); _shell_sense_bash_view_details+=("${!position}")
        ((position += 1)); _shell_sense_bash_view_detail_cells+=("${!position}")
        ((position += 4)); _shell_sense_bash_view_matches+=("${!position}")
      done
      ;;
    view-layout)
      [[ $# == 4 && $1 == "$_shell_sense_bash_active_request" &&
         $2 == "$_shell_sense_bash_active_generation" &&
         $3 == "$_shell_sense_bash_view_revision" ]] || return
      _shell_sense_bash_menu_width=$4
      ;;
    selection-changed)
      [[ $# == 6 && $1 == "$_shell_sense_bash_active_request" &&
         $2 == "$_shell_sense_bash_active_generation" &&
         $3 == "$_shell_sense_bash_view_revision" &&
         $4 =~ ^[0-9]+$ && $5 =~ ^[0-9]+$ && $6 =~ ^[0-9]+$ ]] || return
      ((10#$4 >= _shell_sense_bash_navigation_serial)) || return
      local -i selected=$((10#$5 + 1))
      ((selected >= 1 && selected <= ${#_shell_sense_bash_view_ids[@]})) || return
      _shell_sense_bash_navigation_serial=$4
      _shell_sense_bash_selected=$selected
      _shell_sense_bash_selected_absolute=$6
      _shell_sense_bash_update_menu_viewport "$_shell_sense_bash_selected_absolute"
      _shell_sense_bash_view_ready=1
      _shell_sense_bash_render_popup
      ;;
    documentation-begin)
      [[ $# == 10 && $1 == "$_shell_sense_bash_active_request" &&
         $2 == "$_shell_sense_bash_active_generation" &&
         ($4 == side || $4 == below) ]] || return
      _shell_sense_bash_reset_documentation
      _shell_sense_bash_documentation_item=$3
      _shell_sense_bash_documentation_placement=$4
      _shell_sense_bash_documentation_width=$5
      _shell_sense_bash_documentation_expected=$6
      _shell_sense_bash_documentation_viewport_rows=$7
      _shell_sense_bash_documentation_offset=$8
      _shell_sense_bash_documentation_total=$9
      _shell_sense_bash_documentation_scrollbar=${10}
      ;;
    documentation-chunk)
      [[ $# -ge 4 && $1 == "$_shell_sense_bash_active_request" &&
         $2 == "$_shell_sense_bash_active_generation" &&
         $3 == "$_shell_sense_bash_documentation_item" ]] || return
      local -i line_count=$4 offset=5 line
      (($# == 4 + line_count * 3)) || return
      for ((line = 0; line < line_count; line++, offset += 3)); do
        local -i position=$offset
        _shell_sense_bash_documentation_kinds+=("${!position}")
        ((position += 1)); _shell_sense_bash_documentation_cells+=("${!position}")
        ((position += 1)); _shell_sense_bash_documentation_lines+=("${!position}")
      done
      ;;
    documentation-end)
      [[ $# == 3 && $1 == "$_shell_sense_bash_active_request" &&
         $2 == "$_shell_sense_bash_active_generation" &&
         $3 == "$_shell_sense_bash_documentation_item" &&
         ${#_shell_sense_bash_documentation_lines[@]} -eq $_shell_sense_bash_documentation_expected ]] || return
      ((_shell_sense_bash_view_building)) || _shell_sense_bash_render_popup
      _shell_sense_bash_view_ready=1
      ;;
    documentation-clear)
      [[ $# == 2 && $1 == "$_shell_sense_bash_active_request" &&
         $2 == "$_shell_sense_bash_active_generation" ]] || return
      _shell_sense_bash_reset_documentation
      ((_shell_sense_bash_view_building)) || _shell_sense_bash_render_popup
      _shell_sense_bash_view_ready=1
      ;;
    view-end)
      [[ $# == 3 && $_shell_sense_bash_view_building == 1 &&
         $1 == "$_shell_sense_bash_active_request" &&
         $2 == "$_shell_sense_bash_active_generation" &&
         $3 == "$_shell_sense_bash_view_revision" ]] || return
      _shell_sense_bash_view_building=0
      if ((_shell_sense_bash_menu_view_request != 10#$1 ||
           _shell_sense_bash_menu_view_generation != 10#$2)); then
        _shell_sense_bash_menu_view_start=0
        _shell_sense_bash_menu_view_request=$1
        _shell_sense_bash_menu_view_generation=$2
      fi
      _shell_sense_bash_update_menu_viewport "$_shell_sense_bash_selected_absolute"
      _shell_sense_bash_render_popup
      _shell_sense_bash_view_ready=1
      ;;
    accept-bash) _shell_sense_bash_apply_acceptance "$@" ;;
    request-cancelled)
      [[ $1 == "$_shell_sense_bash_active_request" &&
         $2 == "$_shell_sense_bash_active_generation" ]] || return
      _shell_sense_bash_close_popup
      ;;
    error) _shell_sense_bash_last_error="$1: $2" ;;
  esac
}

_shell_sense_bash_drain() {
  [[ -f $_shell_sense_bash_output_mailbox ]] || return
  local -a records=()
  mapfile -t records <"$_shell_sense_bash_output_mailbox"
  ((${#records[@]})) || return
  kill -USR2 "$_shell_sense_bash_worker_pid" 2>/dev/null || true
  local record encoded field
  local -a decoded=()
  for record in "${records[@]}"; do
    _shell_sense_bash_split_record "$record"
    ((${#_shell_sense_bash_record_fields[@]} >= 2)) || continue
    local command=${_shell_sense_bash_record_fields[0]}
    local -i field_count=${_shell_sense_bash_record_fields[1]}
    ((${#_shell_sense_bash_record_fields[@]} == field_count + 2)) || continue
    decoded=()
    for encoded in "${_shell_sense_bash_record_fields[@]:2}"; do
      printf -v field '%b' "$encoded"
      decoded+=("$field")
    done
    _shell_sense_bash_dispatch "$command" "${decoded[@]}"
  done
}

_shell_sense_bash_wait_for() {
  local condition=$1
  local -i attempt
  for ((attempt = 0; attempt < 400; attempt++)); do
    [[ ! -s $_shell_sense_bash_output_mailbox ]] || _shell_sense_bash_drain
    ((condition)) && return 0
    IFS= read -r -t 0.01 -u "$_shell_sense_bash_wait_fd" _ || true
  done
  return 1
}

_shell_sense_bash_capture() {
  local request=$1 generation=$2 request_buffer=$3 request_cursor=$4
  _shell_sense_bash_byte_length "${_shell_sense_bash_active_buffer:0:_shell_sense_bash_active_point}"
  local active_cursor=$_shell_sense_bash_byte_count
  if [[ $request != "$_shell_sense_bash_active_request" ||
        $generation != "$_shell_sense_bash_active_generation" ||
        $request_buffer != "$_shell_sense_bash_active_buffer" ||
        $request_cursor != "$active_cursor" ]]; then
    _shell_sense_bash_send shell-capture-begin "$request" "$generation"
    _shell_sense_bash_send capture-end "$request" "$generation"
    return
  fi
  _shell_sense_bash_collect "$_shell_sense_bash_active_buffer" "$_shell_sense_bash_active_point" "$_shell_sense_bash_fuzzy_min_chars"
  local prefix=${_shell_sense_bash_active_buffer:0:_shell_sense_bash_replace_char_start}
  local token=${_shell_sense_bash_active_buffer:_shell_sense_bash_replace_char_start:_shell_sense_bash_replace_char_end-_shell_sense_bash_replace_char_start}
  local replace_start replace_end
  _shell_sense_bash_byte_length "$prefix"
  replace_start=$_shell_sense_bash_byte_count
  _shell_sense_bash_byte_length "$token"
  replace_end=$((replace_start + _shell_sense_bash_byte_count))
  _shell_sense_bash_encode_message shell-capture-begin "$request" "$generation"
  local batch=$_shell_sense_bash_encoded
  local -i context_count=${#_shell_sense_bash_words[@]}
  _shell_sense_bash_encode_message context-begin "$request" "$generation" \
    "$_shell_sense_bash_cword" "$context_count"
  batch+=$_shell_sense_bash_encoded
  local -i context_first=0 context_last context_chunk_count
  while ((context_first < context_count)); do
    context_last=$((context_first + 60))
    ((context_last <= context_count)) || context_last=$context_count
    context_chunk_count=$((context_last - context_first))
    _shell_sense_bash_encode_message context-chunk "$request" "$generation" \
      "$context_first" "$context_chunk_count" \
      "${_shell_sense_bash_words[@]:context_first:context_chunk_count}"
    batch+=$_shell_sense_bash_encoded
    context_first=$context_last
  done
  _shell_sense_bash_encode_message context-end "$request" "$generation"
  batch+=$_shell_sense_bash_encoded
  local -i index batch_count=0
  for ((index = 0; index < ${#_shell_sense_bash_candidates[@]}; index++)); do
    _shell_sense_bash_encode_message shell-candidate \
      "$request" "$generation" \
      "${_shell_sense_bash_insertions[index]}" "${_shell_sense_bash_candidates[index]}" \
      "${_shell_sense_bash_descriptions[index]}" '' \
      "$replace_start" "$replace_end" "${_shell_sense_bash_kinds[index]}" \
      "$index" "${_shell_sense_bash_append_spaces[index]}" 0 "${_shell_sense_bash_acceptance_identities[index]}" \
      "${_shell_sense_bash_resource_paths[index]}"
    batch+=$_shell_sense_bash_encoded
    ((batch_count += 1))
    if ((batch_count >= 64)); then
      printf '%s' "$batch" >"$_shell_sense_bash_input_fifo"
      batch=
      batch_count=0
    fi
  done
  _shell_sense_bash_encode_message capture-end "$request" "$generation"
  batch+=$_shell_sense_bash_encoded
  printf '%s' "$batch" >"$_shell_sense_bash_input_fifo"
}

_shell_sense_bash_request_completion() {
  local trigger=$1
  [[ $_shell_sense_bash_activation_mode != disabled ]] || return
  ((_shell_sense_bash_request += 1))
  ((_shell_sense_bash_generation += 1))
  _shell_sense_bash_active_request=$_shell_sense_bash_request
  _shell_sense_bash_active_generation=$_shell_sense_bash_generation
  _shell_sense_bash_navigation_serial=0
  _shell_sense_bash_active_buffer=$READLINE_LINE
  _shell_sense_bash_active_point=$READLINE_POINT
  _shell_sense_bash_view_building=0
  local cursor
  _shell_sense_bash_byte_length "${READLINE_LINE:0:READLINE_POINT}"
  cursor=$_shell_sense_bash_byte_count
  _shell_sense_bash_view_ready=0
  _shell_sense_bash_send complete \
    "$_shell_sense_bash_active_request" "$_shell_sense_bash_active_generation" '' \
    "$READLINE_LINE" "$cursor" "$PWD" emacs "${COLUMNS:-80}" "${LINES:-24}" \
    "$trigger" 0
  _shell_sense_bash_wait_for '_shell_sense_bash_view_ready == 1' || _shell_sense_bash_clear_popup
}

_shell_sense_bash_after_edit() {
  local event=$1 character=${2-} trigger=automatic
  case $_shell_sense_bash_activation_mode in
    continuous) ;;
    hybrid)
      [[ " ${_shell_sense_bash_activation_events[*]} " == *" $event "* || " ${_shell_sense_bash_activation_characters[*]} " == *" $character "* ]] || return
      ;;
    *) return ;;
  esac
  [[ -z $character || " ${_shell_sense_bash_immediate_characters[*]} " != *" $character "* ]] || trigger="trigger-character"
  _shell_sense_bash_request_completion "$trigger"
}

_shell_sense_bash_insert() {
  local character=$1 before=${READLINE_LINE:0:READLINE_POINT} after=${READLINE_LINE:READLINE_POINT}
  _shell_sense_bash_clear_popup
  READLINE_LINE=$before$character$after
  READLINE_POINT=$((${#before} + ${#character}))
  _shell_sense_bash_after_edit insert "$character"
}

_shell_sense_bash_backspace() {
  _shell_sense_bash_clear_popup
  if ((READLINE_POINT > 0)); then
    READLINE_LINE=${READLINE_LINE:0:READLINE_POINT-1}${READLINE_LINE:READLINE_POINT}
    ((READLINE_POINT -= 1))
  fi
  _shell_sense_bash_after_edit backspace
}

_shell_sense_bash_apply_acceptance() {
  local request=$1 generation=$2 item_id=$3 insertion=$4 append_space=$7 identity=$8
  if [[ $request != "$_shell_sense_bash_active_request" ||
        $generation != "$_shell_sense_bash_active_generation" ]] ||
    ! _shell_sense_bash_editor_matches_active; then
    _shell_sense_bash_send selection-finished "$request" "$generation" "$item_id" 0
    return
  fi
  local range start end
  range=${identity%:*}
  start=${range%%:*}
  end=${range#*:}
  local before=${READLINE_LINE:0:start} after=${READLINE_LINE:end}
  [[ $append_space != 1 ]] || insertion+=' '
  _shell_sense_bash_clear_popup
  _shell_sense_bash_install_keymap closed
  READLINE_LINE=$before$insertion$after
  READLINE_POINT=$((${#before} + ${#insertion}))
  _shell_sense_bash_send selection-finished "$request" "$generation" "$item_id" 1
  ((_shell_sense_bash_after_accept)) && _shell_sense_bash_pending_after_accept=1
}

_shell_sense_bash_editor_matches_active() {
  [[ $READLINE_LINE == "$_shell_sense_bash_active_buffer" &&
     $READLINE_POINT == "$_shell_sense_bash_active_point" ]]
}

_shell_sense_bash_binding_action() {
  local state=$1 key=$2
  _shell_sense_bash_resolved_action=
  local -i index
  for ((index = 0; index < ${#_shell_sense_bash_binding_keys[@]}; index++)); do
    if [[ ${_shell_sense_bash_binding_states[index]} == "$state" && ${_shell_sense_bash_binding_keys[index]} == "$key" ]]; then
      _shell_sense_bash_resolved_action=${_shell_sense_bash_binding_actions[index]}
      return
    fi
  done
}

_shell_sense_bash_key_sequence() {
  case $1 in
    tab) printf '\\C-i' ;;
    ctrl-space) printf '\\C-@' ;;
    ctrl-c) printf '\\C-c' ;;
    enter) printf '\\C-m' ;;
    ctrl-e) printf '\\C-e' ;;
    ctrl-n) printf '\\C-n' ;;
    ctrl-p) printf '\\C-p' ;;
    ctrl-d) printf '\\C-d' ;;
    ctrl-u) printf '\\C-u' ;;
    ctrl-f) printf '\\C-f' ;;
    ctrl-b) printf '\\C-b' ;;
    ctrl-g) printf '\\C-g' ;;
    escape) printf '\\e' ;;
    right) printf '\\e[C' ;;
    end) printf '\\e[F' ;;
    backspace) printf '\\C-?' ;;
    *) return 1 ;;
  esac
}

_shell_sense_bash_install_keymap() {
  local state=$1 key sequence command
  [[ $_shell_sense_bash_installed_keymap != "$state" ]] || return
  local -a keys=()
  for key in "${_shell_sense_bash_binding_keys[@]}"; do
    [[ " ${keys[*]} " == *" $key "* ]] || keys+=("$key")
  done
  for key in "${keys[@]}"; do
    sequence=$(_shell_sense_bash_key_sequence "$key") || continue
    _shell_sense_bash_binding_action "$state" "$key"
    if [[ $_shell_sense_bash_resolved_action =~ ^(execute|interrupt|pass-through)$ ]]; then
      _shell_sense_bash_restore_binding "$key"
    elif [[ -n $_shell_sense_bash_resolved_action && $_shell_sense_bash_resolved_action != none ]]; then
      printf -v command '"%s":_shell_sense_bash_key %q' "$sequence" "$key"
      bind -x "$command"
    elif [[ $_shell_sense_bash_resolved_action == none ]]; then
      bind -r "$sequence" 2>/dev/null || true
    else
      _shell_sense_bash_restore_binding "$key"
    fi
  done
  if [[ $state == closed ]]; then
    _shell_sense_bash_popup_visible=0
  fi
  _shell_sense_bash_installed_keymap=$state
}

_shell_sense_bash_key() {
  if ((_shell_sense_bash_popup_visible)) && ! _shell_sense_bash_editor_matches_active; then
    _shell_sense_bash_clear_popup
    _shell_sense_bash_request_completion automatic
  fi
  local key=$1 state=closed
  ((_shell_sense_bash_popup_visible)) && state=popup
  _shell_sense_bash_binding_action "$state" "$key"
  case $_shell_sense_bash_resolved_action in
    trigger) _shell_sense_bash_request_completion manual ;;
    accept)
      local id=${_shell_sense_bash_view_ids[_shell_sense_bash_selected-1]}
      _shell_sense_bash_view_ready=0
      _shell_sense_bash_pending_after_accept=0
      _shell_sense_bash_send select "$_shell_sense_bash_active_request" "$_shell_sense_bash_active_generation" "$id"
      _shell_sense_bash_wait_for '_shell_sense_bash_popup_visible == 0' || true
      if ((_shell_sense_bash_pending_after_accept)); then
        _shell_sense_bash_pending_after_accept=0
        _shell_sense_bash_request_completion after-accept
      fi
      ;;
    next|previous|page-down|page-up|documentation-down|documentation-up|documentation-page-down|documentation-page-up|toggle-documentation)
      _shell_sense_bash_navigate "$_shell_sense_bash_resolved_action"
      ;;
    dismiss) _shell_sense_bash_clear_popup; _shell_sense_bash_install_keymap closed ;;
  esac
}

_shell_sense_bash_navigate() {
  local action=$1
  if [[ $action =~ ^(next|previous|page-down|page-up)$ ]]; then
    local -i desired=$_shell_sense_bash_selected_absolute
    case $action in
      next)
        if ((desired + 1 < _shell_sense_bash_total)); then
          ((desired += 1))
        elif ((_shell_sense_bash_cycle)); then
          desired=0
        fi
        ;;
      previous)
        if ((desired > 0)); then
          ((desired -= 1))
        elif ((_shell_sense_bash_cycle)); then
          desired=$((_shell_sense_bash_total - 1))
        fi
        ;;
      page-down)
        ((desired += _shell_sense_bash_max_rows))
        ((desired < _shell_sense_bash_total)) || desired=$((_shell_sense_bash_total - 1))
        ;;
      page-up)
        ((desired -= _shell_sense_bash_max_rows))
        ((desired >= 0)) || desired=0
        ;;
    esac
    ((_shell_sense_bash_navigation_serial += 1))
    local -i relative=$((desired - _shell_sense_bash_window_start + 1))
    _shell_sense_bash_menu_viewport_start_for "$desired" "$_shell_sense_bash_menu_view_start"
    local -i desired_view_start=$_shell_sense_bash_menu_viewport_start
    if ((relative >= 1 && relative <= ${#_shell_sense_bash_view_ids[@]})) &&
      _shell_sense_bash_cached_menu_viewport_contains "$desired_view_start"; then
      _shell_sense_bash_selected=$relative
      _shell_sense_bash_selected_absolute=$desired
      _shell_sense_bash_menu_view_start=$desired_view_start
      _shell_sense_bash_render_popup
    fi
  fi
  _shell_sense_bash_view_ready=0
  _shell_sense_bash_send navigate "$_shell_sense_bash_active_request" \
    "$_shell_sense_bash_active_generation" "$_shell_sense_bash_navigation_serial" "$action"
  _shell_sense_bash_wait_for '_shell_sense_bash_view_ready == 1' || true
}

_shell_sense_bash_close_popup() {
  _shell_sense_bash_clear_popup
  _shell_sense_bash_install_keymap closed
}

_shell_sense_bash_before_prompt() {
  if ((_shell_sense_bash_popup_lines > 0)); then
    printf '\e[?2026h\e7'
    local -i row
    for ((row = 0; row < _shell_sense_bash_popup_lines; row++)); do
      printf '\r\e[2K'
      ((row + 1 == _shell_sense_bash_popup_lines)) || printf '\e[B'
    done
    printf '\e8\e[?2026l'
  fi
  _shell_sense_bash_popup_lines=0
  _shell_sense_bash_popup_visible=0
  _shell_sense_bash_install_keymap closed
  _shell_sense_bash_start
}

_shell_sense_bash_install_prompt_hook() {
  local declaration
  local -a existing_commands=()
  declaration=$(declare -p PROMPT_COMMAND 2>/dev/null || true)
  if [[ $declaration == 'declare -a '* ]]; then
    existing_commands=("${PROMPT_COMMAND[@]}")
  elif [[ -n ${PROMPT_COMMAND-} ]]; then
    existing_commands=("$PROMPT_COMMAND")
  fi
  local -a retained_commands=()
  local command
  for command in "${existing_commands[@]}"; do
    [[ $command == _shell_sense_bash_before_prompt ]] || retained_commands+=("$command")
  done
  unset PROMPT_COMMAND
  declare -ga PROMPT_COMMAND=(_shell_sense_bash_before_prompt "${retained_commands[@]}")
}

_shell_sense_bash_capture_bindings() {
  ((_shell_sense_bash_bindings_captured == 0)) || return
  local functions macros shell_bindings key sequence line function_name
  functions=$(bind -P)
  macros=$(bind -S)
  shell_bindings=$(bind -X)
  local -a keys=()
  for key in "${_shell_sense_bash_binding_keys[@]}"; do
    [[ " ${keys[*]} " == *" $key "* ]] || keys+=("$key")
  done
  for key in "${keys[@]}"; do
    sequence=$(_shell_sense_bash_key_sequence "$key") || continue
    while IFS= read -r line; do
      if [[ $line == *"\"$sequence\""* ]]; then
        _shell_sense_bash_original_binding_types[$key]=shell
        _shell_sense_bash_original_binding_values[$key]=$line
        break
      fi
    done <<<"$shell_bindings"
    [[ -z ${_shell_sense_bash_original_binding_types[$key]-} ]] || continue
    while IFS= read -r line; do
      if [[ $line == *"\"$sequence\""* ]]; then
        _shell_sense_bash_original_binding_types[$key]=macro
        _shell_sense_bash_original_binding_values[$key]=$line
        break
      fi
    done <<<"$macros"
    [[ -z ${_shell_sense_bash_original_binding_types[$key]-} ]] || continue
    while IFS= read -r line; do
      if [[ $line == *"\"$sequence\""* ]]; then
        function_name=${line%% *}
        _shell_sense_bash_original_binding_types[$key]=function
        _shell_sense_bash_original_binding_values[$key]=$function_name
        break
      fi
    done <<<"$functions"
  done
  _shell_sense_bash_bindings_captured=1
}

_shell_sense_bash_restore_binding() {
  local key=$1 sequence value
  sequence=$(_shell_sense_bash_key_sequence "$key") || return
  value=${_shell_sense_bash_original_binding_values[$key]-}
  case ${_shell_sense_bash_original_binding_types[$key]-} in
    shell) bind -x "$value" ;;
    macro) bind "$value" ;;
    function) bind "\"$sequence\":$value" ;;
    *) bind -r "$sequence" 2>/dev/null || true ;;
  esac
}

_shell_sense_bash_bind_printable() {
  local -i code
  local character escaped shell_character binding octal
  for ((code = 32; code <= 126; code++)); do
    printf -v octal '%03o' "$code"
    printf -v character '%b' "\\$octal"
    escaped=${character//\\/\\\\}
    escaped=${escaped//\"/\\\"}
    printf -v shell_character '%q' "$character"
    binding="\"$escaped\":_shell_sense_bash_insert $shell_character"
    bind -x "$binding"
  done
  _shell_sense_bash_bind_backspace
}

_shell_sense_bash_bind_backspace() {
  local keymap backspace
  for keymap in emacs-standard vi-insert; do
    for backspace in '"\C-h"' '"\C-?"'; do
      bind -m "$keymap" -x "$backspace:_shell_sense_bash_backspace"
    done
  done
}

_shell_sense_bash_start() {
  [[ $- == *i* ]] || return
  _shell_sense_bash_install_prompt_hook
  if ((_shell_sense_bash_worker_pid > 0)); then
    if kill -0 "$_shell_sense_bash_worker_pid" 2>/dev/null &&
        ((_shell_sense_bash_ready == 1 && _shell_sense_bash_configured == 1)); then
      return
    fi
    _shell_sense_bash_reset_transport
  fi
  local root
  root=$(cd -- "$_shell_sense_bash_plugin_dir/../.." && pwd)
  local command_path=
  if [[ -n ${SHELL_SENSE_COMMAND-} ]]; then
    command_path=$SHELL_SENSE_COMMAND
  elif command -v shell-sense >/dev/null; then
    command_path=$(command -v shell-sense)
  elif [[ -x $root/target/release/shell-sense ]]; then
    command_path=$root/target/release/shell-sense
  elif [[ -x $root/target/debug/shell-sense ]]; then
    command_path=$root/target/debug/shell-sense
  else
    return 1
  fi

  local runtime_base=${XDG_RUNTIME_DIR:-/tmp/shell-sense-$UID}
  local runtime_dir=$runtime_base/shell-sense token=$BASHPID-$RANDOM-$RANDOM
  mkdir -p -- "$runtime_dir" || return
  chmod 700 -- "$runtime_dir" || return
  _shell_sense_bash_input_fifo=$runtime_dir/bash-$token.in
  _shell_sense_bash_output_mailbox=$runtime_dir/bash-$token.out
  local wait_fifo=$runtime_dir/bash-$token.wait
  mkfifo -m 600 -- "$_shell_sense_bash_input_fifo" || return
  mkfifo -m 600 -- "$wait_fifo" || return
  exec {_shell_sense_bash_wait_fd}<>"$wait_fifo"
  unlink -- "$wait_fifo"
  : >"$_shell_sense_bash_output_mailbox"
  chmod 600 -- "$_shell_sense_bash_output_mailbox"

  local state_base=${XDG_STATE_HOME:-$HOME/.local/state}
  mkdir -p -- "$state_base/shell-sense"
  chmod 700 -- "$state_base/shell-sense" || return
  _shell_sense_bash_log=$state_base/shell-sense/worker-$BASHPID.log
  local -a arguments=(worker --shell bash --shell-executable "$BASH" --shell-version "$BASH_VERSION"
    --shell-input-fifo "$_shell_sense_bash_input_fifo"
    --shell-output-mailbox "$_shell_sense_bash_output_mailbox"
    --shell-process-id "$BASHPID")
  [[ -z ${SHELL_SENSE_SOCKET-} ]] || arguments+=(--socket "$SHELL_SENSE_SOCKET")
  [[ -z ${SHELL_SENSE_CONFIG-} ]] || arguments+=(--config "$SHELL_SENSE_CONFIG")
  [[ -z ${SHELL_SENSE_PROFILE-} ]] || arguments+=(--profile "$SHELL_SENSE_PROFILE")
  # Bash cannot safely run the array-heavy mailbox decoder from a signal trap
  # while Readline owns the stack. The signal only interrupts Readline; the
  # next Shell Sense action drains and acknowledges the mailbox synchronously.
  trap ':' USR1
  "$command_path" "${arguments[@]}" </dev/null >>"$_shell_sense_bash_log" 2>&1 &
  _shell_sense_bash_worker_pid=$!
  _shell_sense_bash_wait_for '_shell_sense_bash_ready == 1 && _shell_sense_bash_configured == 1' || return 1
  _shell_sense_bash_capture_bindings
  _shell_sense_bash_bind_printable
  _shell_sense_bash_install_keymap closed
}

_shell_sense_bash_reset_transport() {
  _shell_sense_bash_clear_popup
  ((_shell_sense_bash_worker_pid <= 0)) || kill "$_shell_sense_bash_worker_pid" 2>/dev/null || true
  _shell_sense_bash_worker_pid=0
  _shell_sense_bash_ready=0
  _shell_sense_bash_configured=0
  [[ -z ${_shell_sense_bash_input_fifo-} ]] || unlink -- "$_shell_sense_bash_input_fifo" 2>/dev/null || true
  [[ -z ${_shell_sense_bash_output_mailbox-} ]] || unlink -- "$_shell_sense_bash_output_mailbox" 2>/dev/null || true
  _shell_sense_bash_input_fifo=
  _shell_sense_bash_output_mailbox=
  if ((_shell_sense_bash_wait_fd >= 0)); then
    exec {_shell_sense_bash_wait_fd}>&-
    _shell_sense_bash_wait_fd=-1
  fi
}

_shell_sense_bash_cleanup() {
  _shell_sense_bash_clear_popup
  ((_shell_sense_bash_worker_pid <= 0)) || {
    _shell_sense_bash_send goodbye || true
    kill "$_shell_sense_bash_worker_pid" 2>/dev/null || true
  }
  [[ -z ${_shell_sense_bash_input_fifo-} ]] || unlink -- "$_shell_sense_bash_input_fifo" 2>/dev/null || true
  [[ -z ${_shell_sense_bash_output_mailbox-} ]] || unlink -- "$_shell_sense_bash_output_mailbox" 2>/dev/null || true
  if ((_shell_sense_bash_wait_fd >= 0)); then
    exec {_shell_sense_bash_wait_fd}>&-
    _shell_sense_bash_wait_fd=-1
  fi
}
