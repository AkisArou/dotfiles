# Live ZLE client for zsh-sense.

typeset -gi _zsh_sense_read_fd=-1
typeset -gi _zsh_sense_write_fd=-1
typeset -gi _zsh_sense_worker_pid=0
typeset -gi _zsh_sense_ready=0
typeset -gi _zsh_sense_configured=0
typeset -gi _zsh_sense_request_serial=0
typeset -gi _zsh_sense_generation=0
typeset -gi _zsh_sense_active_request=0
typeset -gi _zsh_sense_active_generation=0
typeset -gi _zsh_sense_active_cursor_byte=0
typeset -gi _zsh_sense_popup_visible=0
typeset -gi _zsh_sense_popup_stale=0
typeset -gi _zsh_sense_render_dirty=1
typeset -gi _zsh_sense_render_columns=0
typeset -gi _zsh_sense_indicator_cells=0
typeset -gi _zsh_sense_selected=0
typeset -gi _zsh_sense_view_revision=0
typeset -gi _zsh_sense_temp_selected=0
typeset -gi _zsh_sense_temp_expected=0
typeset -gi _zsh_sense_temp_received=0
typeset -gi _zsh_sense_temp_total=0
typeset -gi _zsh_sense_temp_window_start=0
typeset -gi _zsh_sense_temp_selected_absolute=0
typeset -gi _zsh_sense_temp_max_label_cells=0
typeset -gi _zsh_sense_temp_max_described_cells=0
typeset -gi _zsh_sense_view_total=0
typeset -gi _zsh_sense_view_window_start=0
typeset -gi _zsh_sense_selected_absolute=0
typeset -gi _zsh_sense_max_label_cells=0
typeset -gi _zsh_sense_max_described_cells=0
typeset -gi _zsh_sense_last_apply_status=0
typeset -gi _zsh_sense_parse_offset=1
typeset -g _zsh_sense_rx_buffer=
typeset -g _zsh_sense_parse_value=
typeset -g _zsh_sense_active_buffer=
typeset -g _zsh_sense_last_buffer=
typeset -g _zsh_sense_owned_postdisplay=
typeset -gi _zsh_sense_last_cursor=-1
typeset -g _zsh_sense_activation_mode=continuous
typeset -gi _zsh_sense_after_accept=1
typeset -gi _zsh_sense_popup_enabled=1
typeset -gi _zsh_sense_max_rows=10
typeset -gi _zsh_sense_max_width=140
typeset -gi _zsh_sense_min_width=24
typeset -gi _zsh_sense_padding=1
typeset -g _zsh_sense_decorations=full
typeset -g _zsh_sense_border=none
typeset -gi _zsh_sense_show_title=0
typeset -gi _zsh_sense_show_footer=1
typeset -gi _zsh_sense_show_scrollbar=1
typeset -g _zsh_sense_scrollbar_character='▐'
typeset -gi _zsh_sense_show_groups=1
typeset -gi _zsh_sense_show_descriptions=1
typeset -g _zsh_sense_indicator_mode=icon
typeset -g _zsh_sense_selected_marker=
typeset -g _zsh_sense_style_menu_raw='fg=#bbbbbb,bg=#202020'
typeset -g _zsh_sense_style_border_raw='fg=#d4d4d4'
typeset -g _zsh_sense_style_selected_raw='bg=#343b41'
typeset -g _zsh_sense_style_label_raw='fg=#d4d4d4'
typeset -g _zsh_sense_style_label_match_raw='fg=#18a2fe,bold'
typeset -g _zsh_sense_style_detail_raw='fg=#bbbbbb'
typeset -g _zsh_sense_style_kind_raw='fg=#bbbbbb'
typeset -g _zsh_sense_style_group_raw='fg=#4ec9b0'
typeset -g _zsh_sense_style_footer_raw='fg=#bbbbbb'
typeset -g _zsh_sense_style_scrollbar_thumb_raw='fg=#bbbbbb'
typeset -g _zsh_sense_style_scrollbar_gutter_raw='fg=#343b41'
typeset -g _zsh_sense_style_diagnostic_error_raw='fg=#f14c4c,underline'
typeset -g _zsh_sense_style_diagnostic_warning_raw='fg=#cca700,underline'
typeset -g _zsh_sense_style_ghost_raw='fg=#707070'
typeset -g _zsh_sense_style_menu=
typeset -g _zsh_sense_style_border=
typeset -g _zsh_sense_style_selected=
typeset -g _zsh_sense_style_label=
typeset -g _zsh_sense_style_label_selected=
typeset -g _zsh_sense_style_label_match=
typeset -g _zsh_sense_style_label_match_selected=
typeset -g _zsh_sense_style_detail=
typeset -g _zsh_sense_style_detail_selected=
typeset -g _zsh_sense_style_kind=
typeset -g _zsh_sense_style_kind_selected=
typeset -g _zsh_sense_style_footer=
typeset -g _zsh_sense_style_scrollbar_thumb=
typeset -g _zsh_sense_style_scrollbar_gutter=
typeset -gA _zsh_sense_style_kinds_raw=(
  text 'fg=#bbbbbb'
  command 'fg=#c586c0'
  alias 'fg=#c586c0'
  builtin 'fg=#c586c0'
  function 'fg=#c586c0'
  subcommand 'fg=#c586c0'
  option 'fg=#ffd602'
  option-value 'fg=#9cdcfe'
  variable 'fg=#9cdcfe'
  file 'fg=#d4d4d4'
  directory 'fg=#569cd6'
  symlink 'fg=#d4d4d4'
  snippet 'fg=#ffd602'
  action 'fg=#ffd602'
)
typeset -gA _zsh_sense_style_kinds=()
typeset -gA _zsh_sense_style_kinds_selected=()
typeset -gi _zsh_sense_capture_fuzzy_min_chars=3
typeset -ga _zsh_sense_trigger_characters=( / - = : ' ' )
typeset -ga _zsh_sense_immediate_characters=( / - = )
typeset -ga _zsh_sense_events=( insert backspace delete word-delete paste history cursor accept )
typeset -gA _zsh_sense_bindings_closed=()
typeset -gA _zsh_sense_bindings_popup=()
typeset -gA _zsh_sense_bindings_snippet=()
typeset -gA _zsh_sense_key_sequences=(
  tab '^I'
  ctrl-space '^@'
  ctrl-e '^E'
  enter '^M'
  ctrl-n '^N'
  ctrl-p '^P'
  ctrl-d '^D'
  ctrl-u '^U'
  escape '^['
  right '^[[C'
  end '^[[F'
  shift-tab '^[[Z'
)
typeset -gA _zsh_sense_original_widgets=()
typeset -gA _zsh_sense_original_names=()
typeset -gA _zsh_sense_bound_sequences=()
typeset -gA _zsh_sense_widget_keys=()
typeset -ga _zsh_sense_item_ids=()
typeset -ga _zsh_sense_item_labels=()
typeset -ga _zsh_sense_item_label_cells=()
typeset -ga _zsh_sense_item_details=()
typeset -ga _zsh_sense_item_detail_cells=()
typeset -ga _zsh_sense_item_kinds=()
typeset -ga _zsh_sense_item_match_ranges=()
typeset -ga _zsh_sense_item_groups=()
typeset -ga _zsh_sense_item_insertions=()
typeset -ga _zsh_sense_item_acceptance_backends=()
typeset -ga _zsh_sense_item_acceptance_identities=()
typeset -ga _zsh_sense_render_lines=()
typeset -ga _zsh_sense_render_highlight_starts=()
typeset -ga _zsh_sense_render_highlight_ends=()
typeset -ga _zsh_sense_render_highlight_styles=()
typeset -ga _zsh_sense_temp_ids=()
typeset -ga _zsh_sense_temp_labels=()
typeset -ga _zsh_sense_temp_label_cells=()
typeset -ga _zsh_sense_temp_details=()
typeset -ga _zsh_sense_temp_detail_cells=()
typeset -ga _zsh_sense_temp_kinds=()
typeset -ga _zsh_sense_temp_match_ranges=()
typeset -ga _zsh_sense_temp_groups=()
typeset -ga _zsh_sense_temp_insertions=()
typeset -ga _zsh_sense_temp_acceptance_backends=()
typeset -ga _zsh_sense_temp_acceptance_identities=()
typeset -g _zsh_sense_fifo_in=
typeset -g _zsh_sense_fifo_out=
typeset -g _zsh_sense_log_file=
typeset -g _zsh_sense_ui_locale=${LC_ALL:-${LC_CTYPE:-${LANG:-C.UTF-8}}}

_zsh_sense_byte_length() {
  emulate -L zsh
  local LC_ALL=C
  REPLY=${#1}
}

_zsh_sense_cursor_byte() {
  emulate -L zsh
  local left=
  (( CURSOR > 0 )) && left=$BUFFER[1,CURSOR]
  _zsh_sense_byte_length "$left"
}

_zsh_sense_netstring() {
  emulate -L zsh
  local LC_ALL=C value=$1
  REPLY="${#value}:$value,"
}

_zsh_sense_encode_message() {
  emulate -L zsh
  setopt localoptions no_aliases

  local command=$1 field data=
  shift
  _zsh_sense_netstring "$command"
  data=$REPLY
  _zsh_sense_netstring "$#"
  data+=$REPLY
  for field in "$@"; do
    _zsh_sense_netstring "$field"
    data+=$REPLY
  done
  REPLY=$data
}

_zsh_sense_write_messages() {
  emulate -L zsh
  setopt localoptions no_aliases
  (( _zsh_sense_write_fd >= 0 )) || return 1

  local data=$1 chunk=

  local LC_ALL=C
  local -i offset=1 written=0 write_status=0
  local errno_name=
  while (( offset <= $#data )); do
    chunk=$data[offset,-1]
    syswrite -c written -o $_zsh_sense_write_fd "$chunk" 2>/dev/null
    write_status=$?
    if (( written > 0 )); then
      (( offset += written ))
      continue
    fi
    if (( write_status == 2 )); then
      errno_name=${errnos[${ERRNO:-0}]-}
      if [[ $errno_name == (EAGAIN|EWOULDBLOCK) ]]; then
        zselect -w $_zsh_sense_write_fd -t 5 >/dev/null 2>&1
        continue
      fi
    fi
    _zsh_sense_disconnect
    return 1
  done
}

_zsh_sense_send() {
  emulate -L zsh
  setopt localoptions no_aliases
  _zsh_sense_encode_message "$@" || return 1
  local data=$REPLY
  _zsh_sense_write_messages "$data"
}

_zsh_sense_take_netstring() {
  emulate -L zsh
  setopt localoptions no_aliases
  local LC_ALL=C
  local -i total=$#_zsh_sense_rx_buffer start=$_zsh_sense_parse_offset
  (( start <= total )) || return 1
  local tail=$_zsh_sense_rx_buffer[start,-1]
  local -i relative_colon=${tail[(i):]}
  (( relative_colon <= $#tail )) || return 1
  (( relative_colon > 1 && relative_colon <= 21 )) || return 2
  local length_text=$tail[1,relative_colon-1]
  [[ $length_text == <-> && ( $length_text == 0 || $length_text != 0* ) ]] || return 2
  local -i payload_length=$(( 10#$length_text ))
  local -i colon=$(( start + relative_colon - 1 ))
  local -i payload_start=$(( colon + 1 ))
  local -i payload_end=$(( payload_start + payload_length - 1 ))
  local -i comma=$(( payload_end + 1 ))
  (( comma <= total )) || return 1
  [[ $_zsh_sense_rx_buffer[comma] == ',' ]] || return 2
  if (( payload_length )); then
    _zsh_sense_parse_value=$_zsh_sense_rx_buffer[payload_start,payload_end]
  else
    _zsh_sense_parse_value=
  fi
  _zsh_sense_parse_offset=$(( comma + 1 ))
}

_zsh_sense_parse_messages() {
  emulate -L zsh
  setopt localoptions no_aliases
  local LC_ALL=C
  local command count_text
  local -a fields
  local -i parse_status count index consumed

  while [[ -n $_zsh_sense_rx_buffer ]]; do
    _zsh_sense_parse_offset=1
    _zsh_sense_take_netstring
    parse_status=$?
    (( parse_status == 1 )) && return 0
    (( parse_status == 0 )) || { _zsh_sense_disconnect; return 1; }
    command=$_zsh_sense_parse_value

    _zsh_sense_take_netstring
    parse_status=$?
    (( parse_status == 1 )) && return 0
    (( parse_status == 0 )) || { _zsh_sense_disconnect; return 1; }
    count_text=$_zsh_sense_parse_value
    [[ $count_text == <-> && ( $count_text == 0 || $count_text != 0* ) ]] || {
      _zsh_sense_disconnect
      return 1
    }
    count=$(( 10#$count_text ))
    (( count <= 128 )) || { _zsh_sense_disconnect; return 1; }
    fields=()
    for (( index = 1; index <= count; index++ )); do
      _zsh_sense_take_netstring
      parse_status=$?
      (( parse_status == 1 )) && return 0
      (( parse_status == 0 )) || { _zsh_sense_disconnect; return 1; }
      fields+=( "$_zsh_sense_parse_value" )
    done
    consumed=$_zsh_sense_parse_offset
    if (( consumed > $#_zsh_sense_rx_buffer )); then
      _zsh_sense_rx_buffer=
    else
      _zsh_sense_rx_buffer=$_zsh_sense_rx_buffer[consumed,-1]
    fi
    _zsh_sense_dispatch "$command" "${fields[@]}"
  done
}

_zsh_sense_fd_callback() {
  emulate -L zsh
  setopt localoptions no_aliases
  _zsh_sense_ui_locale=${LC_ALL:-${LC_CTYPE:-${LANG:-C.UTF-8}}}
  local -i fd=$1 count=0 read_status=0
  local chunk=
  [[ $fd == $_zsh_sense_read_fd ]] || return 0

  while true; do
    chunk=
    sysread -c count -i $fd -s 65536 -t 0 chunk 2>/dev/null
    read_status=$?
    if (( read_status == 0 )); then
      _zsh_sense_rx_buffer+=$chunk
      continue
    fi
    if (( read_status == 5 )); then
      if (( ! _zsh_sense_ready && _zsh_sense_worker_pid > 0 )) &&
          kill -0 $_zsh_sense_worker_pid 2>/dev/null; then
        return 0
      fi
      _zsh_sense_disconnect
      return 0
    fi
    break
  done
  _zsh_sense_parse_messages
  return 0
}

_zsh_sense_dispatch() {
  emulate -L zsh
  # Netstring parsing is byte-oriented and runs under LC_ALL=C. Do not leak
  # that dynamic locale into completion functions or popup construction.
  local LC_ALL=$_zsh_sense_ui_locale
  local command=$1
  shift
  local -a fields=( "$@" )
  case $command in
    ready)
      _zsh_sense_ready=1
      ;;
    config)
      _zsh_sense_apply_config "${fields[@]}"
      ;;
    keybinding)
      (( $#fields == 3 )) && case $fields[1] in
        closed) _zsh_sense_bindings_closed[$fields[2]]=$fields[3] ;;
        popup) _zsh_sense_bindings_popup[$fields[2]]=$fields[3] ;;
        snippet) _zsh_sense_bindings_snippet[$fields[2]]=$fields[3] ;;
      esac
      ;;
    style)
      (( $#fields == 2 )) && _zsh_sense_apply_style "$fields[1]" "$fields[2]"
      ;;
    popup-option)
      if (( $#fields == 2 )); then
        case $fields[1] in
          scrollbar-character) _zsh_sense_scrollbar_character=$fields[2] ;;
        esac
        _zsh_sense_render_dirty=1
      fi
      ;;
    kind-style)
      (( $#fields == 2 )) && _zsh_sense_style_kinds_raw[$fields[1]]=$fields[2]
      ;;
    config-end)
      _zsh_sense_rebuild_styles
      _zsh_sense_install_keybindings
      _zsh_sense_configured=1
      ;;
    capture-request)
      _zsh_sense_capture_request "${fields[@]}"
      ;;
    view-begin)
      _zsh_sense_view_begin "${fields[@]}"
      ;;
    view-item)
      _zsh_sense_view_item "${fields[@]}"
      ;;
    view-chunk)
      _zsh_sense_view_chunk "${fields[@]}"
      ;;
    view-end)
      _zsh_sense_view_end "${fields[@]}"
      ;;
    accept-zsh)
      _zsh_sense_accept_zsh "${fields[@]}"
      ;;
    request-cancelled)
      if [[ $fields[1] == $_zsh_sense_active_request && $fields[2] == $_zsh_sense_active_generation ]]; then
        _zsh_sense_clear_popup
      fi
      ;;
    error)
      typeset -g _zsh_sense_last_error="${fields[1]-}: ${fields[2]-}"
      ;;
  esac
}

_zsh_sense_send_command_candidates() {
  emulate -L zsh
  setopt localoptions no_aliases
  local request=$1 generation=$2
  local -i request_cursor=$3 total=$#_zsh_sense_capture_words
  # 10 uniform header fields + (55 * 2) item fields = 120 fields, below the
  # shell wire limit of 128. Rust derives presentation metadata from kind and
  # the ordinal is implicit, so neither is repeated in Zsh.
  local -i batch_size=55 first last count index prefix_bytes suffix_bytes start end
  local -a fields wire_messages=()
  local word kind

  _zsh_sense_encode_message capture-begin "$request" "$generation" portable
  wire_messages+=( "$REPLY" )
  for (( first = 1; first <= total; first += batch_size )); do
    # Input may arrive after capture started. Stop at a bounded batch boundary
    # so continuous completion never monopolizes ZLE while typed bytes wait.
    (( PENDING > 0 || KEYS_QUEUED_COUNT > 0 )) && break
    (( last = first + batch_size - 1 ))
    (( last > total )) && last=$total
    (( count = last - first + 1 ))
    _zsh_sense_byte_length "$_zsh_sense_fast_command_prefix"
    prefix_bytes=$REPLY
    _zsh_sense_byte_length "$_zsh_sense_fast_command_suffix"
    suffix_bytes=$REPLY
    (( start = request_cursor - prefix_bytes, start < 0 )) && start=0
    (( end = request_cursor + suffix_bytes ))
    fields=(
      "$request" "$generation" "$start" "$end"
      "$_zsh_sense_fast_command_prefix" "$_zsh_sense_fast_command_suffix"
      "$_zsh_sense_fast_command_iprefix" "$_zsh_sense_fast_command_isuffix"
      "$first" "$count"
    )
    for (( index = first; index <= last; index++ )); do
      word=$_zsh_sense_capture_words[index]
      kind=${_zsh_sense_capture_kinds[index]:-text}
      fields+=( "$word" "$kind" )
    done
    _zsh_sense_encode_message command-candidates "${fields[@]}" || return 1
    wire_messages+=( "$REPLY" )
    if (( $#wire_messages >= 4 )); then
      _zsh_sense_write_messages "${(j::)wire_messages}" || return 1
      wire_messages=()
    fi
  done
  _zsh_sense_encode_message capture-end "$request" "$generation"
  wire_messages+=( "$REPLY" )
  _zsh_sense_write_messages "${(j::)wire_messages}"
}

_zsh_sense_merge_styles() {
  emulate -L zsh
  local -A values=() seen=()
  local -a order=() tokens=() merged=()
  local specification token key
  for specification in "$@"; do
    tokens=( "${(@s:,:)specification}" )
    for token in "${tokens[@]}"; do
      [[ -n $token ]] || continue
      if [[ $token == none ]]; then
        values=( none none )
        seen=( none 1 )
        order=( none )
        continue
      fi
      if [[ -n ${seen[none]-} ]]; then
        values=()
        seen=()
        order=()
      fi
      case $token in
        fg=*) key=fg ;;
        bg=*) key=bg ;;
        *) key=$token ;;
      esac
      if [[ -z ${seen[$key]-} ]]; then
        order+=( "$key" )
        seen[$key]=1
      fi
      values[$key]=$token
    done
  done
  for key in "${order[@]}"; do
    merged+=( "$values[$key]" )
  done
  REPLY=${(j:,:)merged}
}

_zsh_sense_apply_style() {
  emulate -L zsh
  local name=$1 value=$2
  case $name in
    menu) _zsh_sense_style_menu_raw=$value ;;
    border) _zsh_sense_style_border_raw=$value ;;
    selected) _zsh_sense_style_selected_raw=$value ;;
    label) _zsh_sense_style_label_raw=$value ;;
    label-match) _zsh_sense_style_label_match_raw=$value ;;
    detail) _zsh_sense_style_detail_raw=$value ;;
    kind) _zsh_sense_style_kind_raw=$value ;;
    group) _zsh_sense_style_group_raw=$value ;;
    footer) _zsh_sense_style_footer_raw=$value ;;
    scrollbar-thumb) _zsh_sense_style_scrollbar_thumb_raw=$value ;;
    scrollbar-gutter) _zsh_sense_style_scrollbar_gutter_raw=$value ;;
    diagnostic-error) _zsh_sense_style_diagnostic_error_raw=$value ;;
    diagnostic-warning) _zsh_sense_style_diagnostic_warning_raw=$value ;;
    ghost) _zsh_sense_style_ghost_raw=$value ;;
  esac
  _zsh_sense_render_dirty=1
}

_zsh_sense_rebuild_styles() {
  emulate -L zsh
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw"
  _zsh_sense_style_menu=$REPLY
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_border_raw"
  _zsh_sense_style_border=$REPLY
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_selected_raw"
  _zsh_sense_style_selected=$REPLY
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_label_raw"
  _zsh_sense_style_label=$REPLY
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_label_raw" \
    "$_zsh_sense_style_selected_raw"
  _zsh_sense_style_label_selected=$REPLY
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_label_raw" \
    "$_zsh_sense_style_label_match_raw"
  _zsh_sense_style_label_match=$REPLY
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_label_raw" \
    "$_zsh_sense_style_label_match_raw" "$_zsh_sense_style_selected_raw"
  _zsh_sense_style_label_match_selected=$REPLY
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_detail_raw"
  _zsh_sense_style_detail=$REPLY
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_detail_raw" \
    "$_zsh_sense_style_selected_raw"
  _zsh_sense_style_detail_selected=$REPLY
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_kind_raw"
  _zsh_sense_style_kind=$REPLY
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_kind_raw" \
    "$_zsh_sense_style_selected_raw"
  _zsh_sense_style_kind_selected=$REPLY
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_footer_raw"
  _zsh_sense_style_footer=$REPLY
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_scrollbar_thumb_raw"
  _zsh_sense_style_scrollbar_thumb=$REPLY
  _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_scrollbar_gutter_raw"
  _zsh_sense_style_scrollbar_gutter=$REPLY

  _zsh_sense_style_kinds=()
  _zsh_sense_style_kinds_selected=()
  local kind
  for kind in ${(k)_zsh_sense_style_kinds_raw}; do
    _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_kind_raw" \
      "$_zsh_sense_style_kinds_raw[$kind]"
    _zsh_sense_style_kinds[$kind]=$REPLY
    _zsh_sense_merge_styles "$_zsh_sense_style_menu_raw" "$_zsh_sense_style_kind_raw" \
      "$_zsh_sense_style_kinds_raw[$kind]" "$_zsh_sense_style_selected_raw"
    _zsh_sense_style_kinds_selected[$kind]=$REPLY
  done
  _zsh_sense_render_dirty=1
}

_zsh_sense_apply_config() {
  emulate -L zsh
  local -a fields=( "$@" )
  (( $#fields >= 23 )) || return 1
  _zsh_sense_activation_mode=$fields[1]
  _zsh_sense_after_accept=$fields[3]
  _zsh_sense_popup_enabled=$fields[4]
  _zsh_sense_max_rows=$fields[5]
  _zsh_sense_max_width=$fields[6]
  _zsh_sense_min_width=$fields[7]
  _zsh_sense_padding=$fields[8]
  _zsh_sense_decorations=$fields[9]
  _zsh_sense_border=$fields[10]
  _zsh_sense_show_title=$fields[11]
  _zsh_sense_show_footer=$fields[12]
  _zsh_sense_show_scrollbar=$fields[13]
  _zsh_sense_show_groups=$fields[14]
  _zsh_sense_show_descriptions=$fields[15]
  _zsh_sense_style_detail_raw=$fields[16]
  _zsh_sense_indicator_mode=$fields[17]
  _zsh_sense_selected_marker=$fields[18]
  _zsh_sense_capture_matcher=$fields[19]
  _zsh_sense_capture_fuzzy_min_chars=$fields[20]
  _zsh_sense_render_dirty=1
  local -i offset=21 count=$fields[21]
  (( offset++ ))
  if (( count )); then
    _zsh_sense_trigger_characters=( "${(@)fields[offset,$(( offset + count - 1 ))]}" )
  else
    _zsh_sense_trigger_characters=()
  fi
  (( offset += count ))
  (( offset <= $#fields )) || return 1
  count=$fields[offset]
  (( offset++ ))
  if (( count )); then
    _zsh_sense_immediate_characters=( "${(@)fields[offset,$(( offset + count - 1 ))]}" )
  else
    _zsh_sense_immediate_characters=()
  fi
  (( offset += count ))
  (( offset <= $#fields )) || return 1
  count=$fields[offset]
  (( offset++ ))
  if (( count )); then
    _zsh_sense_events=( "${(@)fields[offset,$(( offset + count - 1 ))]}" )
  else
    _zsh_sense_events=()
  fi
}

_zsh_sense_capture_request() {
  emulate -L zsh
  setopt localoptions no_aliases
  (( $# >= 4 )) || return 1
  local request=$1 generation=$2 request_buffer=$3
  local -i request_cursor=$4
  _zsh_sense_cursor_byte
  if [[ $request != $_zsh_sense_active_request ||
        $generation != $_zsh_sense_active_generation ||
        $request_buffer != "$BUFFER" || $request_cursor != $REPLY ]]; then
    _zsh_sense_send capture-begin "$request" "$generation" portable
    _zsh_sense_send capture-end "$request" "$generation"
    return 0
  fi

  # A capture widget runs synchronously in the portable backend. Never start
  # it while ZLE already has user input waiting; the next edit will issue a
  # newer generation after those bytes have been processed.
  if (( PENDING > 0 || KEYS_QUEUED_COUNT > 0 )); then
    _zsh_sense_send capture-begin "$request" "$generation" portable
    _zsh_sense_send capture-end "$request" "$generation"
    return 0
  fi

  local original_buffer=$BUFFER
  local -i original_cursor=$CURSOR
  _zsh_sense_fast_command_handled=0
  zle .zsh-sense-fast-command-capture || true
  if (( ! _zsh_sense_fast_command_handled )); then
    BUFFER=$original_buffer
    CURSOR=$original_cursor
    zle .zsh-sense-portable-capture
  fi
  BUFFER=$original_buffer
  CURSOR=$original_cursor

  # The completion function itself is synchronous in portable mode, but the
  # expensive serialization phase is cancellable. If another key arrived
  # during generation, return an empty/stale capture immediately; processing
  # that key will issue the authoritative next generation.
  if (( PENDING > 0 || KEYS_QUEUED_COUNT > 0 )); then
    _zsh_sense_send capture-begin "$request" "$generation" portable
    _zsh_sense_send capture-end "$request" "$generation"
    return 0
  fi

  if (( _zsh_sense_fast_command_handled )); then
    _zsh_sense_send_command_candidates "$request" "$generation" "$request_cursor"
    return
  fi

  # A command-name request can contain hundreds of candidates. Encode a
  # bounded group and write it as one stream chunk instead of blocking ZLE on
  # one syscall per candidate. The wire format remains a sequence of ordinary
  # messages, so batching is transparent to the worker and bounded in memory.
  local -a wire_messages=()
  local -i wire_batch_size=64
  _zsh_sense_encode_message capture-begin "$request" "$generation" portable
  wire_messages+=( "$REPLY" )
  local -i index prefix_bytes suffix_bytes start end flags
  local word display description group explanation kind identity
  for (( index = 1; index <= $#_zsh_sense_capture_words; index++ )); do
    (( index > 1 && index % 16 == 1 && ( PENDING > 0 || KEYS_QUEUED_COUNT > 0 ) )) && break
    word=$_zsh_sense_capture_words[index]
    display=$_zsh_sense_capture_displays[index]
    description=$_zsh_sense_capture_descriptions[index]
    group=$_zsh_sense_capture_groups[index]
    explanation=$_zsh_sense_capture_explanations[index]
    identity=$index
    flags=0
    [[ $_zsh_sense_capture_flags[index] == *f* ]] && (( flags |= 1 ))
    kind=${_zsh_sense_capture_kinds[index]:-}
    if [[ -n $kind ]]; then
      :
    elif [[ $word == */ ]]; then
      kind=directory
      (( flags |= 2 ))
    elif [[ $word == -* ]]; then
      kind=option
    elif (( flags & 1 )) || [[ $_zsh_sense_capture_prefixes[index] == */* ]]; then
      kind=file
    else
      kind=text
    fi
    _zsh_sense_byte_length "$_zsh_sense_capture_prefixes[index]"
    prefix_bytes=$REPLY
    _zsh_sense_byte_length "$_zsh_sense_capture_suffixes[index]"
    suffix_bytes=$REPLY
    (( start = request_cursor - prefix_bytes, start < 0 )) && start=0
    (( end = request_cursor + suffix_bytes ))
    _zsh_sense_encode_message candidate \
      "$request" "$generation" "$word" "$display" "$description" "$explanation" \
      "$group" "" "$_zsh_sense_capture_calls[index]" "$start" "$end" "$kind" \
      "$flags" "$identity" "$(( index - 1 ))" \
      "$_zsh_sense_capture_prefixes[index]" "$_zsh_sense_capture_suffixes[index]" \
      "$_zsh_sense_capture_iprefixes[index]" "$_zsh_sense_capture_isuffixes[index]" \
      "" "" "" "" "" "" "" 0 || return 1
    wire_messages+=( "$REPLY" )
    if (( $#wire_messages >= wire_batch_size )); then
      _zsh_sense_write_messages "${(j::)wire_messages}" || return 1
      wire_messages=()
    fi
  done
  _zsh_sense_encode_message capture-end "$request" "$generation"
  wire_messages+=( "$REPLY" )
  _zsh_sense_write_messages "${(j::)wire_messages}"
}

_zsh_sense_view_begin() {
  emulate -L zsh
  (( $# >= 15 )) || return 1
  [[ $2 == $_zsh_sense_active_request && $3 == $_zsh_sense_active_generation ]] || return 0
  _zsh_sense_view_revision=$4
  _zsh_sense_temp_selected=${5:-0}
  (( _zsh_sense_temp_selected++ ))
  [[ $9 == <-> ]] || return 1
  _zsh_sense_temp_expected=$9
  [[ ${10} == <-> && ${11} == <-> && ${12} == <-> &&
     ${13} == <-> && ${14} == <-> ]] || return 1
  _zsh_sense_temp_total=${10}
  _zsh_sense_temp_window_start=${11}
  _zsh_sense_temp_selected_absolute=${12}
  _zsh_sense_temp_max_label_cells=${13}
  _zsh_sense_temp_max_described_cells=${14}
  _zsh_sense_temp_received=0
  _zsh_sense_temp_ids=()
  _zsh_sense_temp_labels=()
  _zsh_sense_temp_label_cells=()
  _zsh_sense_temp_details=()
  _zsh_sense_temp_detail_cells=()
  _zsh_sense_temp_kinds=()
  _zsh_sense_temp_match_ranges=()
  _zsh_sense_temp_groups=()
  _zsh_sense_temp_insertions=()
  _zsh_sense_temp_acceptance_backends=()
  _zsh_sense_temp_acceptance_identities=()
  if (( _zsh_sense_temp_expected )); then
    _zsh_sense_temp_ids[_zsh_sense_temp_expected]=
    _zsh_sense_temp_labels[_zsh_sense_temp_expected]=
    _zsh_sense_temp_label_cells[_zsh_sense_temp_expected]=0
    _zsh_sense_temp_details[_zsh_sense_temp_expected]=
    _zsh_sense_temp_detail_cells[_zsh_sense_temp_expected]=0
    _zsh_sense_temp_kinds[_zsh_sense_temp_expected]=
    _zsh_sense_temp_match_ranges[_zsh_sense_temp_expected]=
    _zsh_sense_temp_groups[_zsh_sense_temp_expected]=
    _zsh_sense_temp_insertions[_zsh_sense_temp_expected]=
    _zsh_sense_temp_acceptance_backends[_zsh_sense_temp_expected]=
    _zsh_sense_temp_acceptance_identities[_zsh_sense_temp_expected]=
  fi
}

_zsh_sense_view_item() {
  emulate -L zsh
  local -a fields=( "$@" )
  (( $#fields >= 19 )) || return 1
  [[ $fields[1] == $_zsh_sense_active_request && $fields[2] == $_zsh_sense_active_generation ]] || return 0
  (( ++_zsh_sense_temp_received <= _zsh_sense_temp_expected )) || return 1
  local -i item=$_zsh_sense_temp_received
  _zsh_sense_temp_ids[item]=$fields[3]
  _zsh_sense_temp_labels[item]=$fields[5]
  _zsh_sense_temp_label_cells[item]=${#_zsh_sense_temp_labels[item]}
  _zsh_sense_temp_kinds[item]=$fields[8]
  _zsh_sense_temp_match_ranges[item]=
  _zsh_sense_temp_details[item]=$fields[10]
  _zsh_sense_temp_detail_cells[item]=${#_zsh_sense_temp_details[item]}
  _zsh_sense_temp_groups[item]=$fields[11]
  _zsh_sense_temp_insertions[item]=$fields[14]
  local -i acceptance_offset=$(( 20 + fields[19] ))
  _zsh_sense_temp_acceptance_backends[item]=${fields[acceptance_offset]-}
  _zsh_sense_temp_acceptance_identities[item]=${fields[$(( acceptance_offset + 1 ))]-}
}

_zsh_sense_view_chunk() {
  emulate -L zsh
  local -a fields=( "$@" )
  (( $#fields >= 3 )) || return 1
  [[ $fields[1] == $_zsh_sense_active_request && $fields[2] == $_zsh_sense_active_generation ]] || return 0
  [[ $fields[3] == <-> ]] || return 1
  local -i count=$fields[3]
  (( $#fields == 3 + count * 10 )) || return 1
  (( _zsh_sense_temp_received + count <= _zsh_sense_temp_expected )) || return 1

  local -i index item offset=4
  for (( index = 1; index <= count; index++, offset += 10 )); do
    (( item = ++_zsh_sense_temp_received ))
    _zsh_sense_temp_ids[item]=$fields[offset]
    _zsh_sense_temp_labels[item]=$fields[$(( offset + 1 ))]
    [[ $fields[$(( offset + 2 ))] == <-> ]] || return 1
    _zsh_sense_temp_label_cells[item]=$fields[$(( offset + 2 ))]
    _zsh_sense_temp_kinds[item]=$fields[$(( offset + 3 ))]
    _zsh_sense_temp_details[item]=$fields[$(( offset + 4 ))]
    [[ $fields[$(( offset + 5 ))] == <-> ]] || return 1
    _zsh_sense_temp_detail_cells[item]=$fields[$(( offset + 5 ))]
    _zsh_sense_temp_groups[item]=$fields[$(( offset + 6 ))]
    _zsh_sense_temp_insertions[item]=
    _zsh_sense_temp_acceptance_backends[item]=$fields[$(( offset + 7 ))]
    _zsh_sense_temp_acceptance_identities[item]=$fields[$(( offset + 8 ))]
    _zsh_sense_temp_match_ranges[item]=$fields[$(( offset + 9 ))]
  done
}

_zsh_sense_view_end() {
  emulate -L zsh
  (( $# >= 3 )) || return 1
  [[ $1 == $_zsh_sense_active_request && $2 == $_zsh_sense_active_generation ]] || return 0
  (( _zsh_sense_temp_received == _zsh_sense_temp_expected )) || return 1
  _zsh_sense_item_ids=( "${_zsh_sense_temp_ids[@]}" )
  _zsh_sense_item_labels=( "${_zsh_sense_temp_labels[@]}" )
  _zsh_sense_item_label_cells=( "${_zsh_sense_temp_label_cells[@]}" )
  _zsh_sense_item_details=( "${_zsh_sense_temp_details[@]}" )
  _zsh_sense_item_detail_cells=( "${_zsh_sense_temp_detail_cells[@]}" )
  _zsh_sense_item_kinds=( "${_zsh_sense_temp_kinds[@]}" )
  _zsh_sense_item_match_ranges=( "${_zsh_sense_temp_match_ranges[@]}" )
  _zsh_sense_item_groups=( "${_zsh_sense_temp_groups[@]}" )
  _zsh_sense_item_insertions=( "${_zsh_sense_temp_insertions[@]}" )
  _zsh_sense_item_acceptance_backends=( "${_zsh_sense_temp_acceptance_backends[@]}" )
  _zsh_sense_item_acceptance_identities=( "${_zsh_sense_temp_acceptance_identities[@]}" )
  _zsh_sense_view_total=$_zsh_sense_temp_total
  _zsh_sense_view_window_start=$_zsh_sense_temp_window_start
  _zsh_sense_selected_absolute=$_zsh_sense_temp_selected_absolute
  _zsh_sense_max_label_cells=$_zsh_sense_temp_max_label_cells
  _zsh_sense_max_described_cells=$_zsh_sense_temp_max_described_cells
  _zsh_sense_render_dirty=1
  _zsh_sense_popup_stale=0
  _zsh_sense_selected=$_zsh_sense_temp_selected
  (( _zsh_sense_selected < 1 )) && _zsh_sense_selected=1
  (( _zsh_sense_selected > $#_zsh_sense_item_ids )) && _zsh_sense_selected=$#_zsh_sense_item_ids
  if (( $#_zsh_sense_item_ids && _zsh_sense_popup_enabled )); then
    _zsh_sense_popup_visible=1
  else
    _zsh_sense_clear_popup
  fi
  _zsh_sense_render
}

_zsh_sense_accept_zsh() {
  emulate -L zsh
  (( $# >= 19 )) || return 1
  [[ $1 == $_zsh_sense_active_request && $2 == $_zsh_sense_active_generation ]] || return 0
  [[ $3 == portable ]] || return 0
  local identity=$5
  [[ $identity == <-> ]] || return 1
  _zsh_sense_apply_serial=$_zsh_sense_capture_serial
  _zsh_sense_apply_index=$identity
  zle .zsh-sense-portable-apply
  _zsh_sense_clear_popup
  _zsh_sense_last_buffer=$BUFFER
  _zsh_sense_last_cursor=$CURSOR
  if (( _zsh_sense_after_accept )); then
    _zsh_sense_request after-accept
  fi
}

_zsh_sense_request() {
  emulate -L zsh
  setopt localoptions no_aliases
  (( _zsh_sense_ready && _zsh_sense_configured )) || return 1
  [[ $_zsh_sense_activation_mode != disabled ]] || return 1
  [[ -n $BUFFER ]] || { _zsh_sense_clear_popup; return 0; }
  local trigger=${1:-automatic}
  if (( _zsh_sense_active_request )); then
    _zsh_sense_send cancel "$_zsh_sense_active_request" "$_zsh_sense_active_generation"
  fi
  (( _zsh_sense_request_serial++, _zsh_sense_generation++ ))
  _zsh_sense_active_request=$_zsh_sense_request_serial
  _zsh_sense_active_generation=$_zsh_sense_generation
  _zsh_sense_active_buffer=$BUFFER
  _zsh_sense_cursor_byte
  _zsh_sense_active_cursor_byte=$REPLY
  _zsh_sense_last_buffer=$BUFFER
  _zsh_sense_last_cursor=$CURSOR
  # Keep the last complete frame on-screen while the next generation is
  # debounced, captured, and ranked. Clearing it here made every ordinary edit
  # produce a blank frame followed by a populated frame (visible flashing).
  # Mark it stale instead: it remains useful visual continuity, but it cannot
  # be navigated or accepted against the newly edited buffer.
  (( _zsh_sense_popup_visible )) && _zsh_sense_popup_stale=1
  _zsh_sense_send complete \
    "$_zsh_sense_active_request" "$_zsh_sense_active_generation" "" "$BUFFER" \
    "$_zsh_sense_active_cursor_byte" "$PWD" "${KEYMAP:-main}" \
    "${COLUMNS:-80}" "${LINES:-24}" "$trigger" 0 || _zsh_sense_clear_popup
}

_zsh_sense_event_for_widget() {
  emulate -L zsh
  case ${LASTWIDGET:-} in
    *backward-delete*) REPLY=backspace ;;
    *kill-word*|*delete-word*) REPLY=word-delete ;;
    *delete*) REPLY=delete ;;
    *paste*) REPLY=paste ;;
    *history*|up-line-or-history|down-line-or-history) REPLY=history ;;
    *forward-char*|*backward-char*|*beginning-of-line*|*end-of-line*) REPLY=cursor ;;
    *)
      if [[ $BUFFER != $_zsh_sense_last_buffer ]]; then REPLY=insert; else REPLY=cursor; fi
      ;;
  esac
}

_zsh_sense_line_pre_redraw() {
  emulate -L zsh
  (( _zsh_sense_configured )) || return 0
  local -i changed=0
  [[ $BUFFER != $_zsh_sense_last_buffer || $CURSOR != $_zsh_sense_last_cursor ]] && changed=1
  local event=
  if (( changed )); then
    # Determine the event before rendering so LASTWIDGET still describes the
    # edit that requested this redraw.
    _zsh_sense_event_for_widget
    event=$REPLY
  fi
  if (( changed )); then
    _zsh_sense_last_buffer=$BUFFER
    _zsh_sense_last_cursor=$CURSOR
    if (( ${_zsh_sense_events[(Ie)$event]} )); then
      # Apply lifecycle policy before rendering. In manual/hybrid modes this
      # prevents one stale frame from being redrawn after an edit has already
      # invalidated it; in continuous mode `_request` deliberately retains
      # the last complete frame until its replacement arrives.
      case $_zsh_sense_activation_mode in
        continuous)
          local char=${LBUFFER[-1]-}
          if [[ -n $char ]] && (( ${_zsh_sense_immediate_characters[(Ie)$char]} )); then
            _zsh_sense_request trigger-character
          else
            _zsh_sense_request automatic
          fi
          ;;
        hybrid)
          local char=${LBUFFER[-1]-}
          if [[ -n $char ]] && (( ${_zsh_sense_trigger_characters[(Ie)$char]} )); then
            _zsh_sense_request trigger-character
          else
            _zsh_sense_clear_popup
          fi
          ;;
        manual|disabled)
          _zsh_sense_clear_popup
          ;;
      esac
    fi
  fi
  (( _zsh_sense_popup_visible )) && _zsh_sense_render
}

_zsh_sense_line_init() {
  emulate -L zsh
  # ZLE resets POSTDISPLAY for every new editing session. Forget the previous
  # ownership token before clearing plugin state so content installed by
  # another line-init hook is never mistaken for ours.
  _zsh_sense_owned_postdisplay=
  _zsh_sense_clear_popup 0
  _zsh_sense_last_buffer=$BUFFER
  _zsh_sense_last_cursor=$CURSOR
}

_zsh_sense_line_finish() {
  emulate -L zsh
  # POSTDISPLAY is part of ZLE's editable display. Remove the panel before ZLE
  # commits the accepted line to terminal scrollback. This is also a fallback
  # for custom accept-line widgets that do not use the configured Enter key.
  _zsh_sense_prepare_line_finish
}

_zsh_sense_prepare_line_finish() {
  emulate -L zsh
  if (( _zsh_sense_active_request )); then
    _zsh_sense_send cancel \
      "$_zsh_sense_active_request" "$_zsh_sense_active_generation"
  fi
  _zsh_sense_active_request=0
  _zsh_sense_active_buffer=
  _zsh_sense_clear_popup 0
}

_zsh_sense_erase_edit_display() {
  emulate -L zsh
  local -i panel_lines=$#_zsh_sense_render_lines
  (( panel_lines )) || return 0
  [[ -n ${terminfo[sc]-} && -n ${terminfo[cud]-} && -n ${terminfo[dl]-} &&
     -n ${terminfo[rc]-} ]] || return 0
  # Syntax highlighters/transient-prompt integrations redraw the accepted
  # command during line-finish, so delete their editable row together with the
  # panel. Plain ZLE preserves its existing editable row; delete only the panel
  # below it. Both paths collapse rows rather than leaving a popup-sized gap.
  echoti sc
  if (( $+functions[_zsh_highlight] || $+functions[_p9k_on_widget_zle-line-finish] )); then
    echoti dl $(( panel_lines + 1 ))
  else
    echoti cud 1
    echoti dl $panel_lines
  fi
  echoti rc
}

_zsh_sense_remove_highlights() {
  emulate -L zsh
  (( ${+region_highlight} )) || return 0
  region_highlight=( ${region_highlight:#*memo=zsh-sense} )
}

_zsh_sense_remove_postdisplay() {
  emulate -L zsh
  _zsh_sense_remove_highlights
  [[ -n $_zsh_sense_owned_postdisplay ]] || return 0
  if [[ $POSTDISPLAY == *"$_zsh_sense_owned_postdisplay" ]]; then
    local -i base_length=$(( $#POSTDISPLAY - $#_zsh_sense_owned_postdisplay ))
    if (( base_length > 0 )); then
      POSTDISPLAY=$POSTDISPLAY[1,base_length]
    else
      POSTDISPLAY=
    fi
  fi
  _zsh_sense_owned_postdisplay=
}

_zsh_sense_set_postdisplay() {
  emulate -L zsh
  local panel=$1 separator=$'\n'
  _zsh_sense_remove_postdisplay
  [[ -n $panel ]] || return 0
  [[ -n $POSTDISPLAY && $POSTDISPLAY[-1] == $'\n' ]] && separator=
  local -i highlight_base=$(( $#BUFFER + $#POSTDISPLAY + $#separator ))
  _zsh_sense_owned_postdisplay="$separator$panel"
  POSTDISPLAY+=$_zsh_sense_owned_postdisplay
  local -i index start end
  for (( index = 1; index <= $#_zsh_sense_render_highlight_starts; index++ )); do
    start=$_zsh_sense_render_highlight_starts[index]
    end=$_zsh_sense_render_highlight_ends[index]
    region_highlight+=(
      "$(( highlight_base + start )) $(( highlight_base + end )) $_zsh_sense_render_highlight_styles[index] memo=zsh-sense"
    )
  done
}

_zsh_sense_clear_popup() {
  local -i request_redisplay=${1:-1}
  _zsh_sense_remove_postdisplay
  _zsh_sense_popup_visible=0
  _zsh_sense_popup_stale=0
  _zsh_sense_selected=0
  _zsh_sense_view_total=0
  _zsh_sense_view_window_start=0
  _zsh_sense_selected_absolute=0
  _zsh_sense_max_label_cells=0
  _zsh_sense_max_described_cells=0
  _zsh_sense_item_ids=()
  _zsh_sense_item_labels=()
  _zsh_sense_item_label_cells=()
  _zsh_sense_item_details=()
  _zsh_sense_item_detail_cells=()
  _zsh_sense_item_kinds=()
  _zsh_sense_item_match_ranges=()
  _zsh_sense_item_groups=()
  _zsh_sense_item_insertions=()
  _zsh_sense_item_acceptance_backends=()
  _zsh_sense_item_acceptance_identities=()
  _zsh_sense_render_dirty=1
  _zsh_sense_render_columns=0
  _zsh_sense_render_lines=()
  _zsh_sense_render_highlight_starts=()
  _zsh_sense_render_highlight_ends=()
  _zsh_sense_render_highlight_styles=()
  if (( request_redisplay )) && zle >/dev/null 2>&1; then
    zle -R 2>/dev/null
  fi
}

_zsh_sense_kind_indicator() {
  emulate -L zsh
  local icon=
  _zsh_sense_indicator_cells=0
  if [[ $_zsh_sense_indicator_mode == none ]]; then
    REPLY=
    return
  fi
  case $1 in
    directory) icon='󰉋' ;;
    file|symlink) icon='󰈔' ;;
    option|option-value) icon='󰘵' ;;
    command|subcommand) icon='󰆍' ;;
    variable) icon='󰫧' ;;
    service) icon='󰒍' ;;
    git-branch) icon='' ;;
    snippet) icon='󰩫' ;;
  esac
  case $_zsh_sense_indicator_mode in
    icon)
      REPLY=$icon
      [[ -n $icon ]] && _zsh_sense_indicator_cells=1
      ;;
    text)
      REPLY="[${1[1]}]"
      _zsh_sense_indicator_cells=3
      ;;
    both)
      if [[ -n $icon ]]; then
        REPLY="$icon [$1]"
        _zsh_sense_indicator_cells=$(( 1 + 1 + ${#1} + 2 ))
      else
        REPLY="[$1]"
        _zsh_sense_indicator_cells=$(( ${#1} + 2 ))
      fi
      ;;
    *)
      REPLY=
      ;;
  esac
}

_zsh_sense_truncate() {
  emulate -L zsh
  local text=$1
  local -i width=$2
  if (( $#text <= width )); then
    REPLY=$text
  elif (( width <= 1 )); then
    REPLY=${text[1,width]}
  else
    REPLY="${text[1,$(( width - 1 ))]}…"
  fi
}

_zsh_sense_scrollbar_geometry() {
  emulate -L zsh
  local -i rows=$1 total=$2 selected=$3
  local -i thumb_rows=0 thumb_first=0 track=0
  if (( rows > 0 && total > rows )); then
    (( thumb_rows = (rows * rows) / total ))
    (( thumb_rows < 1 )) && thumb_rows=1
    (( thumb_rows > rows )) && thumb_rows=$rows
    (( selected < 0 )) && selected=0
    (( selected >= total )) && selected=$(( total - 1 ))
    (( track = rows - thumb_rows ))
    if (( track > 0 && total > 1 )); then
      # Map the selected item over the complete track rather than deriving the
      # thumb from the asynchronously updated viewport. This makes both
      # endpoints exact and keeps local navigation visually synchronous.
      (( thumb_first = (selected * track + (total - 1) / 2) / (total - 1) ))
    fi
  fi
  REPLY="$thumb_rows:$thumb_first"
}

_zsh_sense_render() {
  emulate -L zsh
  # Netstring parsing is byte-oriented and dynamically scopes LC_ALL=C into
  # dispatch handlers. Render in the interactive locale so ZLE receives real
  # multibyte characters instead of displaying them as `\M-...` byte escapes.
  local LC_ALL=$_zsh_sense_ui_locale
  (( _zsh_sense_popup_visible && $#_zsh_sense_item_ids )) || return 0
  if (( ! _zsh_sense_render_dirty &&
        _zsh_sense_render_columns == COLUMNS &&
        $#_zsh_sense_render_lines )); then
    _zsh_sense_set_postdisplay "${(F)_zsh_sense_render_lines}"
    zle -R
    return 0
  fi
  local tl tr bl br horizontal vertical
  case $_zsh_sense_border in
    sharp) tl=┌ tr=┐ bl=└ br=┘ horizontal=─ vertical=│ ;;
    ascii) tl=+ tr=+ bl=+ br=+ horizontal=- vertical='|' ;;
    none) tl= tr= bl= br= horizontal= vertical= ;;
    *) tl=╭ tr=╮ bl=╰ br=╯ horizontal=─ vertical=│ ;;
  esac
  local -i terminal_width=$(( COLUMNS > 0 ? COLUMNS : 80 ))
  local -i rows=$#_zsh_sense_item_ids
  (( rows > _zsh_sense_max_rows )) && rows=$_zsh_sense_max_rows
  local -i scrollbar_active=0 scrollbar_cells=0
  if (( _zsh_sense_show_scrollbar && _zsh_sense_view_total > rows )); then
    scrollbar_active=1
    scrollbar_cells=1
  fi
  local -i marker_cells=$#_zsh_sense_selected_marker marker_prefix_cells=0
  (( marker_cells )) && marker_prefix_cells=$(( marker_cells + 1 ))
  # Reserve a stable indicator column for the configured presentation mode.
  # It must not depend on the current viewport, or the panel width would jump
  # when navigation reveals a different candidate kind. The private-use Nerd
  # Font glyphs used here advance one ZLE cell; treating them as double-width
  # shifted every custom right border one column to the right.
  local -i indicator_cells=0
  case $_zsh_sense_indicator_mode in
    icon) indicator_cells=1 ;;
    text) indicator_cells=3 ;;
    both) indicator_cells=16 ;;
  esac
  local -i prefix_cells=$marker_prefix_cells
  (( indicator_cells )) && (( prefix_cells += indicator_cells + 1 ))
  local -i candidate_cells=$_zsh_sense_max_label_cells
  (( _zsh_sense_show_descriptions )) &&
    candidate_cells=$_zsh_sense_max_described_cells
  local -i content_width=$(( prefix_cells + candidate_cells + scrollbar_cells ))
  local -i border_width=2
  [[ $_zsh_sense_border == none ]] && border_width=0
  local -i width=$(( content_width + (2 * _zsh_sense_padding) + border_width ))
  if (( _zsh_sense_show_title )); then
    local title=' completions '
    local -i title_width=$(( $#title + border_width ))
    (( width < title_width )) && width=$title_width
  fi
  if (( _zsh_sense_show_footer )) && [[ $_zsh_sense_border != none ]]; then
    local footer=" $(( _zsh_sense_selected_absolute + 1 ))/$_zsh_sense_view_total "
    local -i footer_width=$(( $#footer + border_width ))
    (( width < footer_width )) && width=$footer_width
  fi
  (( width > _zsh_sense_max_width )) && width=$_zsh_sense_max_width
  (( width < _zsh_sense_min_width )) && width=$_zsh_sense_min_width
  # Leave the terminal's final cell unused. Writing into it can trigger an
  # implicit wrap before ZLE has accounted for the following display row.
  (( width > terminal_width - 1 )) && width=$(( terminal_width - 1 ))
  (( width < 8 )) && return 0
  local -i inner=$(( width - border_width ))
  (( content_width = inner - (2 * _zsh_sense_padding) ))
  local -i row_content_width=$(( content_width - scrollbar_cells ))
  (( row_content_width < 1 )) && return 0
  local -i first=1
  if (( _zsh_sense_selected > rows )); then
    first=$(( _zsh_sense_selected - rows + 1 ))
  fi
  (( first + rows - 1 > $#_zsh_sense_item_ids )) && first=$(( $#_zsh_sense_item_ids - rows + 1 ))
  local -a lines=() highlight_starts=() highlight_ends=() highlight_styles=()
  local -a match_ranges=()
  local fill row marker marker_prefix icon label detail left padding line line_prefix line_suffix
  local kind match_ranges_text match_range label_style match_style kind_style detail_style
  local -i index available icon_cells label_cells detail_cells left_cells indicator_delta
  local -i panel_chars=0 detail_gap=0 detail_start=0 line_start=0
  local -i interior_start=0 interior_end=0 icon_start=0 icon_end=0
  local -i label_offset=0 label_start=0 label_visible=0 match_start=0 match_end=0
  local -i is_selected=0 scrollbar_position=0
  local -i thumb_rows=0 thumb_first=0 row_number=0
  padding=${(l:$_zsh_sense_padding:: :)}
  if (( scrollbar_active )); then
    _zsh_sense_scrollbar_geometry "$rows" "$_zsh_sense_view_total" \
      "$_zsh_sense_selected_absolute"
    thumb_rows=${REPLY%:*}
    thumb_first=${REPLY#*:}
  fi
  if [[ $_zsh_sense_border != none ]]; then
    if (( _zsh_sense_show_title )); then
      fill=${(pl:$(( inner - $#title ))::$horizontal:)}
      line="$tl$title$fill$tr"
    else
      line="$tl${(pl:$inner::$horizontal:)}$tr"
    fi
    lines+=( "$line" )
    highlight_starts+=( $panel_chars )
    highlight_ends+=( $(( panel_chars + ${#line} )) )
    highlight_styles+=( "$_zsh_sense_style_border" )
    (( panel_chars += ${#line} + 1 ))
  fi
  for (( index = first; index < first + rows; index++ )); do
    line_start=$panel_chars
    (( row_number++ ))
    (( is_selected = index == _zsh_sense_selected ))
    marker=
    marker_prefix=
    if (( marker_cells )); then
      marker=${(l:$marker_cells:: :)}
      (( is_selected )) && marker=$_zsh_sense_selected_marker
      marker_prefix="$marker "
    fi
    kind=$_zsh_sense_item_kinds[index]
    _zsh_sense_kind_indicator "$kind"
    icon=$REPLY
    icon_cells=$_zsh_sense_indicator_cells
    label=$_zsh_sense_item_labels[index]
    label_cells=${_zsh_sense_item_label_cells[index]:-${#label}}
    detail=$_zsh_sense_item_details[index]
    detail_cells=${_zsh_sense_item_detail_cells[index]:-${#detail}}
    (( _zsh_sense_show_descriptions )) || detail=
    if [[ -n $icon ]]; then
      left="$marker_prefix$icon $label"
      left_cells=$(( marker_prefix_cells + icon_cells + 1 + label_cells ))
    else
      left="$marker_prefix$label"
      left_cells=$(( marker_prefix_cells + label_cells ))
    fi
    available=$row_content_width
    if [[ -n $detail ]]; then
      if (( left_cells + 2 + detail_cells > available )); then
        local -i detail_width=$(( available / 2 ))
        _zsh_sense_truncate "$detail" $detail_width
        detail=$REPLY
        detail_cells=$#detail
        # The icon's terminal-cell width can exceed its Zsh character count.
        # Remove that delta from the character budget passed to the fallback
        # truncator so a clamped row still keeps its right border aligned.
        indicator_delta=$(( icon_cells - $#icon ))
        (( indicator_delta < 0 )) && indicator_delta=0
        _zsh_sense_truncate "$left" $(( available - detail_cells - 1 - indicator_delta ))
        left=$REPLY
        left_cells=$(( $#left + indicator_delta ))
      fi
      detail_gap=$(( available - left_cells - detail_cells ))
      row="$left${(l:$detail_gap:: :)}$detail"
    else
      if (( left_cells > available )); then
        indicator_delta=$(( icon_cells - $#icon ))
        (( indicator_delta < 0 )) && indicator_delta=0
        _zsh_sense_truncate "$left" $(( available - indicator_delta ))
        left=$REPLY
        left_cells=$(( $#left + indicator_delta ))
      fi
      row="$left${(l:$(( available - left_cells )):: :)}"
    fi
    if (( scrollbar_active )); then
      row+="$_zsh_sense_scrollbar_character"
    fi
    if [[ $_zsh_sense_border == none ]]; then
      line_prefix=$padding
      line_suffix=$padding
    else
      line_prefix="$vertical$padding"
      line_suffix="$padding$vertical"
    fi
    line="$line_prefix$row$line_suffix"

    if [[ $_zsh_sense_border == none ]]; then
      interior_start=$line_start
      interior_end=$(( line_start + ${#line} ))
    else
      interior_start=$(( line_start + 1 ))
      interior_end=$(( line_start + ${#line} - 1 ))
    fi
    if (( is_selected )); then
      highlight_starts+=( $interior_start )
      highlight_ends+=( $interior_end )
      highlight_styles+=( "$_zsh_sense_style_selected" )
      label_style=$_zsh_sense_style_label_selected
      match_style=$_zsh_sense_style_label_match_selected
      kind_style=${_zsh_sense_style_kinds_selected[$kind]:-$_zsh_sense_style_kind_selected}
      detail_style=$_zsh_sense_style_detail_selected
    else
      label_style=$_zsh_sense_style_label
      match_style=$_zsh_sense_style_label_match
      kind_style=${_zsh_sense_style_kinds[$kind]:-$_zsh_sense_style_kind}
      detail_style=$_zsh_sense_style_detail
    fi

    if [[ -n $icon ]]; then
      icon_start=$(( line_start + ${#line_prefix} + ${#marker_prefix} ))
      icon_end=$(( icon_start + ${#icon} ))
      (( icon_end > line_start + ${#line_prefix} + ${#left} )) &&
        icon_end=$(( line_start + ${#line_prefix} + ${#left} ))
      if (( icon_end > icon_start )); then
        highlight_starts+=( $icon_start )
        highlight_ends+=( $icon_end )
        highlight_styles+=( "$kind_style" )
      fi
      label_offset=$(( ${#marker_prefix} + ${#icon} + 1 ))
    else
      label_offset=${#marker_prefix}
    fi
    label_visible=$(( ${#left} - label_offset ))
    (( label_visible > ${#label} )) && label_visible=${#label}
    if (( label_visible > 0 )); then
      label_start=$(( line_start + ${#line_prefix} + label_offset ))
      highlight_starts+=( $label_start )
      highlight_ends+=( $(( label_start + label_visible )) )
      highlight_styles+=( "$label_style" )

      match_ranges_text=$_zsh_sense_item_match_ranges[index]
      match_ranges=( "${(@s:,:)match_ranges_text}" )
      for match_range in "${match_ranges[@]}"; do
        [[ $match_range == <->:<-> ]] || continue
        match_start=${match_range%%:*}
        match_end=${match_range#*:}
        (( match_start < label_visible )) || continue
        (( match_end > label_visible )) && match_end=$label_visible
        (( match_end > match_start )) || continue
        highlight_starts+=( $(( label_start + match_start )) )
        highlight_ends+=( $(( label_start + match_end )) )
        highlight_styles+=( "$match_style" )
      done
    fi
    if [[ -n $detail ]]; then
      detail_start=$(( panel_chars + ${#line_prefix} + ${#left} + detail_gap ))
      highlight_starts+=( $detail_start )
      highlight_ends+=( $(( detail_start + ${#detail} )) )
      highlight_styles+=( "$detail_style" )
    fi
    if [[ $_zsh_sense_border != none ]]; then
      highlight_starts+=( $line_start $(( line_start + ${#line} - 1 )) )
      highlight_ends+=( $(( line_start + 1 )) $(( line_start + ${#line} )) )
      highlight_styles+=( "$_zsh_sense_style_border" "$_zsh_sense_style_border" )
    fi
    if (( scrollbar_active )); then
      scrollbar_position=$(( line_start + ${#line_prefix} + ${#row} - 1 ))
      highlight_starts+=( $scrollbar_position )
      highlight_ends+=( $(( scrollbar_position + 1 )) )
      highlight_styles+=( "$_zsh_sense_style_scrollbar_gutter" )
      if (( row_number > thumb_first && row_number <= thumb_first + thumb_rows )); then
        highlight_starts+=( $scrollbar_position )
        highlight_ends+=( $(( scrollbar_position + 1 )) )
        highlight_styles+=( "$_zsh_sense_style_scrollbar_thumb" )
      fi
    fi
    lines+=( "$line" )
    (( panel_chars += ${#line} + 1 ))
  done
  if [[ $_zsh_sense_border != none ]]; then
    line_start=$panel_chars
    if (( _zsh_sense_show_footer )); then
      fill=${(pl:$(( inner - $#footer ))::$horizontal:)}
      line="$bl$fill$footer$br"
      highlight_starts+=( $line_start $(( line_start + ${#bl} + ${#fill} )) )
      highlight_ends+=( $(( line_start + ${#line} )) $(( line_start + ${#bl} + ${#fill} + ${#footer} )) )
      highlight_styles+=( "$_zsh_sense_style_border" "$_zsh_sense_style_footer" )
    else
      line="$bl${(pl:$inner::$horizontal:)}$br"
      highlight_starts+=( $line_start )
      highlight_ends+=( $(( line_start + ${#line} )) )
      highlight_styles+=( "$_zsh_sense_style_border" )
    fi
    lines+=( "$line" )
  fi
  local panel="${(F)lines}"
  _zsh_sense_render_lines=( "${lines[@]}" )
  _zsh_sense_render_highlight_starts=( 0 "${highlight_starts[@]}" )
  _zsh_sense_render_highlight_ends=( ${#panel} "${highlight_ends[@]}" )
  _zsh_sense_render_highlight_styles=( "$_zsh_sense_style_menu" "${highlight_styles[@]}" )
  _zsh_sense_render_columns=$COLUMNS
  _zsh_sense_render_dirty=0
  _zsh_sense_set_postdisplay "$panel"
  zle -R
}

_zsh_sense_accept_selected() {
  (( _zsh_sense_popup_visible && ! _zsh_sense_popup_stale &&
     _zsh_sense_selected >= 1 &&
     _zsh_sense_selected <= $#_zsh_sense_item_ids )) || return 1
  local backend=$_zsh_sense_item_acceptance_backends[_zsh_sense_selected]
  local identity=$_zsh_sense_item_acceptance_identities[_zsh_sense_selected]
  if [[ $backend == portable && $identity == <-> ]]; then
    _zsh_sense_apply_serial=$_zsh_sense_capture_serial
    _zsh_sense_apply_index=$identity
    zle .zsh-sense-portable-apply
    typeset -gi _zsh_sense_last_apply_status=$?
    (( _zsh_sense_last_apply_status == 0 )) || return 1
    _zsh_sense_clear_popup
    _zsh_sense_last_buffer=$BUFFER
    _zsh_sense_last_cursor=$CURSOR
    (( _zsh_sense_after_accept )) && _zsh_sense_request after-accept
    return 0
  fi
  _zsh_sense_send select "$_zsh_sense_active_request" "$_zsh_sense_active_generation" \
    "$_zsh_sense_item_ids[_zsh_sense_selected]"
}

_zsh_sense_call_original() {
  emulate -L zsh
  local logical=$1 map=${KEYMAP:-main}
  [[ $map == main ]] && map=${_zsh_sense_main_keymap:-viins}
  local widget=${_zsh_sense_original_widgets[$map:$logical]-}
  if [[ -z $widget ]]; then
    # A key may name a widget supplied by an optional plugin that has not been
    # loaded yet (the user's autosuggest-accept binding is one such case).
    # Resolve it lazily so our dispatcher can still be installed now and will
    # delegate correctly if that plugin appears later.
    local original=${_zsh_sense_original_names[$map:$logical]-}
    [[ -n $original && -n ${widgets[$original]-} ]] && widget=$original
  fi
  [[ -n $widget ]] && zle "$widget"
}

_zsh_sense_key_dispatch() {
  emulate -L zsh
  local logical=${_zsh_sense_widget_keys[$WIDGET]-}
  local action=
  if (( _zsh_sense_popup_visible && ! _zsh_sense_popup_stale )); then
    action=${_zsh_sense_bindings_popup[$logical]-}
  else
    action=${_zsh_sense_bindings_closed[$logical]-}
  fi
  case $action in
    trigger) _zsh_sense_request manual ;;
    accept) _zsh_sense_accept_selected || _zsh_sense_call_original "$logical" ;;
    execute)
      if [[ -n $_zsh_sense_owned_postdisplay ]]; then
        # Return once with an empty POSTDISPLAY so ZLE performs its ordinary
        # differential redraw and erases the panel. Requeue the exact bytes
        # that invoked this widget; their second dispatch sees no panel and
        # delegates to the original accept-line widget.
        local execute_keys=$KEYS
        _zsh_sense_erase_edit_display
        _zsh_sense_prepare_line_finish
        zle -U "$execute_keys"
      else
        _zsh_sense_call_original "$logical"
      fi
      ;;
    next)
      if (( _zsh_sense_popup_visible && ! _zsh_sense_popup_stale )); then
        if (( _zsh_sense_selected < $#_zsh_sense_item_ids &&
              _zsh_sense_selected_absolute + 1 < _zsh_sense_view_total )); then
          (( _zsh_sense_selected++, _zsh_sense_selected_absolute++ ))
          _zsh_sense_render_dirty=1
          _zsh_sense_render
        fi
        _zsh_sense_send navigate "$_zsh_sense_active_request" \
          "$_zsh_sense_active_generation" next
      else _zsh_sense_call_original "$logical"; fi
      ;;
    previous)
      if (( _zsh_sense_popup_visible && ! _zsh_sense_popup_stale )); then
        if (( _zsh_sense_selected > 1 && _zsh_sense_selected_absolute > 0 )); then
          (( _zsh_sense_selected--, _zsh_sense_selected_absolute-- ))
          _zsh_sense_render_dirty=1
          _zsh_sense_render
        fi
        _zsh_sense_send navigate "$_zsh_sense_active_request" \
          "$_zsh_sense_active_generation" previous
      else _zsh_sense_call_original "$logical"; fi
      ;;
    page-down)
      if (( _zsh_sense_popup_visible && ! _zsh_sense_popup_stale )); then
        if (( _zsh_sense_selected + _zsh_sense_max_rows <= $#_zsh_sense_item_ids )); then
          (( _zsh_sense_selected += _zsh_sense_max_rows,
             _zsh_sense_selected_absolute += _zsh_sense_max_rows ))
          _zsh_sense_render_dirty=1
          _zsh_sense_render
        fi
        _zsh_sense_send navigate "$_zsh_sense_active_request" \
          "$_zsh_sense_active_generation" page-down
      else _zsh_sense_call_original "$logical"; fi
      ;;
    page-up)
      if (( _zsh_sense_popup_visible && ! _zsh_sense_popup_stale )); then
        if (( _zsh_sense_selected > _zsh_sense_max_rows )); then
          (( _zsh_sense_selected -= _zsh_sense_max_rows,
             _zsh_sense_selected_absolute -= _zsh_sense_max_rows ))
          _zsh_sense_render_dirty=1
          _zsh_sense_render
        fi
        _zsh_sense_send navigate "$_zsh_sense_active_request" \
          "$_zsh_sense_active_generation" page-up
      else _zsh_sense_call_original "$logical"; fi
      ;;
    dismiss)
      (( _zsh_sense_active_request )) && _zsh_sense_send cancel \
        "$_zsh_sense_active_request" "$_zsh_sense_active_generation"
      _zsh_sense_clear_popup
      ;;
    none) ;;
    *) _zsh_sense_call_original "$logical" ;;
  esac
}

_zsh_sense_install_keybindings() {
  emulate -L zsh
  local main_definition
  local -a definition_words maps keys
  main_definition=$(bindkey -lL main 2>/dev/null)
  definition_words=( ${(z)main_definition} )
  if [[ $definition_words[1] == bindkey && $definition_words[2] == -A &&
        -n $definition_words[3] ]]; then
    _zsh_sense_main_keymap=$definition_words[3]
  else
    _zsh_sense_main_keymap=viins
  fi
  maps=( emacs viins "$_zsh_sense_main_keymap" )
  maps=( ${(u)maps} )
  keys=( ${(k)_zsh_sense_bindings_closed} ${(k)_zsh_sense_bindings_popup} )
  keys=( ${(u)keys} )
  local map logical sequence line original alias widget safe
  for logical in "${keys[@]}"; do
    sequence=${_zsh_sense_key_sequences[$logical]-}
    [[ -n $sequence ]] || continue
    safe=${logical//[^A-Za-z0-9_-]/-}
    widget=".zsh-sense-key-$safe"
    _zsh_sense_widget_keys[$widget]=$logical
    zle -N "$widget" _zsh_sense_key_dispatch
    for map in "${maps[@]}"; do
      line=$(bindkey -M "$map" "$sequence" 2>/dev/null) || continue
      definition_words=( ${(z)line} )
      original=${definition_words[2]-}
      [[ -n $original && $original != "$widget" ]] || continue
      alias=".zsh-sense-original-$map-$safe"
      _zsh_sense_original_names[$map:$logical]=$original
      if zle -A "$original" "$alias" 2>/dev/null; then
        _zsh_sense_original_widgets[$map:$logical]=$alias
      else
        _zsh_sense_original_widgets[$map:$logical]=
      fi
      _zsh_sense_bound_sequences[$map:$logical]=$sequence
      bindkey -M "$map" "$sequence" "$widget"
    done
  done
}

_zsh_sense_disconnect() {
  emulate -L zsh
  if (( _zsh_sense_read_fd >= 0 )); then
    zle -F $_zsh_sense_read_fd 2>/dev/null
    exec {_zsh_sense_read_fd}<&- 2>/dev/null
  fi
  if (( _zsh_sense_write_fd >= 0 )); then
    exec {_zsh_sense_write_fd}>&- 2>/dev/null
  fi
  _zsh_sense_read_fd=-1
  _zsh_sense_write_fd=-1
  _zsh_sense_ready=0
  _zsh_sense_configured=0
  _zsh_sense_clear_popup
}

_zsh_sense_cleanup() {
  emulate -L zsh
  autoload -Uz add-zle-hook-widget add-zsh-hook
  add-zle-hook-widget -d line-pre-redraw _zsh_sense_line_pre_redraw 2>/dev/null
  add-zle-hook-widget -d line-init _zsh_sense_line_init 2>/dev/null
  add-zle-hook-widget -d line-finish _zsh_sense_line_finish 2>/dev/null
  add-zsh-hook -d zshexit _zsh_sense_cleanup 2>/dev/null
  local key map logical sequence original
  for key in ${(k)_zsh_sense_bound_sequences}; do
    map=${key%%:*}
    logical=${key#*:}
    sequence=$_zsh_sense_bound_sequences[$key]
    original=$_zsh_sense_original_names[$key]
    [[ -n $sequence && -n $original ]] && bindkey -M "$map" "$sequence" "$original" 2>/dev/null
  done
  (( _zsh_sense_ready )) && _zsh_sense_send goodbye
  _zsh_sense_disconnect
  [[ -n $_zsh_sense_fifo_in ]] && command unlink -- "$_zsh_sense_fifo_in" 2>/dev/null
  [[ -n $_zsh_sense_fifo_out ]] && command unlink -- "$_zsh_sense_fifo_out" 2>/dev/null
}

_zsh_sense_start_worker() {
  emulate -L zsh
  setopt localoptions no_aliases
  local root=${_zsh_sense_plugin_dir:h}
  local -a command_line worker_args
  if [[ -n ${SENSE_ZSH_COMMAND:-} ]]; then
    command_line=( ${(z)SENSE_ZSH_COMMAND} )
  elif (( $+commands[zsh-sense] )); then
    command_line=( "$commands[zsh-sense]" )
  elif [[ -x $root/target/release/zsh-sense ]]; then
    command_line=( "$root/target/release/zsh-sense" )
  elif [[ -x $root/target/debug/zsh-sense ]]; then
    command_line=( "$root/target/debug/zsh-sense" )
  else
    return 1
  fi

  local runtime_base=${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}/zsh-sense-$UID}
  local runtime_dir=$runtime_base/zsh-sense
  command mkdir -p -m 700 -- "$runtime_dir" 2>/dev/null || return 1
  command chmod 700 -- "$runtime_dir" 2>/dev/null || return 1
  [[ -d $runtime_dir && -O $runtime_dir && ! -L $runtime_dir ]] || return 1
  local token="${sysparams[pid]}-${RANDOM}-${RANDOM}"
  _zsh_sense_fifo_in="$runtime_dir/shell-$token.in"
  _zsh_sense_fifo_out="$runtime_dir/shell-$token.out"
  command mkfifo -m 600 -- "$_zsh_sense_fifo_in" "$_zsh_sense_fifo_out" || return 1

  local state_base=${XDG_STATE_HOME:-$HOME/.local/state}
  command mkdir -p -m 700 -- "$state_base/zsh-sense" 2>/dev/null
  _zsh_sense_log_file="$state_base/zsh-sense/worker-${sysparams[pid]}.log"
  worker_args=( worker --shell-input-fifo "$_zsh_sense_fifo_in" \
    --shell-output-fifo "$_zsh_sense_fifo_out" \
    --zsh-executable "${commands[zsh]:-$SHELL}" --zsh-version "$ZSH_VERSION" \
    --zsh-patchlevel "$ZSH_PATCHLEVEL" )
  [[ -n ${SENSE_ZSH_SOCKET:-} ]] && worker_args+=( --socket "$SENSE_ZSH_SOCKET" )
  [[ -n ${SENSE_ZSH_CONFIG:-} ]] && worker_args+=( --config "$SENSE_ZSH_CONFIG" )
  [[ -n ${ZSH_SENSE_PROFILE:-} ]] && worker_args+=( --profile "$ZSH_SENSE_PROFILE" )
  [[ ${SENSE_ZSH_NO_DAEMON_AUTOSTART:-0} == 1 ]] && worker_args+=( --no-daemon-autostart )
  "${command_line[@]}" "${worker_args[@]}" </dev/null >>"$_zsh_sense_log_file" 2>&1 &!
  _zsh_sense_worker_pid=$!

  local -i attempt
  for (( attempt = 1; attempt <= 200; attempt++ )); do
    sysopen -w -o cloexec,nonblock -u _zsh_sense_write_fd "$_zsh_sense_fifo_in" 2>/dev/null && break
    kill -0 $_zsh_sense_worker_pid 2>/dev/null || break
    zselect -t 1 >/dev/null 2>&1
  done
  (( _zsh_sense_write_fd >= 0 )) || { _zsh_sense_cleanup; return 1; }
  sysopen -r -o cloexec,nonblock -u _zsh_sense_read_fd "$_zsh_sense_fifo_out" 2>/dev/null || {
    _zsh_sense_cleanup
    return 1
  }
  command unlink -- "$_zsh_sense_fifo_in" 2>/dev/null
  command unlink -- "$_zsh_sense_fifo_out" 2>/dev/null
  _zsh_sense_fifo_in=
  _zsh_sense_fifo_out=
  zle -N .zsh-sense-fd-callback _zsh_sense_fd_callback
  zle -Fw $_zsh_sense_read_fd .zsh-sense-fd-callback
}

_zsh_sense_init() {
  emulate -L zsh
  [[ -o interactive ]] || return 0
  zmodload zsh/system zsh/zselect zsh/terminfo 2>/dev/null || return 1
  autoload -Uz add-zle-hook-widget add-zsh-hook
  _zsh_sense_rebuild_styles
  _zsh_sense_portable_init || return 1
  add-zle-hook-widget line-pre-redraw _zsh_sense_line_pre_redraw
  add-zle-hook-widget line-init _zsh_sense_line_init
  add-zle-hook-widget line-finish _zsh_sense_line_finish
  add-zsh-hook zshexit _zsh_sense_cleanup
  _zsh_sense_last_buffer=
  _zsh_sense_last_cursor=-1
  _zsh_sense_start_worker
}
