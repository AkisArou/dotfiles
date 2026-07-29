# Zsh line-editor client for Shell Sense.

typeset -gi _shell_sense_read_fd=-1
typeset -gi _shell_sense_write_fd=-1
typeset -gi _shell_sense_sync_fd=-1
typeset -gi _shell_sense_sync_active=0
typeset -gi _shell_sense_redraw_pending=0
typeset -gi _shell_sense_worker_pid=0
typeset -gi _shell_sense_ready=0
typeset -gi _shell_sense_configured=0
typeset -gi _shell_sense_request_serial=0
typeset -gi _shell_sense_generation=0
typeset -gi _shell_sense_active_request=0
typeset -gi _shell_sense_active_generation=0
typeset -gi _shell_sense_active_cursor_byte=0
typeset -gi _shell_sense_active_cursor=0
typeset -gi _shell_sense_popup_visible=0
typeset -gi _shell_sense_external_presentation=0
typeset -gi _shell_sense_popup_stale=0
typeset -gi _shell_sense_render_dirty=1
typeset -gi _shell_sense_render_columns=0
typeset -gi _shell_sense_render_menu_lines=0
typeset -gi _shell_sense_render_first=1
typeset -gi _shell_sense_menu_view_start=0
typeset -gi _shell_sense_menu_view_request=0
typeset -gi _shell_sense_menu_view_generation=0
typeset -gi _shell_sense_navigation_serial=0
typeset -gi _shell_sense_temp_navigation_serial=0
typeset -gi _shell_sense_indicator_cells=0
typeset -gi _shell_sense_selected=0
typeset -gi _shell_sense_view_revision=0
typeset -gi _shell_sense_view_building=0
typeset -gi _shell_sense_menu_width=0
typeset -gi _shell_sense_temp_view_revision=0
typeset -gi _shell_sense_temp_menu_width=0
typeset -gi _shell_sense_temp_selected=0
typeset -gi _shell_sense_temp_expected=0
typeset -gi _shell_sense_temp_received=0
typeset -gi _shell_sense_temp_total=0
typeset -gi _shell_sense_temp_window_start=0
typeset -gi _shell_sense_temp_selected_absolute=0
typeset -gi _shell_sense_temp_max_label_cells=0
typeset -gi _shell_sense_temp_max_described_cells=0
typeset -gi _shell_sense_view_total=0
typeset -gi _shell_sense_view_window_start=0
typeset -gi _shell_sense_selected_absolute=0
typeset -gi _shell_sense_max_label_cells=0
typeset -gi _shell_sense_max_described_cells=0
typeset -gi _shell_sense_last_apply_status=0
typeset -gi _shell_sense_parse_offset=1
typeset -gi _shell_sense_interrupt_key_enabled=1
typeset -gi _shell_sense_terminal_interrupt_disabled=0
typeset -g _shell_sense_rx_buffer=
typeset -g _shell_sense_parse_value=
typeset -g _shell_sense_active_buffer=
typeset -g _shell_sense_continuity_ghost=
typeset -g _shell_sense_last_buffer=
typeset -g _shell_sense_owned_postdisplay=
typeset -gi _shell_sense_last_cursor=-1
typeset -g _shell_sense_activation_mode=continuous
typeset -gi _shell_sense_after_accept=1
typeset -gi _shell_sense_popup_enabled=1
typeset -gi _shell_sense_max_rows=10
typeset -gi _shell_sense_scrolloff=2
typeset -gi _shell_sense_cycle=1
typeset -gi _shell_sense_max_width=140
typeset -gi _shell_sense_min_width=24
typeset -gi _shell_sense_padding=1
typeset -g _shell_sense_decorations=full
typeset -g _shell_sense_border=none
typeset -gi _shell_sense_show_title=0
typeset -gi _shell_sense_show_footer=1
typeset -gi _shell_sense_show_scrollbar=1
typeset -g _shell_sense_scrollbar_character='▐'
typeset -gi _shell_sense_documentation_padding=0
typeset -gi _shell_sense_show_documentation_scrollbar=1
typeset -gi _shell_sense_show_groups=1
typeset -gi _shell_sense_show_descriptions=1
typeset -g _shell_sense_indicator_mode=icon
typeset -g _shell_sense_selected_marker=
typeset -g _shell_sense_style_menu_raw='fg=#bbbbbb,bg=#202020'
typeset -g _shell_sense_style_border_raw='fg=#d4d4d4'
typeset -g _shell_sense_style_selected_raw='bg=#343b41'
typeset -g _shell_sense_style_label_raw='fg=#d4d4d4'
typeset -g _shell_sense_style_label_match_raw='fg=#18a2fe,bold'
typeset -g _shell_sense_style_detail_raw='fg=#bbbbbb'
typeset -g _shell_sense_style_kind_raw='fg=#bbbbbb'
typeset -g _shell_sense_style_group_raw='fg=#4ec9b0'
typeset -g _shell_sense_style_footer_raw='fg=#bbbbbb'
typeset -g _shell_sense_style_scrollbar_thumb_raw='fg=#bbbbbb'
typeset -g _shell_sense_style_scrollbar_gutter_raw='fg=#343b41'
typeset -g _shell_sense_style_ghost_raw='fg=#707070'
typeset -g _shell_sense_style_documentation_raw='fg=#d4d4d4,bg=#202020'
typeset -g _shell_sense_style_documentation_border_raw='fg=#d4d4d4'
typeset -g _shell_sense_style_documentation_heading_raw='fg=#18a2fe,bold'
typeset -g _shell_sense_style_documentation_code_raw='fg=#ce9178'
typeset -g _shell_sense_style_documentation_quote_raw='fg=#808080'
typeset -g _shell_sense_style_menu=
typeset -g _shell_sense_style_border=
typeset -g _shell_sense_style_selected=
typeset -g _shell_sense_style_label=
typeset -g _shell_sense_style_label_selected=
typeset -g _shell_sense_style_label_match=
typeset -g _shell_sense_style_label_match_selected=
typeset -g _shell_sense_style_detail=
typeset -g _shell_sense_style_detail_selected=
typeset -g _shell_sense_style_kind=
typeset -g _shell_sense_style_kind_selected=
typeset -g _shell_sense_style_group=
typeset -g _shell_sense_style_footer=
typeset -g _shell_sense_style_scrollbar_thumb=
typeset -g _shell_sense_style_scrollbar_gutter=
typeset -g _shell_sense_style_ghost=
typeset -g _shell_sense_style_documentation=
typeset -g _shell_sense_style_documentation_border=
typeset -g _shell_sense_style_documentation_heading=
typeset -g _shell_sense_style_documentation_code=
typeset -g _shell_sense_style_documentation_quote=
typeset -gi _shell_sense_ghost_enabled=1
typeset -g _shell_sense_ghost_partial_accept=token
typeset -gA _shell_sense_style_kinds_raw=(
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
)
typeset -gA _shell_sense_style_kinds=()
typeset -gA _shell_sense_style_kinds_selected=()
typeset -gi _shell_sense_capture_fuzzy_min_chars=3
typeset -ga _shell_sense_trigger_characters=( / - = : ' ' )
typeset -ga _shell_sense_immediate_characters=( / - = )
typeset -ga _shell_sense_events=( insert backspace delete word-delete paste history cursor accept )
typeset -gA _shell_sense_bindings_closed=()
typeset -gA _shell_sense_bindings_popup=()
typeset -gA _shell_sense_key_sequences=(
  tab '^I'
  ctrl-space '^@'
  ctrl-c '^C'
  ctrl-e '^E'
  enter '^M'
  ctrl-n '^N'
  ctrl-p '^P'
  ctrl-d '^D'
  ctrl-u '^U'
  ctrl-f '^F'
  ctrl-b '^B'
  ctrl-g '^G'
  escape '^['
  right '^[[C'
  end '^[[F'
  shift-tab '^[[Z'
)
typeset -gA _shell_sense_original_widgets=()
typeset -gA _shell_sense_original_names=()
typeset -gA _shell_sense_bound_sequences=()
typeset -gA _shell_sense_widget_keys=()
typeset -ga _shell_sense_item_ids=()
typeset -ga _shell_sense_item_labels=()
typeset -ga _shell_sense_item_label_cells=()
typeset -ga _shell_sense_item_details=()
typeset -ga _shell_sense_item_detail_cells=()
typeset -ga _shell_sense_item_kinds=()
typeset -ga _shell_sense_item_icons=()
typeset -ga _shell_sense_item_match_ranges=()
typeset -ga _shell_sense_item_groups=()
typeset -ga _shell_sense_item_insertions=()
typeset -ga _shell_sense_item_acceptance_sources=()
typeset -ga _shell_sense_item_acceptance_identities=()
typeset -ga _shell_sense_item_ghosts=()
typeset -ga _shell_sense_render_lines=()
typeset -ga _shell_sense_render_highlight_starts=()
typeset -ga _shell_sense_render_highlight_ends=()
typeset -ga _shell_sense_render_highlight_styles=()
typeset -ga _shell_sense_temp_ids=()
typeset -ga _shell_sense_temp_labels=()
typeset -ga _shell_sense_temp_label_cells=()
typeset -ga _shell_sense_temp_details=()
typeset -ga _shell_sense_temp_detail_cells=()
typeset -ga _shell_sense_temp_kinds=()
typeset -ga _shell_sense_temp_icons=()
typeset -ga _shell_sense_temp_match_ranges=()
typeset -ga _shell_sense_temp_groups=()
typeset -ga _shell_sense_temp_insertions=()
typeset -ga _shell_sense_temp_acceptance_sources=()
typeset -ga _shell_sense_temp_acceptance_identities=()
typeset -ga _shell_sense_temp_ghosts=()
typeset -g _shell_sense_documentation_item=
typeset -g _shell_sense_documentation_placement=
typeset -gi _shell_sense_documentation_width=0
typeset -gi _shell_sense_documentation_viewport_rows=0
typeset -gi _shell_sense_documentation_offset=0
typeset -gi _shell_sense_documentation_total=0
typeset -gi _shell_sense_documentation_scrollbar=0
typeset -ga _shell_sense_documentation_kinds=()
typeset -ga _shell_sense_documentation_cells=()
typeset -ga _shell_sense_documentation_lines=()
typeset -g _shell_sense_temp_documentation_item=
typeset -g _shell_sense_temp_documentation_placement=
typeset -gi _shell_sense_temp_documentation_width=0
typeset -gi _shell_sense_temp_documentation_expected=0
typeset -gi _shell_sense_temp_documentation_received=0
typeset -gi _shell_sense_temp_documentation_viewport_rows=0
typeset -gi _shell_sense_temp_documentation_offset=0
typeset -gi _shell_sense_temp_documentation_total=0
typeset -gi _shell_sense_temp_documentation_scrollbar=0
typeset -ga _shell_sense_temp_documentation_kinds=()
typeset -ga _shell_sense_temp_documentation_cells=()
typeset -ga _shell_sense_temp_documentation_lines=()
typeset -g _shell_sense_fifo_in=
typeset -g _shell_sense_fifo_out=
typeset -g _shell_sense_sync_fifo=
typeset -g _shell_sense_log_file=
typeset -g _shell_sense_ui_locale=${LC_ALL:-${LC_CTYPE:-${LANG:-C.UTF-8}}}

_shell_sense_byte_length() {
  emulate -L zsh
  local LC_ALL=C
  REPLY=${#1}
}

_shell_sense_cursor_byte() {
  emulate -L zsh
  local left=
  (( CURSOR > 0 )) && left=$BUFFER[1,CURSOR]
  _shell_sense_byte_length "$left"
}

_shell_sense_netstring() {
  emulate -L zsh
  local LC_ALL=C value=$1
  REPLY="${#value}:$value,"
}

_shell_sense_encode_message() {
  emulate -L zsh
  setopt localoptions no_aliases

  local command=$1 field data=
  shift
  _shell_sense_netstring "$command"
  data=$REPLY
  _shell_sense_netstring "$#"
  data+=$REPLY
  for field in "$@"; do
    _shell_sense_netstring "$field"
    data+=$REPLY
  done
  REPLY=$data
}

_shell_sense_write_messages() {
  emulate -L zsh
  setopt localoptions no_aliases
  (( _shell_sense_write_fd >= 0 )) || return 1

  local data=$1 chunk=

  local LC_ALL=C
  local -i offset=1 written=0 write_status=0
  local errno_name=
  while (( offset <= $#data )); do
    chunk=$data[offset,-1]
    syswrite -c written -o $_shell_sense_write_fd "$chunk" 2>/dev/null
    write_status=$?
    if (( written > 0 )); then
      (( offset += written ))
      continue
    fi
    if (( write_status == 2 )); then
      errno_name=${errnos[${ERRNO:-0}]-}
      if [[ $errno_name == (EAGAIN|EWOULDBLOCK) ]]; then
        zselect -w $_shell_sense_write_fd -t 5 >/dev/null 2>&1
        continue
      fi
    fi
    _shell_sense_disconnect
    return 1
  done
}

_shell_sense_send() {
  emulate -L zsh
  setopt localoptions no_aliases
  _shell_sense_encode_message "$@" || return 1
  local data=$REPLY
  _shell_sense_write_messages "$data"
}

_shell_sense_take_netstring() {
  emulate -L zsh
  setopt localoptions no_aliases
  local LC_ALL=C
  local -i total=$#_shell_sense_rx_buffer start=$_shell_sense_parse_offset
  (( start <= total )) || return 1
  local tail=$_shell_sense_rx_buffer[start,-1]
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
  [[ $_shell_sense_rx_buffer[comma] == ',' ]] || return 2
  if (( payload_length )); then
    _shell_sense_parse_value=$_shell_sense_rx_buffer[payload_start,payload_end]
  else
    _shell_sense_parse_value=
  fi
  _shell_sense_parse_offset=$(( comma + 1 ))
}

_shell_sense_parse_messages() {
  emulate -L zsh
  setopt localoptions no_aliases
  local LC_ALL=C
  local command count_text
  local -a fields
  local -i parse_status count index consumed

  while [[ -n $_shell_sense_rx_buffer ]]; do
    _shell_sense_parse_offset=1
    _shell_sense_take_netstring
    parse_status=$?
    (( parse_status == 1 )) && return 0
    (( parse_status == 0 )) || { _shell_sense_disconnect; return 1; }
    command=$_shell_sense_parse_value

    _shell_sense_take_netstring
    parse_status=$?
    (( parse_status == 1 )) && return 0
    (( parse_status == 0 )) || { _shell_sense_disconnect; return 1; }
    count_text=$_shell_sense_parse_value
    [[ $count_text == <-> && ( $count_text == 0 || $count_text != 0* ) ]] || {
      _shell_sense_disconnect
      return 1
    }
    count=$(( 10#$count_text ))
    (( count <= 128 )) || { _shell_sense_disconnect; return 1; }
    fields=()
    for (( index = 1; index <= count; index++ )); do
      _shell_sense_take_netstring
      parse_status=$?
      (( parse_status == 1 )) && return 0
      (( parse_status == 0 )) || { _shell_sense_disconnect; return 1; }
      fields+=( "$_shell_sense_parse_value" )
    done
    consumed=$_shell_sense_parse_offset
    if (( consumed > $#_shell_sense_rx_buffer )); then
      _shell_sense_rx_buffer=
    else
      _shell_sense_rx_buffer=$_shell_sense_rx_buffer[consumed,-1]
    fi
    _shell_sense_dispatch "$command" "${fields[@]}"
  done
}

_shell_sense_fd_callback() {
  emulate -L zsh
  setopt localoptions no_aliases
  _shell_sense_ui_locale=${LC_ALL:-${LC_CTYPE:-${LANG:-C.UTF-8}}}
  local -i fd=$1 count=0 read_status=0
  local chunk=
  [[ $fd == $_shell_sense_read_fd ]] || return 0

  while true; do
    chunk=
    sysread -c count -i $fd -s 65536 -t 0 chunk 2>/dev/null
    read_status=$?
    if (( read_status == 0 )); then
      _shell_sense_rx_buffer+=$chunk
      continue
    fi
    if (( read_status == 5 )); then
      if (( ! _shell_sense_ready && _shell_sense_worker_pid > 0 )) &&
          kill -0 $_shell_sense_worker_pid 2>/dev/null; then
        return 0
      fi
      _shell_sense_disconnect
      return 0
    fi
    break
  done
  _shell_sense_parse_messages
  return 0
}

_shell_sense_dispatch() {
  emulate -L zsh
  # Netstring parsing is byte-oriented and runs under LC_ALL=C. Do not leak
  # that dynamic locale into completion functions or popup construction.
  local LC_ALL=$_shell_sense_ui_locale
  local command=$1
  shift
  local -a fields=( "$@" )
  case $command in
    ready)
      _shell_sense_ready=1
      ;;
    config)
      _shell_sense_apply_config "${fields[@]}"
      ;;
    keybinding)
      (( $#fields == 3 )) && case $fields[1] in
        closed) _shell_sense_bindings_closed[$fields[2]]=$fields[3] ;;
        popup) _shell_sense_bindings_popup[$fields[2]]=$fields[3] ;;
      esac
      ;;
    style)
      (( $#fields == 2 )) && _shell_sense_apply_style "$fields[1]" "$fields[2]"
      ;;
    popup-option)
      if (( $#fields == 2 )); then
        case $fields[1] in
          scrollbar-character) _shell_sense_scrollbar_character=$fields[2] ;;
          scrolloff) _shell_sense_scrolloff=$fields[2] ;;
          cycle) _shell_sense_cycle=$fields[2] ;;
          documentation-padding) _shell_sense_documentation_padding=$fields[2] ;;
          documentation-scrollbar) _shell_sense_show_documentation_scrollbar=$fields[2] ;;
        esac
        _shell_sense_render_dirty=1
      fi
      ;;
    ghost-config)
      if (( $#fields == 2 )); then
        _shell_sense_ghost_enabled=$fields[1]
        _shell_sense_ghost_partial_accept=$fields[2]
        _shell_sense_render_dirty=1
      fi
      ;;
    kind-style)
      (( $#fields == 2 )) && _shell_sense_style_kinds_raw[$fields[1]]=$fields[2]
      ;;
    config-end)
      _shell_sense_rebuild_styles
      _shell_sense_install_keybindings
      _shell_sense_configure_interrupt_key
      _shell_sense_configured=1
      ;;
    presentation)
      if (( $#fields == 1 )); then
        _shell_sense_external_presentation=$fields[1]
        if (( _shell_sense_external_presentation )); then
          _shell_sense_hide_popup
        elif (( $#_shell_sense_item_ids && _shell_sense_popup_enabled )); then
          _shell_sense_popup_visible=1
          _shell_sense_render_dirty=1
          _shell_sense_render
        fi
      fi
      ;;
    capture-request)
      _shell_sense_capture_request "${fields[@]}"
      ;;
    view-begin)
      _shell_sense_view_begin "${fields[@]}"
      ;;
    view-chunk)
      _shell_sense_view_chunk "${fields[@]}"
      ;;
    view-layout)
      _shell_sense_view_layout "${fields[@]}"
      ;;
    selection-changed)
      _shell_sense_selection_changed "${fields[@]}"
      ;;
    documentation-begin)
      _shell_sense_documentation_begin "${fields[@]}"
      ;;
    documentation-chunk)
      _shell_sense_documentation_chunk "${fields[@]}"
      ;;
    documentation-end)
      _shell_sense_documentation_end "${fields[@]}"
      ;;
    documentation-clear)
      _shell_sense_documentation_clear "${fields[@]}"
      ;;
    view-end)
      _shell_sense_view_end "${fields[@]}"
      ;;
    accept-zsh)
      _shell_sense_accept_zsh "${fields[@]}"
      ;;
    request-cancelled)
      if [[ $fields[1] == $_shell_sense_active_request && $fields[2] == $_shell_sense_active_generation ]]; then
        _shell_sense_clear_popup
      fi
      ;;
    error)
      typeset -g _shell_sense_last_error="${fields[1]-}: ${fields[2]-}"
      ;;
  esac
}

_shell_sense_send_command_candidates() {
  emulate -L zsh
  setopt localoptions no_aliases
  local request=$1 generation=$2
  local -i request_cursor=$3 total=$#_shell_sense_capture_words
  # 10 uniform header fields + (55 * 2) item fields = 120 fields, below the
  # shell wire limit of 128. Rust derives presentation metadata from kind and
  # the ordinal is implicit, so neither is repeated in Zsh.
  local -i batch_size=55 first last count index prefix_bytes suffix_bytes start end
  local -a fields wire_messages=()
  local word kind

  _shell_sense_encode_message zsh-capture-begin "$request" "$generation"
  wire_messages+=( "$REPLY" )
  for (( first = 1; first <= total; first += batch_size )); do
    # Input may arrive after capture started. Stop at a bounded batch boundary
    # so continuous completion never monopolizes ZLE while typed bytes wait.
    (( PENDING > 0 || KEYS_QUEUED_COUNT > 0 )) && break
    (( last = first + batch_size - 1 ))
    (( last > total )) && last=$total
    (( count = last - first + 1 ))
    _shell_sense_byte_length "$_shell_sense_fast_command_prefix"
    prefix_bytes=$REPLY
    _shell_sense_byte_length "$_shell_sense_fast_command_suffix"
    suffix_bytes=$REPLY
    (( start = request_cursor - prefix_bytes, start < 0 )) && start=0
    (( end = request_cursor + suffix_bytes ))
    fields=(
      "$request" "$generation" "$start" "$end"
      "$_shell_sense_fast_command_prefix" "$_shell_sense_fast_command_suffix"
      "$_shell_sense_fast_command_iprefix" "$_shell_sense_fast_command_isuffix"
      "$first" "$count"
    )
    for (( index = first; index <= last; index++ )); do
      word=$_shell_sense_capture_words[index]
      kind=${_shell_sense_capture_kinds[index]:-text}
      fields+=( "$word" "$kind" )
    done
    _shell_sense_encode_message zsh-command-candidates "${fields[@]}" || return 1
    wire_messages+=( "$REPLY" )
    if (( $#wire_messages >= 4 )); then
      _shell_sense_write_messages "${(j::)wire_messages}" || return 1
      wire_messages=()
    fi
  done
  _shell_sense_encode_message capture-end "$request" "$generation"
  wire_messages+=( "$REPLY" )
  _shell_sense_write_messages "${(j::)wire_messages}"
}

_shell_sense_send_native_context() {
  emulate -L zsh
  setopt localoptions no_aliases
  local request=$1 generation=$2 current=
  (( _shell_sense_native_context_current >= 0 )) &&
    current=$_shell_sense_native_context_current
  local -a messages=() fields=()
  local -i total=$#_shell_sense_native_context_words first last count
  _shell_sense_encode_message context-begin "$request" "$generation" "$current" "$total"
  messages+=( "$REPLY" )
  for (( first = 1; first <= total; first += 60 )); do
    (( last = first + 59, last > total )) && last=$total
    (( count = last - first + 1 ))
    fields=( "$request" "$generation" "$(( first - 1 ))" "$count"
      "${(@)_shell_sense_native_context_words[first,last]}" )
    _shell_sense_encode_message context-chunk "${fields[@]}"
    messages+=( "$REPLY" )
  done
  _shell_sense_encode_message context-end "$request" "$generation"
  messages+=( "$REPLY" )
  _shell_sense_write_messages "${(j::)messages}"
}

_shell_sense_merge_styles() {
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

_shell_sense_apply_style() {
  emulate -L zsh
  local name=$1 value=$2
  case $name in
    menu) _shell_sense_style_menu_raw=$value ;;
    border) _shell_sense_style_border_raw=$value ;;
    selected) _shell_sense_style_selected_raw=$value ;;
    label) _shell_sense_style_label_raw=$value ;;
    label-match) _shell_sense_style_label_match_raw=$value ;;
    detail) _shell_sense_style_detail_raw=$value ;;
    kind) _shell_sense_style_kind_raw=$value ;;
    group) _shell_sense_style_group_raw=$value ;;
    footer) _shell_sense_style_footer_raw=$value ;;
    scrollbar-thumb) _shell_sense_style_scrollbar_thumb_raw=$value ;;
    scrollbar-gutter) _shell_sense_style_scrollbar_gutter_raw=$value ;;
    ghost) _shell_sense_style_ghost_raw=$value ;;
    documentation) _shell_sense_style_documentation_raw=$value ;;
    documentation-border) _shell_sense_style_documentation_border_raw=$value ;;
    documentation-heading) _shell_sense_style_documentation_heading_raw=$value ;;
    documentation-code) _shell_sense_style_documentation_code_raw=$value ;;
    documentation-quote) _shell_sense_style_documentation_quote_raw=$value ;;
  esac
  _shell_sense_render_dirty=1
}

_shell_sense_rebuild_styles() {
  emulate -L zsh
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw"
  _shell_sense_style_menu=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_border_raw"
  _shell_sense_style_border=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_selected_raw"
  _shell_sense_style_selected=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_label_raw"
  _shell_sense_style_label=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_label_raw" \
    "$_shell_sense_style_selected_raw"
  _shell_sense_style_label_selected=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_label_raw" \
    "$_shell_sense_style_label_match_raw"
  _shell_sense_style_label_match=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_label_raw" \
    "$_shell_sense_style_label_match_raw" "$_shell_sense_style_selected_raw"
  _shell_sense_style_label_match_selected=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_detail_raw"
  _shell_sense_style_detail=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_detail_raw" \
    "$_shell_sense_style_selected_raw"
  _shell_sense_style_detail_selected=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_kind_raw"
  _shell_sense_style_kind=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_kind_raw" \
    "$_shell_sense_style_selected_raw"
  _shell_sense_style_kind_selected=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_group_raw"
  _shell_sense_style_group=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_footer_raw"
  _shell_sense_style_footer=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_scrollbar_thumb_raw"
  _shell_sense_style_scrollbar_thumb=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_scrollbar_gutter_raw"
  _shell_sense_style_scrollbar_gutter=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_ghost_raw"
  _shell_sense_style_ghost=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_documentation_raw"
  _shell_sense_style_documentation=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_documentation_raw" \
    "$_shell_sense_style_documentation_border_raw"
  _shell_sense_style_documentation_border=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_documentation_raw" \
    "$_shell_sense_style_documentation_heading_raw"
  _shell_sense_style_documentation_heading=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_documentation_raw" \
    "$_shell_sense_style_documentation_code_raw"
  _shell_sense_style_documentation_code=$REPLY
  _shell_sense_merge_styles "$_shell_sense_style_documentation_raw" \
    "$_shell_sense_style_documentation_quote_raw"
  _shell_sense_style_documentation_quote=$REPLY

  _shell_sense_style_kinds=()
  _shell_sense_style_kinds_selected=()
  local kind
  for kind in ${(k)_shell_sense_style_kinds_raw}; do
    _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_kind_raw" \
      "$_shell_sense_style_kinds_raw[$kind]"
    _shell_sense_style_kinds[$kind]=$REPLY
    _shell_sense_merge_styles "$_shell_sense_style_menu_raw" "$_shell_sense_style_kind_raw" \
      "$_shell_sense_style_kinds_raw[$kind]" "$_shell_sense_style_selected_raw"
    _shell_sense_style_kinds_selected[$kind]=$REPLY
  done
  _shell_sense_render_dirty=1
}

_shell_sense_apply_config() {
  emulate -L zsh
  local -a fields=( "$@" )
  (( $#fields >= 23 )) || return 1
  _shell_sense_activation_mode=$fields[1]
  _shell_sense_after_accept=$fields[3]
  _shell_sense_popup_enabled=$fields[4]
  _shell_sense_max_rows=$fields[5]
  _shell_sense_max_width=$fields[6]
  _shell_sense_min_width=$fields[7]
  _shell_sense_padding=$fields[8]
  _shell_sense_decorations=$fields[9]
  _shell_sense_border=$fields[10]
  _shell_sense_show_title=$fields[11]
  _shell_sense_show_footer=$fields[12]
  _shell_sense_show_scrollbar=$fields[13]
  _shell_sense_show_groups=$fields[14]
  _shell_sense_show_descriptions=$fields[15]
  _shell_sense_style_detail_raw=$fields[16]
  _shell_sense_indicator_mode=$fields[17]
  _shell_sense_selected_marker=$fields[18]
  _shell_sense_capture_matcher=$fields[19]
  _shell_sense_capture_fuzzy_min_chars=$fields[20]
  _shell_sense_render_dirty=1
  local -i offset=21 count=$fields[21]
  (( offset++ ))
  if (( count )); then
    _shell_sense_trigger_characters=( "${(@)fields[offset,$(( offset + count - 1 ))]}" )
  else
    _shell_sense_trigger_characters=()
  fi
  (( offset += count ))
  (( offset <= $#fields )) || return 1
  count=$fields[offset]
  (( offset++ ))
  if (( count )); then
    _shell_sense_immediate_characters=( "${(@)fields[offset,$(( offset + count - 1 ))]}" )
  else
    _shell_sense_immediate_characters=()
  fi
  (( offset += count ))
  (( offset <= $#fields )) || return 1
  count=$fields[offset]
  (( offset++ ))
  if (( count )); then
    _shell_sense_events=( "${(@)fields[offset,$(( offset + count - 1 ))]}" )
  else
    _shell_sense_events=()
  fi
}

_shell_sense_capture_request() {
  emulate -L zsh
  setopt localoptions no_aliases
  (( $# >= 4 )) || return 1
  local request=$1 generation=$2 request_buffer=$3
  local -i request_cursor=$4
  _shell_sense_cursor_byte
  if [[ $request != $_shell_sense_active_request ||
        $generation != $_shell_sense_active_generation ||
        $request_buffer != "$BUFFER" || $request_cursor != $REPLY ]]; then
    _shell_sense_send zsh-capture-begin "$request" "$generation"
    _shell_sense_send capture-end "$request" "$generation"
    return 0
  fi

  # A capture widget runs synchronously in the live Zsh provider. Never start
  # it while ZLE already has user input waiting; the next edit will issue a
  # newer generation after those bytes have been processed.
  if (( PENDING > 0 || KEYS_QUEUED_COUNT > 0 )); then
    _shell_sense_send zsh-capture-begin "$request" "$generation"
    _shell_sense_send capture-end "$request" "$generation"
    return 0
  fi

  local original_buffer=$BUFFER
  local -i original_cursor=$CURSOR
  _shell_sense_fast_command_handled=0
  zle .shell-sense-fast-command-capture || true
  if (( ! _shell_sense_fast_command_handled )); then
    BUFFER=$original_buffer
    CURSOR=$original_cursor
    zle .shell-sense-zsh-capture
  fi
  BUFFER=$original_buffer
  CURSOR=$original_cursor

  # The completion function itself is synchronous, but the
  # expensive serialization phase is cancellable. If another key arrived
  # during generation, return an empty/stale capture immediately; processing
  # that key will issue the authoritative next generation.
  if (( PENDING > 0 || KEYS_QUEUED_COUNT > 0 )); then
    _shell_sense_send zsh-capture-begin "$request" "$generation"
    _shell_sense_send capture-end "$request" "$generation"
    return 0
  fi

  _shell_sense_send_native_context "$request" "$generation" || return 1

  if (( _shell_sense_fast_command_handled )); then
    _shell_sense_send_command_candidates "$request" "$generation" "$request_cursor"
    return
  fi

  # A command-name request can contain hundreds of candidates. Encode a
  # bounded group and write it as one stream chunk instead of blocking ZLE on
  # one syscall per candidate. The wire format remains a sequence of ordinary
  # messages, so batching is transparent to the worker and bounded in memory.
  local -a wire_messages=()
  local -i wire_batch_size=64
  _shell_sense_encode_message zsh-capture-begin "$request" "$generation"
  wire_messages+=( "$REPLY" )
  local -i index prefix_bytes suffix_bytes start end flags
  local word display description group explanation kind identity resource_path
  for (( index = 1; index <= $#_shell_sense_capture_words; index++ )); do
    (( index > 1 && index % 16 == 1 && ( PENDING > 0 || KEYS_QUEUED_COUNT > 0 ) )) && break
    word=$_shell_sense_capture_words[index]
    display=$_shell_sense_capture_displays[index]
    description=$_shell_sense_capture_descriptions[index]
    group=$_shell_sense_capture_groups[index]
    explanation=$_shell_sense_capture_explanations[index]
    identity=$index
    flags=0
    [[ $_shell_sense_capture_flags[index] == *f* ]] && (( flags |= 1 ))
    kind=${_shell_sense_capture_kinds[index]:-}
    resource_path=${_shell_sense_capture_resource_paths[index]:-}
    if [[ -n $kind ]]; then
      :
    elif [[ $word == */ ]]; then
      kind=directory
      (( flags |= 2 ))
    elif [[ $word == -* ]]; then
      kind=option
    elif (( flags & 1 )) || [[ $_shell_sense_capture_prefixes[index] == */* ]]; then
      kind=file
    else
      kind=text
    fi
    _shell_sense_byte_length "$_shell_sense_capture_prefixes[index]"
    prefix_bytes=$REPLY
    _shell_sense_byte_length "$_shell_sense_capture_suffixes[index]"
    suffix_bytes=$REPLY
    (( start = request_cursor - prefix_bytes, start < 0 )) && start=0
    (( end = request_cursor + suffix_bytes ))
    _shell_sense_encode_message zsh-candidate \
      "$request" "$generation" "$word" "$display" "$description" "$explanation" \
      "$group" "" "$_shell_sense_capture_calls[index]" "$start" "$end" "$kind" \
      "$flags" "$identity" "$(( index - 1 ))" \
      "$_shell_sense_capture_prefixes[index]" "$_shell_sense_capture_suffixes[index]" \
      "$_shell_sense_capture_iprefixes[index]" "$_shell_sense_capture_isuffixes[index]" \
      "" "" "" "" "" "" "" "$resource_path" 0 || return 1
    wire_messages+=( "$REPLY" )
    if (( $#wire_messages >= wire_batch_size )); then
      _shell_sense_write_messages "${(j::)wire_messages}" || return 1
      wire_messages=()
    fi
  done
  _shell_sense_encode_message capture-end "$request" "$generation"
  wire_messages+=( "$REPLY" )
  _shell_sense_write_messages "${(j::)wire_messages}"
}

_shell_sense_view_begin() {
  emulate -L zsh
  (( $# >= 17 )) || return 1
  [[ $2 == $_shell_sense_active_request && $3 == $_shell_sense_active_generation ]] || return 0
  [[ $15 == <-> && ( $16 == replace || $16 == preserve ) && $17 == <-> ]] || return 1
  if (( $15 < _shell_sense_navigation_serial )); then
    _shell_sense_view_building=0
    return 0
  fi
  _shell_sense_view_building=1
  _shell_sense_temp_view_revision=$4
  _shell_sense_temp_navigation_serial=$15
  _shell_sense_temp_menu_width=0
  _shell_sense_temp_selected=${5:-0}
  (( _shell_sense_temp_selected++ ))
  [[ $9 == <-> ]] || return 1
  _shell_sense_temp_expected=$9
  [[ ${10} == <-> && ${11} == <-> && ${12} == <-> &&
     ${13} == <-> && ${14} == <-> ]] || return 1
  _shell_sense_temp_total=${10}
  _shell_sense_temp_window_start=${11}
  _shell_sense_temp_selected_absolute=${12}
  _shell_sense_temp_max_label_cells=${13}
  _shell_sense_temp_max_described_cells=${14}
  _shell_sense_temp_received=0
  _shell_sense_temp_ids=()
  _shell_sense_temp_labels=()
  _shell_sense_temp_label_cells=()
  _shell_sense_temp_details=()
  _shell_sense_temp_detail_cells=()
  _shell_sense_temp_kinds=()
  _shell_sense_temp_icons=()
  _shell_sense_temp_match_ranges=()
  _shell_sense_temp_groups=()
  _shell_sense_temp_insertions=()
  _shell_sense_temp_acceptance_sources=()
  _shell_sense_temp_acceptance_identities=()
  _shell_sense_temp_ghosts=()
  if [[ $16 == preserve ]]; then
    _shell_sense_preserve_temp_documentation
  else
    _shell_sense_reset_temp_documentation
  fi
  if (( _shell_sense_temp_expected )); then
    _shell_sense_temp_ids[_shell_sense_temp_expected]=
    _shell_sense_temp_labels[_shell_sense_temp_expected]=
    _shell_sense_temp_label_cells[_shell_sense_temp_expected]=0
    _shell_sense_temp_details[_shell_sense_temp_expected]=
    _shell_sense_temp_detail_cells[_shell_sense_temp_expected]=0
    _shell_sense_temp_kinds[_shell_sense_temp_expected]=
    _shell_sense_temp_icons[_shell_sense_temp_expected]=
    _shell_sense_temp_match_ranges[_shell_sense_temp_expected]=
    _shell_sense_temp_groups[_shell_sense_temp_expected]=
    _shell_sense_temp_insertions[_shell_sense_temp_expected]=
    _shell_sense_temp_acceptance_sources[_shell_sense_temp_expected]=
    _shell_sense_temp_acceptance_identities[_shell_sense_temp_expected]=
    _shell_sense_temp_ghosts[_shell_sense_temp_expected]=
  fi
}

_shell_sense_view_chunk() {
  emulate -L zsh
  local -a fields=( "$@" )
  (( $#fields >= 3 )) || return 1
  (( _shell_sense_view_building )) || return 0
  [[ $fields[1] == $_shell_sense_active_request && $fields[2] == $_shell_sense_active_generation ]] || return 0
  [[ $fields[3] == <-> ]] || return 1
  local -i count=$fields[3]
  (( $#fields == 3 + count * 12 )) || return 1
  (( _shell_sense_temp_received + count <= _shell_sense_temp_expected )) || return 1

  local -i index item offset=4
  for (( index = 1; index <= count; index++, offset += 12 )); do
    (( item = ++_shell_sense_temp_received ))
    _shell_sense_temp_ids[item]=$fields[offset]
    _shell_sense_temp_labels[item]=$fields[$(( offset + 1 ))]
    [[ $fields[$(( offset + 2 ))] == <-> ]] || return 1
    _shell_sense_temp_label_cells[item]=$fields[$(( offset + 2 ))]
    _shell_sense_temp_kinds[item]=$fields[$(( offset + 3 ))]
    _shell_sense_temp_icons[item]=$fields[$(( offset + 4 ))]
    _shell_sense_temp_details[item]=$fields[$(( offset + 5 ))]
    [[ $fields[$(( offset + 6 ))] == <-> ]] || return 1
    _shell_sense_temp_detail_cells[item]=$fields[$(( offset + 6 ))]
    _shell_sense_temp_groups[item]=$fields[$(( offset + 7 ))]
    _shell_sense_temp_insertions[item]=
    _shell_sense_temp_acceptance_sources[item]=$fields[$(( offset + 8 ))]
    _shell_sense_temp_acceptance_identities[item]=$fields[$(( offset + 9 ))]
    _shell_sense_temp_match_ranges[item]=$fields[$(( offset + 10 ))]
    _shell_sense_temp_ghosts[item]=$fields[$(( offset + 11 ))]
  done
}

_shell_sense_view_end() {
  emulate -L zsh
  (( $# >= 3 )) || return 1
  (( _shell_sense_view_building )) || return 0
  [[ $1 == $_shell_sense_active_request && $2 == $_shell_sense_active_generation ]] || return 0
  [[ $3 == $_shell_sense_temp_view_revision ]] || return 0
  (( _shell_sense_temp_navigation_serial >= _shell_sense_navigation_serial )) || return 0
  (( _shell_sense_temp_received == _shell_sense_temp_expected )) || return 1
  _shell_sense_item_ids=( "${_shell_sense_temp_ids[@]}" )
  _shell_sense_item_labels=( "${_shell_sense_temp_labels[@]}" )
  _shell_sense_item_label_cells=( "${_shell_sense_temp_label_cells[@]}" )
  _shell_sense_item_details=( "${_shell_sense_temp_details[@]}" )
  _shell_sense_item_detail_cells=( "${_shell_sense_temp_detail_cells[@]}" )
  _shell_sense_item_kinds=( "${_shell_sense_temp_kinds[@]}" )
  _shell_sense_item_icons=( "${_shell_sense_temp_icons[@]}" )
  _shell_sense_item_match_ranges=( "${_shell_sense_temp_match_ranges[@]}" )
  _shell_sense_item_groups=( "${_shell_sense_temp_groups[@]}" )
  _shell_sense_item_insertions=( "${_shell_sense_temp_insertions[@]}" )
  _shell_sense_item_acceptance_sources=( "${_shell_sense_temp_acceptance_sources[@]}" )
  _shell_sense_item_acceptance_identities=( "${_shell_sense_temp_acceptance_identities[@]}" )
  _shell_sense_item_ghosts=( "${_shell_sense_temp_ghosts[@]}" )
  _shell_sense_view_total=$_shell_sense_temp_total
  _shell_sense_view_window_start=$_shell_sense_temp_window_start
  _shell_sense_selected_absolute=$_shell_sense_temp_selected_absolute
  _shell_sense_max_label_cells=$_shell_sense_temp_max_label_cells
  _shell_sense_max_described_cells=$_shell_sense_temp_max_described_cells
  _shell_sense_view_revision=$_shell_sense_temp_view_revision
  _shell_sense_navigation_serial=$_shell_sense_temp_navigation_serial
  _shell_sense_menu_width=$_shell_sense_temp_menu_width
  _shell_sense_commit_documentation
  _shell_sense_view_building=0
  _shell_sense_render_dirty=1
  _shell_sense_popup_stale=0
  _shell_sense_continuity_ghost=
  _shell_sense_selected=$_shell_sense_temp_selected
  (( _shell_sense_selected < 1 )) && _shell_sense_selected=1
  (( _shell_sense_selected > $#_shell_sense_item_ids )) && _shell_sense_selected=$#_shell_sense_item_ids
  if (( _shell_sense_menu_view_request != $1 ||
        _shell_sense_menu_view_generation != $2 )); then
    _shell_sense_menu_view_start=0
    _shell_sense_menu_view_request=$1
    _shell_sense_menu_view_generation=$2
  fi
  _shell_sense_update_menu_viewport $_shell_sense_selected_absolute
  if (( $#_shell_sense_item_ids && _shell_sense_popup_enabled &&
        ! _shell_sense_external_presentation )); then
    _shell_sense_popup_visible=1
  else
    _shell_sense_clear_popup
  fi
  _shell_sense_render
}

_shell_sense_selection_changed() {
  emulate -L zsh
  (( $# == 6 )) || return 1
  [[ $1 == $_shell_sense_active_request && $2 == $_shell_sense_active_generation &&
     $3 == $_shell_sense_view_revision && $4 == <-> && $5 == <-> && $6 == <-> ]] || return 0
  (( $4 >= _shell_sense_navigation_serial )) || return 0
  local -i selected=$(( $5 + 1 ))
  (( selected >= 1 && selected <= $#_shell_sense_item_ids )) || return 0
  if (( $4 == _shell_sense_navigation_serial &&
        selected == _shell_sense_selected &&
        $6 == _shell_sense_selected_absolute )); then
    return 0
  fi
  _shell_sense_navigation_serial=$4
  _shell_sense_selected=$selected
  _shell_sense_selected_absolute=$6
  _shell_sense_update_menu_viewport $_shell_sense_selected_absolute
  _shell_sense_render_dirty=1
  _shell_sense_render
}

_shell_sense_reset_temp_documentation() {
  _shell_sense_temp_documentation_item=
  _shell_sense_temp_documentation_placement=
  _shell_sense_temp_documentation_width=0
  _shell_sense_temp_documentation_expected=0
  _shell_sense_temp_documentation_received=0
  _shell_sense_temp_documentation_viewport_rows=0
  _shell_sense_temp_documentation_offset=0
  _shell_sense_temp_documentation_total=0
  _shell_sense_temp_documentation_scrollbar=0
  _shell_sense_temp_documentation_kinds=()
  _shell_sense_temp_documentation_cells=()
  _shell_sense_temp_documentation_lines=()
}

_shell_sense_preserve_temp_documentation() {
  _shell_sense_temp_documentation_item=$_shell_sense_documentation_item
  _shell_sense_temp_documentation_placement=$_shell_sense_documentation_placement
  _shell_sense_temp_documentation_width=$_shell_sense_documentation_width
  _shell_sense_temp_documentation_expected=$#_shell_sense_documentation_lines
  _shell_sense_temp_documentation_received=$#_shell_sense_documentation_lines
  _shell_sense_temp_documentation_viewport_rows=$_shell_sense_documentation_viewport_rows
  _shell_sense_temp_documentation_offset=$_shell_sense_documentation_offset
  _shell_sense_temp_documentation_total=$_shell_sense_documentation_total
  _shell_sense_temp_documentation_scrollbar=$_shell_sense_documentation_scrollbar
  _shell_sense_temp_documentation_kinds=( "${_shell_sense_documentation_kinds[@]}" )
  _shell_sense_temp_documentation_cells=( "${_shell_sense_documentation_cells[@]}" )
  _shell_sense_temp_documentation_lines=( "${_shell_sense_documentation_lines[@]}" )
}

_shell_sense_commit_documentation() {
  _shell_sense_documentation_item=$_shell_sense_temp_documentation_item
  _shell_sense_documentation_placement=$_shell_sense_temp_documentation_placement
  _shell_sense_documentation_width=$_shell_sense_temp_documentation_width
  _shell_sense_documentation_viewport_rows=$_shell_sense_temp_documentation_viewport_rows
  _shell_sense_documentation_offset=$_shell_sense_temp_documentation_offset
  _shell_sense_documentation_total=$_shell_sense_temp_documentation_total
  _shell_sense_documentation_scrollbar=$_shell_sense_temp_documentation_scrollbar
  _shell_sense_documentation_kinds=( "${_shell_sense_temp_documentation_kinds[@]}" )
  _shell_sense_documentation_cells=( "${_shell_sense_temp_documentation_cells[@]}" )
  _shell_sense_documentation_lines=( "${_shell_sense_temp_documentation_lines[@]}" )
}

_shell_sense_view_layout() {
  emulate -L zsh
  (( $# == 4 )) || return 1
  [[ $1 == $_shell_sense_active_request && $2 == $_shell_sense_active_generation &&
     $3 == <-> && $4 == <-> ]] || return 0
  if (( _shell_sense_view_building )) && [[ $3 == $_shell_sense_temp_view_revision ]]; then
    _shell_sense_temp_menu_width=$4
  elif (( ! _shell_sense_view_building )) && [[ $3 == $_shell_sense_view_revision ]]; then
    _shell_sense_menu_width=$4
  fi
}

_shell_sense_documentation_begin() {
  emulate -L zsh
  (( $# == 10 )) || return 1
  [[ $1 == $_shell_sense_active_request && $2 == $_shell_sense_active_generation ]] || return 0
  [[ $4 == side || $4 == below ]] || return 1
  [[ $5 == <-> && $6 == <-> && $7 == <-> && $8 == <-> &&
     $9 == <-> && ${10} == [01] ]] || return 1
  _shell_sense_reset_temp_documentation
  _shell_sense_temp_documentation_item=$3
  _shell_sense_temp_documentation_placement=$4
  _shell_sense_temp_documentation_width=$5
  _shell_sense_temp_documentation_expected=$6
  _shell_sense_temp_documentation_viewport_rows=$7
  _shell_sense_temp_documentation_offset=$8
  _shell_sense_temp_documentation_total=$9
  _shell_sense_temp_documentation_scrollbar=${10}
}

_shell_sense_documentation_chunk() {
  emulate -L zsh
  local -a fields=( "$@" )
  (( $#fields >= 4 )) || return 1
  [[ $fields[1] == $_shell_sense_active_request &&
     $fields[2] == $_shell_sense_active_generation &&
     $fields[3] == $_shell_sense_temp_documentation_item ]] || return 0
  [[ $fields[4] == <-> ]] || return 1
  local -i count=$fields[4] offset=5 index item
  (( $#fields == 4 + count * 3 )) || return 1
  (( _shell_sense_temp_documentation_received + count <=
     _shell_sense_temp_documentation_expected )) || return 1
  for (( index = 1; index <= count; index++, offset += 3 )); do
    [[ $fields[$(( offset + 1 ))] == <-> ]] || return 1
    (( item = ++_shell_sense_temp_documentation_received ))
    _shell_sense_temp_documentation_kinds[item]=$fields[offset]
    _shell_sense_temp_documentation_cells[item]=$fields[$(( offset + 1 ))]
    _shell_sense_temp_documentation_lines[item]=$fields[$(( offset + 2 ))]
  done
}

_shell_sense_documentation_end() {
  emulate -L zsh
  (( $# == 3 )) || return 1
  [[ $1 == $_shell_sense_active_request && $2 == $_shell_sense_active_generation &&
     $3 == $_shell_sense_temp_documentation_item ]] || return 0
  (( _shell_sense_temp_documentation_received ==
     _shell_sense_temp_documentation_expected )) || return 1
  if (( ! _shell_sense_view_building )); then
    _shell_sense_commit_documentation
    _shell_sense_render_dirty=1
    _shell_sense_render
  fi
}

_shell_sense_documentation_clear() {
  emulate -L zsh
  (( $# == 2 )) || return 1
  [[ $1 == $_shell_sense_active_request && $2 == $_shell_sense_active_generation ]] || return 0
  _shell_sense_reset_temp_documentation
  if (( ! _shell_sense_view_building )); then
    _shell_sense_commit_documentation
    _shell_sense_render_dirty=1
    _shell_sense_render
  fi
}

_shell_sense_accept_zsh() {
  emulate -L zsh
  (( $# >= 19 )) || return 1
  local request=$1 generation=$2 item_id=$3 identity=$5
  local -i applied=0
  if [[ $request == $_shell_sense_active_request &&
        $generation == $_shell_sense_active_generation &&
        $identity == <-> ]]; then
    _shell_sense_apply_serial=$_shell_sense_capture_serial
    _shell_sense_apply_index=$identity
    zle .shell-sense-zsh-apply
    if (( $? == 0 )); then
      applied=1
      _shell_sense_clear_popup
      _shell_sense_last_buffer=$BUFFER
      _shell_sense_last_cursor=$CURSOR
    fi
  fi
  _shell_sense_send selection-finished "$request" "$generation" "$item_id" "$applied"
  if (( applied && _shell_sense_after_accept )); then
    _shell_sense_request after-accept
  fi
}

_shell_sense_request() {
  emulate -L zsh
  setopt localoptions no_aliases
  (( _shell_sense_ready && _shell_sense_configured )) || return 1
  [[ $_shell_sense_activation_mode != disabled ]] || return 1
  [[ -n $BUFFER ]] || { _shell_sense_clear_popup; return 0; }
  local trigger=${1:-automatic}
  if (( _shell_sense_active_request )); then
    _shell_sense_send cancel "$_shell_sense_active_request" "$_shell_sense_active_generation"
  fi
  _shell_sense_rebase_continuity_ghost
  (( _shell_sense_request_serial++, _shell_sense_generation++ ))
  _shell_sense_active_request=$_shell_sense_request_serial
  _shell_sense_active_generation=$_shell_sense_generation
  _shell_sense_navigation_serial=0
  _shell_sense_temp_navigation_serial=0
  _shell_sense_active_buffer=$BUFFER
  _shell_sense_active_cursor=$CURSOR
  _shell_sense_cursor_byte
  _shell_sense_active_cursor_byte=$REPLY
  _shell_sense_last_buffer=$BUFFER
  _shell_sense_last_cursor=$CURSOR
  # Keep the last complete frame on-screen while the next generation is
  # debounced, captured, and ranked. Its ghost is locally rebased against the
  # edit above, following Blink's behavior of redrawing the current preview
  # against the new line instead of clearing it before sources answer. The
  # stale frame remains presentation-only: it cannot be navigated or accepted.
  (( _shell_sense_popup_visible )) && _shell_sense_popup_stale=1
  _shell_sense_send complete \
    "$_shell_sense_active_request" "$_shell_sense_active_generation" "" "$BUFFER" \
    "$_shell_sense_active_cursor_byte" "$PWD" "${KEYMAP:-main}" \
    "${COLUMNS:-80}" "${LINES:-24}" "$trigger" 0 || _shell_sense_clear_popup
}

_shell_sense_event_for_widget() {
  emulate -L zsh
  case ${LASTWIDGET:-} in
    *backward-delete*) REPLY=backspace ;;
    *kill-word*|*delete-word*) REPLY=word-delete ;;
    *delete*) REPLY=delete ;;
    *paste*) REPLY=paste ;;
    *history*|up-line-or-history|down-line-or-history) REPLY=history ;;
    *forward-char*|*backward-char*|*beginning-of-line*|*end-of-line*) REPLY=cursor ;;
    *)
      if [[ $BUFFER != $_shell_sense_last_buffer ]]; then REPLY=insert; else REPLY=cursor; fi
      ;;
  esac
}

_shell_sense_line_pre_redraw() {
  emulate -L zsh
  (( _shell_sense_configured )) || return 0
  local -i changed=0
  [[ $BUFFER != $_shell_sense_last_buffer || $CURSOR != $_shell_sense_last_cursor ]] && changed=1
  local event=
  if (( changed )); then
    # Determine the event before rendering so LASTWIDGET still describes the
    # edit that requested this redraw.
    _shell_sense_event_for_widget
    event=$REPLY
  fi
  if (( changed )); then
    _shell_sense_last_buffer=$BUFFER
    _shell_sense_last_cursor=$CURSOR
    if (( ${_shell_sense_events[(Ie)$event]} )); then
      # Apply lifecycle policy before rendering. In manual/hybrid modes this
      # prevents one stale frame from being redrawn after an edit has already
      # invalidated it; in continuous mode `_request` deliberately retains
      # the last complete frame until its replacement arrives.
      case $_shell_sense_activation_mode in
        continuous)
          local char=${LBUFFER[-1]-}
          if [[ -n $char ]] && (( ${_shell_sense_immediate_characters[(Ie)$char]} )); then
            _shell_sense_request trigger-character
          else
            _shell_sense_request automatic
          fi
          ;;
        hybrid)
          local char=${LBUFFER[-1]-}
          if [[ -n $char ]] && (( ${_shell_sense_trigger_characters[(Ie)$char]} )); then
            _shell_sense_request trigger-character
          else
            _shell_sense_clear_popup
          fi
          ;;
        manual|disabled)
          _shell_sense_clear_popup
          ;;
      esac
    fi
  fi
  if (( _shell_sense_popup_visible )); then
    if (( (_shell_sense_render_dirty || changed) &&
          (${PENDING:-0} != 0 || ${KEYS_QUEUED_COUNT:-0} != 0) )); then
      # Key-repeat may keep input queued across many widgets. Defer expensive
      # panel construction to one event-loop callback, but retain the complete
      # cached frame on every native redraw. Other line-pre-redraw integrations
      # rebuild `region_highlight`; failing to reinstall our cached highlights
      # would briefly expose an unselected frame before the coalesced update.
      if _shell_sense_schedule_redraw; then
        _shell_sense_retain_rendered_frame || _shell_sense_render 0
        _shell_sense_begin_synchronized_redraw || true
      else
        _shell_sense_render 0
        _shell_sense_begin_synchronized_redraw || true
      fi
      return 0
    fi
    if (( _shell_sense_render_dirty || changed )); then
      _shell_sense_redraw_pending=0
      # Build the new POSTDISPLAY before ZLE performs the redraw that already
      # follows this hook. A self-pipe callback releases synchronized output
      # only after that native redraw has returned to ZLE's event loop. Calling
      # `zle -R` here would create a second, transient popup frame.
      _shell_sense_render 0
      _shell_sense_begin_synchronized_redraw || true
    else
      _shell_sense_render 0
    fi
  fi
}

_shell_sense_line_init() {
  emulate -L zsh
  _shell_sense_setup_synchronized_redraw
  _shell_sense_ensure_worker
  # ZLE resets POSTDISPLAY for every new editing session. Forget the previous
  # ownership token before clearing plugin state so content installed by
  # another line-init hook is never mistaken for ours.
  _shell_sense_owned_postdisplay=
  _shell_sense_clear_popup 0
  _shell_sense_last_buffer=$BUFFER
  _shell_sense_last_cursor=$CURSOR
}

_shell_sense_line_finish() {
  emulate -L zsh
  # POSTDISPLAY is part of ZLE's editable display. Remove the panel before ZLE
  # commits the accepted line to terminal scrollback. This is also a fallback
  # for custom accept-line widgets that do not use the configured Enter key.
  _shell_sense_prepare_line_finish
}

_shell_sense_prepare_line_finish() {
  emulate -L zsh
  if (( _shell_sense_active_request )); then
    _shell_sense_send cancel \
      "$_shell_sense_active_request" "$_shell_sense_active_generation"
  fi
  _shell_sense_active_request=0
  _shell_sense_active_buffer=
  _shell_sense_clear_popup 0
  # Never leave a terminal buffering synchronized output when ZLE exits before
  # the self-pipe callback gets another event-loop turn.
  _shell_sense_end_synchronized_redraw
}

_shell_sense_erase_edit_display() {
  emulate -L zsh
  local -i panel_lines=$#_shell_sense_render_lines
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

_shell_sense_remove_highlights() {
  emulate -L zsh
  (( ${+region_highlight} )) || return 0
  region_highlight=( ${region_highlight:#*memo=shell-sense} )
}

_shell_sense_end_synchronized_redraw() {
  emulate -L zsh
  (( _shell_sense_sync_active )) || return 0
  print -rn -- $'\e[?2026l'
  _shell_sense_sync_active=0
}

_shell_sense_synchronized_redraw_callback() {
  emulate -L zsh
  local -i fd=${1:--1}
  (( fd >= 0 && fd == _shell_sense_sync_fd )) || return 0

  # Coalesce every wakeup. State lives in the client; bytes merely return
  # control to ZLE's event loop after a native redraw or a burst of input.
  local chunk=
  while sysread -i $fd -s 64 -t 0 chunk 2>/dev/null; do
    [[ -n $chunk ]] || break
    chunk=
  done
  if (( _shell_sense_redraw_pending )); then
    _shell_sense_redraw_pending=0
    if (( _shell_sense_popup_visible && _shell_sense_render_dirty )); then
      # If a preceding native redraw is still inside the same transaction,
      # include this newest frame before releasing it. A render can itself be
      # followed by ZLE's normal post-callback refresh, so a fresh wakeup—not
      # this callback—must release the completed event-loop turn.
      _shell_sense_render
      _shell_sense_defer_synchronized_redraw_end || true
      return 0
    fi
  fi
  _shell_sense_end_synchronized_redraw
}

_shell_sense_defer_synchronized_redraw_end() {
  emulate -L zsh
  (( _shell_sense_sync_active && _shell_sense_sync_fd >= 0 )) || return 1
  if ! syswrite -o $_shell_sense_sync_fd x 2>/dev/null; then
    _shell_sense_end_synchronized_redraw
    return 1
  fi
}

_shell_sense_schedule_redraw() {
  emulate -L zsh
  (( _shell_sense_sync_fd >= 0 )) || return 1
  (( _shell_sense_redraw_pending )) && return 0
  _shell_sense_redraw_pending=1
  if ! syswrite -o $_shell_sense_sync_fd r 2>/dev/null; then
    _shell_sense_redraw_pending=0
    return 1
  fi
}

_shell_sense_begin_synchronized_redraw() {
  emulate -L zsh
  (( _shell_sense_sync_fd >= 0 )) || return 1
  (( _shell_sense_sync_active )) && return 0

  print -rn -- $'\e[?2026h'
  _shell_sense_sync_active=1
  _shell_sense_defer_synchronized_redraw_end
}

_shell_sense_runtime_directory() {
  emulate -L zsh
  local runtime_base=${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}/shell-sense-$UID}
  local runtime_dir=$runtime_base/shell-sense
  command mkdir -p -m 700 -- "$runtime_dir" 2>/dev/null || return 1
  command chmod 700 -- "$runtime_dir" 2>/dev/null || return 1
  [[ -d $runtime_dir && -O $runtime_dir && ! -L $runtime_dir ]] || return 1
  REPLY=$runtime_dir
}

_shell_sense_setup_synchronized_redraw() {
  emulate -L zsh
  (( _shell_sense_sync_fd >= 0 )) && return 0

  _shell_sense_runtime_directory || return 1
  local runtime_dir=$REPLY
  _shell_sense_sync_fifo="$runtime_dir/sync-${sysparams[pid]}-${RANDOM}-${RANDOM}"
  command mkfifo -m 600 -- "$_shell_sense_sync_fifo" 2>/dev/null || {
    _shell_sense_sync_fifo=
    return 1
  }
  if ! sysopen -rw -o cloexec,nonblock -u _shell_sense_sync_fd \
      "$_shell_sense_sync_fifo" 2>/dev/null; then
    command unlink -- "$_shell_sense_sync_fifo" 2>/dev/null
    _shell_sense_sync_fifo=
    _shell_sense_sync_fd=-1
    return 1
  fi
  command unlink -- "$_shell_sense_sync_fifo" 2>/dev/null
  _shell_sense_sync_fifo=

  zle -N .shell-sense-sync-callback _shell_sense_synchronized_redraw_callback
  if ! zle -Fw $_shell_sense_sync_fd .shell-sense-sync-callback 2>/dev/null; then
    exec {_shell_sense_sync_fd}>&- 2>/dev/null
    _shell_sense_sync_fd=-1
    return 1
  fi
}

_shell_sense_teardown_synchronized_redraw() {
  emulate -L zsh
  _shell_sense_redraw_pending=0
  _shell_sense_end_synchronized_redraw
  if (( _shell_sense_sync_fd >= 0 )); then
    zle -F $_shell_sense_sync_fd 2>/dev/null
    exec {_shell_sense_sync_fd}>&- 2>/dev/null
  fi
  _shell_sense_sync_fd=-1
  [[ -z $_shell_sense_sync_fifo ]] ||
    command unlink -- "$_shell_sense_sync_fifo" 2>/dev/null
  _shell_sense_sync_fifo=
}

_shell_sense_redisplay() {
  emulate -L zsh
  # DEC private mode 2026 is ignored by terminals that do not implement
  # synchronized output. Supporting terminals buffer every redraw in this ZLE
  # event-loop turn until the self-pipe callback releases the transaction.
  # `zle -R` can be followed by ZLE's normal post-callback refresh; closing the
  # transaction here would expose those two passes as separate visual frames.
  if (( ! _shell_sense_sync_active )); then
    _shell_sense_begin_synchronized_redraw || true
  fi
  zle -R
  local -i redisplay_status=$?
  # A failed refresh may occur while ZLE is leaving. There may be no further
  # event-loop turn in which the callback can run, so release the terminal now.
  (( redisplay_status == 0 )) || _shell_sense_end_synchronized_redraw
  return $redisplay_status
}

_shell_sense_remove_postdisplay() {
  emulate -L zsh
  _shell_sense_remove_highlights
  [[ -n $_shell_sense_owned_postdisplay ]] || return 0
  if [[ $POSTDISPLAY == *"$_shell_sense_owned_postdisplay" ]]; then
    local -i base_length=$(( $#POSTDISPLAY - $#_shell_sense_owned_postdisplay ))
    if (( base_length > 0 )); then
      POSTDISPLAY=$POSTDISPLAY[1,base_length]
    else
      POSTDISPLAY=
    fi
  fi
  _shell_sense_owned_postdisplay=
}

_shell_sense_set_postdisplay() {
  emulate -L zsh
  local panel=$1 ghost=${2-} separator=$'\n'
  _shell_sense_remove_postdisplay
  [[ -n $panel ]] || return 0
  # Respect an existing display-only continuation (for example an explicitly
  # enabled zsh-autosuggestions instance). The popup composes below it, but two
  # competing inline continuations must never be concatenated.
  [[ -n $POSTDISPLAY ]] && ghost=
  [[ -z $ghost && -n $POSTDISPLAY && $POSTDISPLAY[-1] == $'\n' ]] && separator=
  local -i ghost_start=$(( $#BUFFER + $#POSTDISPLAY ))
  local -i highlight_base=$(( ghost_start + $#ghost + $#separator ))
  _shell_sense_owned_postdisplay="$ghost$separator$panel"
  POSTDISPLAY+=$_shell_sense_owned_postdisplay
  local -i index start end
  if [[ -n $ghost ]]; then
    region_highlight+=(
      "$ghost_start $(( ghost_start + $#ghost )) $_shell_sense_style_ghost memo=shell-sense"
    )
  fi
  for (( index = 1; index <= $#_shell_sense_render_highlight_starts; index++ )); do
    start=$_shell_sense_render_highlight_starts[index]
    end=$_shell_sense_render_highlight_ends[index]
    region_highlight+=(
      "$(( highlight_base + start )) $(( highlight_base + end )) $_shell_sense_render_highlight_styles[index] memo=shell-sense"
    )
  done
}

_shell_sense_retain_rendered_frame() {
  emulate -L zsh
  (( _shell_sense_popup_visible && $#_shell_sense_render_lines )) || return 1
  _shell_sense_current_ghost
  _shell_sense_set_postdisplay "${(F)_shell_sense_render_lines}" "$REPLY"
}

_shell_sense_clear_popup() {
  local -i request_redisplay=${1:-1}
  _shell_sense_redraw_pending=0
  _shell_sense_remove_postdisplay
  _shell_sense_popup_visible=0
  _shell_sense_popup_stale=0
  _shell_sense_continuity_ghost=
  _shell_sense_selected=0
  _shell_sense_view_total=0
  _shell_sense_view_window_start=0
  _shell_sense_selected_absolute=0
  _shell_sense_menu_view_start=0
  _shell_sense_menu_view_request=0
  _shell_sense_menu_view_generation=0
  _shell_sense_max_label_cells=0
  _shell_sense_max_described_cells=0
  _shell_sense_menu_width=0
  _shell_sense_view_building=0
  _shell_sense_item_ids=()
  _shell_sense_item_labels=()
  _shell_sense_item_label_cells=()
  _shell_sense_item_details=()
  _shell_sense_item_detail_cells=()
  _shell_sense_item_kinds=()
  _shell_sense_item_icons=()
  _shell_sense_item_match_ranges=()
  _shell_sense_item_groups=()
  _shell_sense_item_insertions=()
  _shell_sense_item_acceptance_sources=()
  _shell_sense_item_acceptance_identities=()
  _shell_sense_item_ghosts=()
  _shell_sense_documentation_item=
  _shell_sense_documentation_placement=
  _shell_sense_documentation_width=0
  _shell_sense_documentation_viewport_rows=0
  _shell_sense_documentation_offset=0
  _shell_sense_documentation_total=0
  _shell_sense_documentation_scrollbar=0
  _shell_sense_documentation_kinds=()
  _shell_sense_documentation_cells=()
  _shell_sense_documentation_lines=()
  _shell_sense_reset_temp_documentation
  _shell_sense_render_dirty=1
  _shell_sense_render_columns=0
  _shell_sense_render_menu_lines=0
  _shell_sense_render_first=1
  _shell_sense_render_lines=()
  _shell_sense_render_highlight_starts=()
  _shell_sense_render_highlight_ends=()
  _shell_sense_render_highlight_styles=()
  if (( request_redisplay )) && zle >/dev/null 2>&1; then
    _shell_sense_redisplay 2>/dev/null
  fi
}

_shell_sense_hide_popup() {
  emulate -L zsh
  _shell_sense_remove_postdisplay
  _shell_sense_popup_visible=0
  if zle >/dev/null 2>&1; then
    _shell_sense_redisplay 2>/dev/null
  fi
}

_shell_sense_kind_indicator() {
  emulate -L zsh
  local kind=$1 icon=$2
  _shell_sense_indicator_cells=0
  if [[ $_shell_sense_indicator_mode == none ]]; then
    REPLY=
    return
  fi
  case $_shell_sense_indicator_mode in
    icon)
      REPLY=$icon
      [[ -n $icon ]] && _shell_sense_indicator_cells=1
      ;;
    text)
      REPLY="[${kind[1]}]"
      _shell_sense_indicator_cells=3
      ;;
    both)
      REPLY="$icon [${kind[1]}]"
      _shell_sense_indicator_cells=5
      ;;
    *)
      REPLY=
      ;;
  esac
}

_shell_sense_truncate() {
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

_shell_sense_menu_viewport_start_for() {
  emulate -L zsh
  local -i selected=$1 start=$2 total=$_shell_sense_view_total
  local -i rows=$_shell_sense_max_rows scrolloff=$_shell_sense_scrolloff maximum_start=0
  (( rows > total )) && rows=$total
  if (( rows <= 0 )); then
    REPLY=0
    return 0
  fi
  (( scrolloff >= rows )) && scrolloff=$(( rows - 1 ))
  (( maximum_start = total - rows ))
  (( start < 0 )) && start=0
  (( start > maximum_start )) && start=$maximum_start
  (( selected < 0 )) && selected=0
  (( selected >= total )) && selected=$(( total - 1 ))

  # Match an editor window's persistent `scrolloff` behavior. The viewport
  # stays fixed while the selection moves inside its margins; it advances only
  # when the selection would cross one of them. Recomputing the start solely
  # from the selected index made reverse navigation scroll on every key.
  if (( selected < start + scrolloff )); then
    start=$(( selected - scrolloff ))
  elif (( selected >= start + rows - scrolloff )); then
    start=$(( selected - rows + scrolloff + 1 ))
  fi
  (( start < 0 )) && start=0
  (( start > maximum_start )) && start=$maximum_start
  REPLY=$start
}

_shell_sense_update_menu_viewport() {
  _shell_sense_menu_viewport_start_for "$1" "$_shell_sense_menu_view_start"
  _shell_sense_menu_view_start=$REPLY
}

_shell_sense_cached_menu_viewport_contains() {
  emulate -L zsh
  local -i start=$1 rows=$_shell_sense_max_rows
  (( rows > _shell_sense_view_total )) && rows=$_shell_sense_view_total
  local -i cached_end=$(( _shell_sense_view_window_start + $#_shell_sense_item_ids ))
  (( start >= _shell_sense_view_window_start && start + rows <= cached_end ))
}

_shell_sense_viewport_scrollbar_geometry() {
  emulate -L zsh
  local -i rows=$1 total=$2 offset=$3
  local -i thumb_rows=0 thumb_first=0 track=0 maximum_offset=0
  if (( rows > 0 && total > rows )); then
    (( thumb_rows = (rows * rows) / total ))
    (( thumb_rows < 1 )) && thumb_rows=1
    (( thumb_rows > rows )) && thumb_rows=$rows
    (( maximum_offset = total - rows ))
    (( offset < 0 )) && offset=0
    (( offset > maximum_offset )) && offset=$maximum_offset
    (( track = rows - thumb_rows ))
    if (( track > 0 && maximum_offset > 0 )); then
      (( thumb_first = (offset * track + maximum_offset / 2) / maximum_offset ))
    fi
  fi
  REPLY="$thumb_rows:$thumb_first"
}

_shell_sense_current_ghost() {
  emulate -L zsh
  REPLY=
  (( _shell_sense_ghost_enabled && _shell_sense_popup_visible &&
     _shell_sense_selected >= 1 &&
     _shell_sense_selected <= $#_shell_sense_item_ghosts )) || return 0
  # POSTDISPLAY follows the complete editable buffer. Rendering a suffix in
  # the middle of BUFFER would require mutating ZLE state, so the ZLE UI
  # intentionally limits completion-derived ghost text to end-of-line.
  (( CURSOR == $#BUFFER )) || return 0
  if (( _shell_sense_popup_stale )); then
    REPLY=$_shell_sense_continuity_ghost
    return 0
  fi
  REPLY=$_shell_sense_item_ghosts[_shell_sense_selected]
}

_shell_sense_rebase_continuity_ghost() {
  emulate -L zsh
  local ghost prediction
  _shell_sense_continuity_ghost=
  (( _shell_sense_popup_visible && _shell_sense_selected >= 1 &&
     _shell_sense_active_cursor == $#_shell_sense_active_buffer &&
     CURSOR == $#BUFFER )) || return 0
  _shell_sense_current_ghost
  ghost=$REPLY
  [[ -n $ghost ]] || return 0
  prediction="$_shell_sense_active_buffer$ghost"
  (( $#BUFFER <= $#prediction )) || return 0
  [[ ${prediction[1,$#BUFFER]} == "$BUFFER" ]] || return 0
  if (( $#BUFFER < $#prediction )); then
    _shell_sense_continuity_ghost=${prediction[$(( $#BUFFER + 1 )),-1]}
  fi
}

_shell_sense_ghost_chunk() {
  emulate -L zsh
  setopt localoptions extendedglob
  local ghost=$1 mode=$2 chunk=

  case $mode in
    token) chunk=$ghost ;;
    word)
      chunk=${ghost%%[^[:alnum:]_]*}
      # Crossing one punctuation boundary at a time is more useful than doing
      # nothing for kebab-case and dotted candidates.
      [[ -n $chunk ]] || chunk=$ghost[1]
      ;;
    path-segment)
      if [[ $ghost == */* ]]; then
        chunk="${ghost%%/*}/"
      else
        chunk=$ghost
      fi
      ;;
    off|*) return 1 ;;
  esac

  [[ -n $chunk && $chunk == [[:alnum:]_.~+@%:=,/-]## ]] || return 1
  REPLY=$chunk
}

_shell_sense_accept_ghost_part() {
  emulate -L zsh
  local mode=$1 ghost chunk=
  _shell_sense_current_ghost
  ghost=$REPLY
  [[ -n $ghost ]] || return 1
  _shell_sense_ghost_chunk "$ghost" "$mode" || return 1
  chunk=$REPLY

  # If the whole suffix is accepted, delegate to Zsh so quoting and suffix
  # behavior remain authoritative. Direct edits are restricted by
  # `_shell_sense_ghost_chunk` to literal-safe characters.
  if [[ $chunk == $ghost ]]; then
    _shell_sense_accept_selected
    return
  fi
  LBUFFER+=$chunk
  _shell_sense_clear_popup
}

_shell_sense_render() {
  emulate -L zsh
  local -i request_redisplay=${1:-1}
  # Netstring parsing is byte-oriented and dynamically scopes LC_ALL=C into
  # dispatch handlers. Render in the interactive locale so ZLE receives real
  # multibyte characters instead of displaying them as `\M-...` byte escapes.
  local LC_ALL=$_shell_sense_ui_locale
  (( _shell_sense_popup_visible && $#_shell_sense_item_ids )) || return 0
  if (( ! _shell_sense_render_dirty &&
        _shell_sense_render_columns == COLUMNS &&
        $#_shell_sense_render_lines )); then
    _shell_sense_current_ghost
    _shell_sense_set_postdisplay "${(F)_shell_sense_render_lines}" "$REPLY"
    (( request_redisplay )) && _shell_sense_redisplay
    return 0
  fi
  local tl tr bl br horizontal vertical
  case $_shell_sense_border in
    sharp) tl=┌ tr=┐ bl=└ br=┘ horizontal=─ vertical=│ ;;
    ascii) tl=+ tr=+ bl=+ br=+ horizontal=- vertical='|' ;;
    none) tl= tr= bl= br= horizontal= vertical= ;;
    *) tl=╭ tr=╮ bl=╰ br=╯ horizontal=─ vertical=│ ;;
  esac
  local -i terminal_width=$(( COLUMNS > 0 ? COLUMNS : 80 ))
  local -i rows=$#_shell_sense_item_ids
  (( rows > _shell_sense_max_rows )) && rows=$_shell_sense_max_rows
  local -i scrollbar_active=0 scrollbar_cells=0
  if (( _shell_sense_show_scrollbar && _shell_sense_view_total > rows )); then
    scrollbar_active=1
    scrollbar_cells=1
  fi
  local -i marker_cells=$#_shell_sense_selected_marker marker_prefix_cells=0
  (( marker_cells )) && marker_prefix_cells=$(( marker_cells + 1 ))
  # Reserve a stable indicator column for the configured presentation mode.
  # It must not depend on the current viewport, or the panel width would jump
  # when navigation reveals a different candidate kind. The private-use Nerd
  # Font glyphs used here advance one ZLE cell; treating them as double-width
  # shifted every custom right border one column to the right.
  local -i indicator_cells=0
  case $_shell_sense_indicator_mode in
    icon) indicator_cells=1 ;;
    text) indicator_cells=3 ;;
    both) indicator_cells=5 ;;
  esac
  local -i prefix_cells=$marker_prefix_cells
  (( indicator_cells )) && (( prefix_cells += indicator_cells + 1 ))
  local -i candidate_cells=$_shell_sense_max_label_cells
  (( _shell_sense_show_descriptions )) &&
    candidate_cells=$_shell_sense_max_described_cells
  local -i content_width=$(( prefix_cells + candidate_cells + scrollbar_cells ))
  local -i border_width=2
  [[ $_shell_sense_border == none ]] && border_width=0
  local -i width=$(( content_width + (2 * _shell_sense_padding) + border_width ))
  if (( _shell_sense_show_title )); then
    local title=' completions '
    local -i title_width=$(( $#title + border_width ))
    (( width < title_width )) && width=$title_width
  fi
  if (( _shell_sense_show_footer )) && [[ $_shell_sense_border != none ]]; then
    local footer=" $(( _shell_sense_selected_absolute + 1 ))/$_shell_sense_view_total "
    local -i footer_width=$(( $#footer + border_width ))
    (( width < footer_width )) && width=$footer_width
  fi
  if (( _shell_sense_menu_width > 0 )); then
    width=$_shell_sense_menu_width
  else
    (( width > _shell_sense_max_width )) && width=$_shell_sense_max_width
    (( width < _shell_sense_min_width )) && width=$_shell_sense_min_width
  fi
  # Leave the terminal's final cell unused. Writing into it can trigger an
  # implicit wrap before ZLE has accounted for the following display row.
  (( width > terminal_width - 1 )) && width=$(( terminal_width - 1 ))
  (( width < 8 )) && return 0
  local -i inner=$(( width - border_width ))
  (( content_width = inner - (2 * _shell_sense_padding) ))
  local -i row_content_width=$(( content_width - scrollbar_cells ))
  (( row_content_width < 1 )) && return 0
  local -i first=$(( _shell_sense_menu_view_start - _shell_sense_view_window_start + 1 ))
  # The worker keeps a larger prefetch window around this absolute menu
  # viewport. Refuse to manufacture a different viewport from an incomplete
  # slice: doing so would display the selected item at one row and then move it
  # when the authoritative prefetch window arrived.
  (( first >= 1 && first + rows - 1 <= $#_shell_sense_item_ids )) || return 1
  _shell_sense_render_first=$first
  local -a lines=() highlight_starts=() highlight_ends=() highlight_styles=()
  local -a match_ranges=()
  local fill row marker marker_prefix icon label detail left padding line line_prefix line_suffix
  local kind match_ranges_text match_range label_style match_style kind_style detail_style
  local -i index available icon_cells label_cells detail_cells left_cells indicator_delta
  local -i panel_chars=0 detail_gap=0 detail_start=0 line_start=0 row_identity_position=0
  local -i interior_start=0 interior_end=0 icon_start=0 icon_end=0
  local -i label_offset=0 label_start=0 label_visible=0 match_start=0 match_end=0
  local -i is_selected=0 scrollbar_position=0
  local -i thumb_rows=0 thumb_first=0 row_number=0
  padding=${(l:$_shell_sense_padding:: :)}
  if (( scrollbar_active )); then
    # Like Blink's scrollbar, represent the visible viewport rather than the
    # selected candidate. Selection changes within a stable viewport must not
    # move the thumb.
    _shell_sense_viewport_scrollbar_geometry "$rows" "$_shell_sense_view_total" \
      "$_shell_sense_menu_view_start"
    thumb_rows=${REPLY%:*}
    thumb_first=${REPLY#*:}
  fi
  if [[ $_shell_sense_border != none ]]; then
    if (( _shell_sense_show_title )); then
      fill=${(pl:$(( inner - $#title ))::$horizontal:)}
      line="$tl$title$fill$tr"
    else
      line="$tl${(pl:$inner::$horizontal:)}$tr"
    fi
    lines+=( "$line" )
    highlight_starts+=( $panel_chars )
    highlight_ends+=( $(( panel_chars + ${#line} )) )
    highlight_styles+=( "$_shell_sense_style_border" )
    (( panel_chars += ${#line} + 1 ))
  fi
  for (( index = first; index < first + rows; index++ )); do
    line_start=$panel_chars
    (( row_number++ ))
    (( is_selected = index == _shell_sense_selected ))
    marker=
    marker_prefix=
    if (( marker_cells )); then
      marker=${(l:$marker_cells:: :)}
      (( is_selected )) && marker=$_shell_sense_selected_marker
      marker_prefix="$marker "
    fi
    kind=$_shell_sense_item_kinds[index]
    _shell_sense_kind_indicator "$kind" "$_shell_sense_item_icons[index]"
    icon=$REPLY
    icon_cells=$_shell_sense_indicator_cells
    label=$_shell_sense_item_labels[index]
    label_cells=${_shell_sense_item_label_cells[index]:-${#label}}
    detail=$_shell_sense_item_details[index]
    detail_cells=${_shell_sense_item_detail_cells[index]:-${#detail}}
    (( _shell_sense_show_descriptions )) || detail=
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
        _shell_sense_truncate "$detail" $detail_width
        detail=$REPLY
        detail_cells=$#detail
        # The icon's terminal-cell width can exceed its Zsh character count.
        # Remove that delta from the character budget passed to the fallback
        # truncator so a clamped row still keeps its right border aligned.
        indicator_delta=$(( icon_cells - $#icon ))
        (( indicator_delta < 0 )) && indicator_delta=0
        _shell_sense_truncate "$left" $(( available - detail_cells - 1 - indicator_delta ))
        left=$REPLY
        left_cells=$(( $#left + indicator_delta ))
      fi
      detail_gap=$(( available - left_cells - detail_cells ))
      row="$left${(l:$detail_gap:: :)}$detail"
    else
      if (( left_cells > available )); then
        indicator_delta=$(( icon_cells - $#icon ))
        (( indicator_delta < 0 )) && indicator_delta=0
        _shell_sense_truncate "$left" $(( available - indicator_delta ))
        left=$REPLY
        left_cells=$(( $#left + indicator_delta ))
      fi
      row="$left${(l:$(( available - left_cells )):: :)}"
    fi
    if [[ $_shell_sense_border == none ]]; then
      line_prefix=$padding
      if (( scrollbar_active )); then
        # Keep the normal right-side breathing room inside the content area,
        # then place the scrollbar at the physical edge of the popup.
        row+="$padding$_shell_sense_scrollbar_character"
        line_suffix=
      else
        line_suffix=$padding
      fi
    else
      line_prefix="$vertical$padding"
      if (( scrollbar_active )); then
        row+="$padding$_shell_sense_scrollbar_character"
        line_suffix=$vertical
      else
        line_suffix="$padding$vertical"
      fi
    fi
    line="$line_prefix$row$line_suffix"

    if [[ $_shell_sense_border == none ]]; then
      interior_start=$line_start
      interior_end=$(( line_start + ${#line} ))
    else
      interior_start=$(( line_start + 1 ))
      interior_end=$(( line_start + ${#line} - 1 ))
    fi
    if (( is_selected )); then
      highlight_starts+=( $interior_start )
      highlight_ends+=( $interior_end )
      highlight_styles+=( "$_shell_sense_style_selected" )
      label_style=$_shell_sense_style_label_selected
      match_style=$_shell_sense_style_label_match_selected
      kind_style=${_shell_sense_style_kinds_selected[$kind]:-$_shell_sense_style_kind_selected}
      detail_style=$_shell_sense_style_detail_selected
    else
      label_style=$_shell_sense_style_label
      match_style=$_shell_sense_style_label_match
      kind_style=${_shell_sense_style_kinds[$kind]:-$_shell_sense_style_kind}
      detail_style=$_shell_sense_style_detail
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

      match_ranges_text=$_shell_sense_item_match_ranges[index]
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
    if [[ $_shell_sense_border != none ]]; then
      highlight_starts+=( $line_start $(( line_start + ${#line} - 1 )) )
      highlight_ends+=( $(( line_start + 1 )) $(( line_start + ${#line} )) )
      highlight_styles+=( "$_shell_sense_style_border" "$_shell_sense_style_border" )
    fi
    if (( scrollbar_active )); then
      scrollbar_position=$(( line_start + ${#line_prefix} + ${#row} - 1 ))
      highlight_starts+=( $scrollbar_position )
      highlight_ends+=( $(( scrollbar_position + 1 )) )
      highlight_styles+=( "$_shell_sense_style_scrollbar_gutter" )
      if (( row_number > thumb_first && row_number <= thumb_first + thumb_rows )); then
        highlight_starts+=( $scrollbar_position )
        highlight_ends+=( $(( scrollbar_position + 1 )) )
        highlight_styles+=( "$_shell_sense_style_scrollbar_thumb" )
      fi
    fi
    if (( _shell_sense_padding > 0 && row_number % 2 == 0 )); then
      # Give adjacent physical rows distinct terminal attributes without
      # changing their appearance. This prevents ZLE from representing a
      # viewport update as a visible insert/delete-line transition.
      row_identity_position=$line_start
      [[ $_shell_sense_border == none ]] || (( row_identity_position++ ))
      highlight_starts+=( $row_identity_position )
      highlight_ends+=( $(( row_identity_position + 1 )) )
      highlight_styles+=( bold )
    fi
    lines+=( "$line" )
    (( panel_chars += ${#line} + 1 ))
  done
  if [[ $_shell_sense_border != none ]]; then
    line_start=$panel_chars
    if (( _shell_sense_show_footer )); then
      fill=${(pl:$(( inner - $#footer ))::$horizontal:)}
      line="$bl$fill$footer$br"
      highlight_starts+=( $line_start $(( line_start + ${#bl} + ${#fill} )) )
      highlight_ends+=( $(( line_start + ${#line} )) $(( line_start + ${#bl} + ${#fill} + ${#footer} )) )
      highlight_styles+=( "$_shell_sense_style_border" "$_shell_sense_style_footer" )
    else
      line="$bl${(pl:$inner::$horizontal:)}$br"
      highlight_starts+=( $line_start )
      highlight_ends+=( $(( line_start + ${#line} )) )
      highlight_styles+=( "$_shell_sense_style_border" )
    fi
    lines+=( "$line" )
  fi
  _shell_sense_render_menu_lines=$#lines
  if (( _shell_sense_documentation_width > 0 &&
        _shell_sense_documentation_viewport_rows > 0 )); then
    local -a menu_lines=( "${lines[@]}" ) documentation_lines=()
    local -a documentation_text_starts=() documentation_text_ends=()
    local -a documentation_text_styles=()
    local -a documentation_scrollbar_offsets=() documentation_scrollbar_styles=()
    local -i documentation_width=$_shell_sense_documentation_width
    local -i documentation_border_width=2 documentation_inner documentation_content
    local -i source_index source_cells source_fill documentation_line_start
    local -i documentation_scrollbar_cells=0 documentation_scrollbar_offset=0
    local -i documentation_thumb_rows=0 documentation_thumb_first=0
    local documentation_text documentation_line documentation_style documentation_padding
    [[ $_shell_sense_border == none ]] && documentation_border_width=0
    if (( _shell_sense_show_documentation_scrollbar &&
          _shell_sense_documentation_scrollbar )); then
      documentation_scrollbar_cells=1
      _shell_sense_viewport_scrollbar_geometry \
        "$_shell_sense_documentation_viewport_rows" \
        "$_shell_sense_documentation_total" \
        "$_shell_sense_documentation_offset"
      documentation_thumb_rows=${REPLY%:*}
      documentation_thumb_first=${REPLY#*:}
    fi
    documentation_inner=$(( documentation_width - documentation_border_width ))
    documentation_content=$(( documentation_inner -
      2 * _shell_sense_documentation_padding - documentation_scrollbar_cells ))
    (( documentation_content > 0 )) || return 0
    documentation_padding=${(l:$_shell_sense_documentation_padding:: :)}
    if [[ $_shell_sense_border != none ]]; then
      documentation_lines+=( "$tl${(pl:$documentation_inner::$horizontal:)}$tr" )
    fi
    for (( source_index = 1;
           source_index <= _shell_sense_documentation_viewport_rows;
           source_index++ )); do
      documentation_text=${_shell_sense_documentation_lines[source_index]-}
      source_cells=${_shell_sense_documentation_cells[source_index]:-0}
      (( source_fill = documentation_content - source_cells, source_fill < 0 )) && source_fill=0
      if [[ $_shell_sense_border == none ]]; then
        documentation_line="$documentation_padding$documentation_text${(l:$source_fill:: :)}$documentation_padding"
        documentation_line_start=${#documentation_padding}
      else
        documentation_line="$vertical$documentation_padding$documentation_text${(l:$source_fill:: :)}$documentation_padding"
        documentation_line_start=$(( ${#vertical} + ${#documentation_padding} ))
      fi
      if (( documentation_scrollbar_cells )); then
        documentation_scrollbar_offset=${#documentation_line}
        documentation_line+=$_shell_sense_scrollbar_character
        if (( source_index > documentation_thumb_first &&
              source_index <= documentation_thumb_first + documentation_thumb_rows )); then
          documentation_scrollbar_styles+=( "$_shell_sense_style_scrollbar_thumb" )
        else
          documentation_scrollbar_styles+=( "$_shell_sense_style_scrollbar_gutter" )
        fi
        documentation_scrollbar_offsets+=( $documentation_scrollbar_offset )
      else
        documentation_scrollbar_offsets+=( -1 )
        documentation_scrollbar_styles+=( "" )
      fi
      if [[ $_shell_sense_border != none ]]; then
        documentation_line+=$vertical
      fi
      case $_shell_sense_documentation_kinds[source_index] in
        heading) documentation_style=$_shell_sense_style_documentation_heading ;;
        code) documentation_style=$_shell_sense_style_documentation_code ;;
        quote) documentation_style=$_shell_sense_style_documentation_quote ;;
        separator) documentation_style=$_shell_sense_style_documentation_border ;;
        *) documentation_style=$_shell_sense_style_documentation ;;
      esac
      documentation_lines+=( "$documentation_line" )
      documentation_text_starts+=( $documentation_line_start )
      documentation_text_ends+=( $(( documentation_line_start + ${#documentation_text} )) )
      documentation_text_styles+=( "$documentation_style" )
    done
    if [[ $_shell_sense_border != none ]]; then
      documentation_lines+=( "$bl${(pl:$documentation_inner::$horizontal:)}$br" )
    fi

    if [[ $_shell_sense_documentation_placement == side ]]; then
      local -a old_line_starts=() new_line_starts=() combined_lines=()
      local -i line_number old_cursor=0 new_cursor=0 combined_count
      (( combined_count = $#menu_lines > $#documentation_lines ?
                         $#menu_lines : $#documentation_lines ))
      for (( line_number = 1; line_number <= combined_count; line_number++ )); do
        local menu_line=${menu_lines[line_number]-${(l:$width:: :)}}
        local doc_line=${documentation_lines[line_number]-${(l:$documentation_width:: :)}}
        old_line_starts[line_number]=$old_cursor
        new_line_starts[line_number]=$new_cursor
        combined_lines+=( "$menu_line $doc_line" )
        (( line_number <= $#menu_lines )) && (( old_cursor += ${#menu_line} + 1 ))
        (( new_cursor += ${#menu_line} + 1 + ${#doc_line} + 1 ))
      done
      local -i highlight_index offset mapped line_limit
      for (( highlight_index = 1; highlight_index <= $#highlight_starts; highlight_index++ )); do
        offset=highlight_starts[highlight_index]
        for (( line_number = 1; line_number <= $#menu_lines; line_number++ )); do
          line_limit=$(( old_line_starts[line_number] + ${#menu_lines[line_number]} + 1 ))
          if (( offset < line_limit || line_number == $#menu_lines )); then
            mapped=$(( new_line_starts[line_number] + offset - old_line_starts[line_number] ))
            highlight_starts[highlight_index]=$mapped
            break
          fi
        done
        offset=highlight_ends[highlight_index]
        for (( line_number = 1; line_number <= $#menu_lines; line_number++ )); do
          line_limit=$(( old_line_starts[line_number] + ${#menu_lines[line_number]} + 1 ))
          if (( offset < line_limit || line_number == $#menu_lines )); then
            mapped=$(( new_line_starts[line_number] + offset - old_line_starts[line_number] ))
            highlight_ends[highlight_index]=$mapped
            break
          fi
        done
      done
      local -i documentation_row=0 text_index=0 doc_base
      for (( line_number = 1; line_number <= $#documentation_lines; line_number++ )); do
        (( line_number <= $#menu_lines )) && doc_base=$(( new_line_starts[line_number] + ${#menu_lines[line_number]} + 1 )) ||
          doc_base=$(( new_line_starts[line_number] + width + 1 ))
        highlight_starts+=( $doc_base )
        highlight_ends+=( $(( doc_base + ${#documentation_lines[line_number]} )) )
        highlight_styles+=( "$_shell_sense_style_documentation" )
        if [[ $_shell_sense_border != none &&
              ( line_number == 1 || line_number == $#documentation_lines ) ]]; then
          highlight_starts+=( $doc_base )
          highlight_ends+=( $(( doc_base + ${#documentation_lines[line_number]} )) )
          highlight_styles+=( "$_shell_sense_style_border" )
        else
          (( text_index++ ))
          highlight_starts+=( $(( doc_base + documentation_text_starts[text_index] )) )
          highlight_ends+=( $(( doc_base + documentation_text_ends[text_index] )) )
          highlight_styles+=( "$documentation_text_styles[text_index]" )
          if (( documentation_scrollbar_offsets[text_index] >= 0 )); then
            highlight_starts+=( $(( doc_base + documentation_scrollbar_offsets[text_index] )) )
            highlight_ends+=( $(( doc_base + documentation_scrollbar_offsets[text_index] + 1 )) )
            highlight_styles+=( "$documentation_scrollbar_styles[text_index]" )
          fi
        fi
      done
      lines=( "${combined_lines[@]}" )
    else
      local -i below_cursor=${#${(F)menu_lines}} below_index text_index=0
      (( $#menu_lines )) && (( below_cursor++ ))
      for (( below_index = 1; below_index <= $#documentation_lines; below_index++ )); do
        highlight_starts+=( $below_cursor )
        highlight_ends+=( $(( below_cursor + ${#documentation_lines[below_index]} )) )
        highlight_styles+=( "$_shell_sense_style_documentation" )
        if [[ $_shell_sense_border != none &&
              ( below_index == 1 || below_index == $#documentation_lines ) ]]; then
          highlight_starts+=( $below_cursor )
          highlight_ends+=( $(( below_cursor + ${#documentation_lines[below_index]} )) )
          highlight_styles+=( "$_shell_sense_style_border" )
        else
          (( text_index++ ))
          highlight_starts+=( $(( below_cursor + documentation_text_starts[text_index] )) )
          highlight_ends+=( $(( below_cursor + documentation_text_ends[text_index] )) )
          highlight_styles+=( "$documentation_text_styles[text_index]" )
          if (( documentation_scrollbar_offsets[text_index] >= 0 )); then
            highlight_starts+=( $(( below_cursor + documentation_scrollbar_offsets[text_index] )) )
            highlight_ends+=( $(( below_cursor + documentation_scrollbar_offsets[text_index] + 1 )) )
            highlight_styles+=( "$documentation_scrollbar_styles[text_index]" )
          fi
        fi
        (( below_cursor += ${#documentation_lines[below_index]} + 1 ))
      done
      lines+=( "${documentation_lines[@]}" )
    fi
  fi
  local panel="${(F)lines}"
  _shell_sense_render_lines=( "${lines[@]}" )
  _shell_sense_render_highlight_starts=( 0 "${highlight_starts[@]}" )
  _shell_sense_render_highlight_ends=( ${#panel} "${highlight_ends[@]}" )
  _shell_sense_render_highlight_styles=( "$_shell_sense_style_menu" "${highlight_styles[@]}" )
  _shell_sense_render_columns=$COLUMNS
  _shell_sense_render_dirty=0
  _shell_sense_current_ghost
  _shell_sense_set_postdisplay "$panel" "$REPLY"
  (( request_redisplay )) && _shell_sense_redisplay
}

_shell_sense_accept_selected() {
  (( _shell_sense_popup_visible && ! _shell_sense_popup_stale &&
     _shell_sense_selected >= 1 &&
     _shell_sense_selected <= $#_shell_sense_item_ids )) || return 1
  local source=$_shell_sense_item_acceptance_sources[_shell_sense_selected]
  local identity=$_shell_sense_item_acceptance_identities[_shell_sense_selected]
  if [[ $source == zsh && $identity == <-> ]]; then
    _shell_sense_apply_serial=$_shell_sense_capture_serial
    _shell_sense_apply_index=$identity
    zle .shell-sense-zsh-apply
    typeset -gi _shell_sense_last_apply_status=$?
    (( _shell_sense_last_apply_status == 0 )) || return 1
    _shell_sense_clear_popup
    _shell_sense_last_buffer=$BUFFER
    _shell_sense_last_cursor=$CURSOR
    (( _shell_sense_after_accept )) && _shell_sense_request after-accept
    return 0
  fi
  _shell_sense_send select "$_shell_sense_active_request" "$_shell_sense_active_generation" \
    "$_shell_sense_item_ids[_shell_sense_selected]"
}

_shell_sense_call_original() {
  emulate -L zsh
  local logical=$1 map=${KEYMAP:-main}
  [[ $map == main ]] && map=${_shell_sense_main_keymap:-viins}
  local widget=${_shell_sense_original_widgets[$map:$logical]-}
  if [[ -z $widget ]]; then
    # A key may name a widget supplied by an optional plugin that has not been
    # loaded yet (the user's autosuggest-accept binding is one such case).
    # Resolve it lazily so our dispatcher can still be installed now and will
    # delegate correctly if that plugin appears later.
    local original=${_shell_sense_original_names[$map:$logical]-}
    [[ -n $original && -n ${widgets[$original]-} ]] && widget=$original
  fi
  [[ -n $widget ]] && zle "$widget"
}

_shell_sense_cleanup_then_replay_key() {
  emulate -L zsh
  local keys=$1
  # ZLE commits Enter immediately. Give it one widget return with the
  # popup physically deleted and POSTDISPLAY empty, then replay the exact key
  # so the ordinary widget runs against a clean editing display.
  _shell_sense_erase_edit_display
  _shell_sense_prepare_line_finish
  zle -U "$keys"
}

_shell_sense_restore_terminal_interrupt() {
  emulate -L zsh
  (( _shell_sense_terminal_interrupt_disabled )) || return 0
  command stty intr '^C' </dev/tty 2>/dev/null
  _shell_sense_terminal_interrupt_disabled=0
}

_shell_sense_arm_interrupt_key() {
  emulate -L zsh
  (( _shell_sense_interrupt_key_enabled )) || return 0
  if (( _shell_sense_terminal_interrupt_disabled )); then
    # A blank line or an interrupted edit can start another prompt without a
    # preexec restoration. Keep the already-owned setting disabled.
    command stty intr undef </dev/tty 2>/dev/null
    return 0
  fi
  local settings
  settings=$(command stty -a </dev/tty 2>/dev/null) || return 0
  # If VINTR is already custom or disabled, Ctrl-C is an ordinary byte and
  # needs no terminal mutation. Manage only the conventional ^C setting.
  [[ $settings =~ '(^|[[:space:];])intr[[:space:]]*=[[:space:]]*\^C([;[:space:]]|$)' ]] || return 0
  command stty intr undef </dev/tty 2>/dev/null || return 0
  _shell_sense_terminal_interrupt_disabled=1
}

_shell_sense_configure_interrupt_key() {
  emulate -L zsh
  if [[ ${_shell_sense_bindings_closed[ctrl-c]-} == interrupt ||
        ${_shell_sense_bindings_popup[ctrl-c]-} == interrupt ]]; then
    _shell_sense_interrupt_key_enabled=1
  else
    _shell_sense_interrupt_key_enabled=0
    _shell_sense_restore_terminal_interrupt
  fi
}

_shell_sense_key_dispatch() {
  emulate -L zsh
  local logical=${_shell_sense_widget_keys[$WIDGET]-}
  local action=
  if [[ -n $_shell_sense_owned_postdisplay &&
        ${_shell_sense_bindings_popup[$logical]-} == interrupt ]]; then
    # An edit can make a still-rendered panel stale. Ctrl-C must nevertheless
    # use popup cleanup rather than the closed-state binding table.
    action=interrupt
  elif (( _shell_sense_popup_visible && ! _shell_sense_popup_stale )); then
    action=${_shell_sense_bindings_popup[$logical]-}
  else
    action=${_shell_sense_bindings_closed[$logical]-}
  fi
  case $action in
    trigger) _shell_sense_request manual ;;
    accept) _shell_sense_accept_selected || _shell_sense_call_original "$logical" ;;
    execute)
      if [[ -n $_shell_sense_owned_postdisplay ]]; then
        _shell_sense_cleanup_then_replay_key "$KEYS"
      else
        _shell_sense_call_original "$logical"
      fi
      ;;
    interrupt)
      if [[ -n $_shell_sense_owned_postdisplay ]]; then
        _shell_sense_erase_edit_display
        _shell_sense_prepare_line_finish
      fi
      # `interrupt` has explicit send-break semantics regardless of what the
      # same key did before shell-sense. It aborts ZLE immediately and starts a
      # fresh line without reintroducing the cleared POSTDISPLAY.
      zle send-break
      ;;
    next|previous|page-down|page-up)
      _shell_sense_navigate_candidate "$action" || _shell_sense_call_original "$logical"
      ;;
    documentation-down|documentation-up|documentation-page-down|documentation-page-up|toggle-documentation)
      if (( _shell_sense_popup_visible && ! _shell_sense_popup_stale )); then
        _shell_sense_send navigate "$_shell_sense_active_request" \
          "$_shell_sense_active_generation" "$_shell_sense_navigation_serial" "$action"
      else
        _shell_sense_call_original "$logical"
      fi
      ;;
    accept-next-token)
      _shell_sense_accept_ghost_part "$_shell_sense_ghost_partial_accept" ||
        _shell_sense_call_original "$logical"
      ;;
    accept-ghost)
      _shell_sense_current_ghost
      if [[ -n $REPLY ]]; then
        _shell_sense_accept_selected || _shell_sense_call_original "$logical"
      else
        _shell_sense_call_original "$logical"
      fi
      ;;
    dismiss)
      (( _shell_sense_active_request )) && _shell_sense_send cancel \
        "$_shell_sense_active_request" "$_shell_sense_active_generation"
      _shell_sense_clear_popup
      ;;
    none) ;;
    *) _shell_sense_call_original "$logical" ;;
  esac
}

_shell_sense_navigate_candidate() {
  emulate -L zsh
  (( _shell_sense_popup_visible && ! _shell_sense_popup_stale &&
     _shell_sense_view_total > 0 )) || return 1
  local action=$1
  local -i desired=$_shell_sense_selected_absolute
  case $action in
    next)
      if (( desired + 1 < _shell_sense_view_total )); then
        (( desired++ ))
      elif (( _shell_sense_cycle )); then
        desired=0
      fi
      ;;
    previous)
      if (( desired > 0 )); then
        (( desired-- ))
      elif (( _shell_sense_cycle )); then
        desired=$(( _shell_sense_view_total - 1 ))
      fi
      ;;
    page-down)
      (( desired += _shell_sense_max_rows ))
      (( desired >= _shell_sense_view_total )) && desired=$(( _shell_sense_view_total - 1 ))
      ;;
    page-up)
      (( desired -= _shell_sense_max_rows ))
      (( desired < 0 )) && desired=0
      ;;
    *) return 1 ;;
  esac
  (( _shell_sense_navigation_serial++ ))
  local -i relative=$(( desired - _shell_sense_view_window_start + 1 ))
  _shell_sense_menu_viewport_start_for "$desired" "$_shell_sense_menu_view_start"
  local -i desired_view_start=$REPLY
  if (( relative >= 1 && relative <= $#_shell_sense_item_ids )) &&
      _shell_sense_cached_menu_viewport_contains "$desired_view_start"; then
    _shell_sense_selected=$relative
    _shell_sense_selected_absolute=$desired
    _shell_sense_menu_view_start=$desired_view_start
    _shell_sense_render_dirty=1
  fi
  _shell_sense_send navigate "$_shell_sense_active_request" \
    "$_shell_sense_active_generation" "$_shell_sense_navigation_serial" "$action"
}

_shell_sense_install_keybindings() {
  emulate -L zsh
  local main_definition
  local -a definition_words maps keys
  main_definition=$(bindkey -lL main 2>/dev/null)
  definition_words=( ${(z)main_definition} )
  if [[ $definition_words[1] == bindkey && $definition_words[2] == -A &&
        -n $definition_words[3] ]]; then
    _shell_sense_main_keymap=$definition_words[3]
  else
    _shell_sense_main_keymap=viins
  fi
  maps=( emacs viins "$_shell_sense_main_keymap" )
  maps=( ${(u)maps} )
  keys=( ${(k)_shell_sense_bindings_closed} ${(k)_shell_sense_bindings_popup} )
  keys=( ${(u)keys} )
  local map logical sequence line original alias widget safe
  for logical in "${keys[@]}"; do
    sequence=${_shell_sense_key_sequences[$logical]-}
    [[ -n $sequence ]] || continue
    safe=${logical//[^A-Za-z0-9_-]/-}
    widget=".shell-sense-key-$safe"
    _shell_sense_widget_keys[$widget]=$logical
    zle -N "$widget" _shell_sense_key_dispatch
    for map in "${maps[@]}"; do
      line=$(bindkey -M "$map" "$sequence" 2>/dev/null) || continue
      definition_words=( ${(z)line} )
      original=${definition_words[2]-}
      [[ -n $original && $original != "$widget" ]] || continue
      alias=".shell-sense-original-$map-$safe"
      _shell_sense_original_names[$map:$logical]=$original
      if zle -A "$original" "$alias" 2>/dev/null; then
        _shell_sense_original_widgets[$map:$logical]=$alias
      else
        _shell_sense_original_widgets[$map:$logical]=
      fi
      _shell_sense_bound_sequences[$map:$logical]=$sequence
      bindkey -M "$map" "$sequence" "$widget"
    done
  done
}

_shell_sense_disconnect() {
  emulate -L zsh
  if (( _shell_sense_read_fd >= 0 )); then
    zle -F $_shell_sense_read_fd 2>/dev/null
    exec {_shell_sense_read_fd}<&- 2>/dev/null
  fi
  if (( _shell_sense_write_fd >= 0 )); then
    exec {_shell_sense_write_fd}>&- 2>/dev/null
  fi
  _shell_sense_read_fd=-1
  _shell_sense_write_fd=-1
  _shell_sense_ready=0
  _shell_sense_configured=0
  _shell_sense_clear_popup
}

_shell_sense_ensure_worker() {
  emulate -L zsh
  if (( _shell_sense_worker_pid > 0 && _shell_sense_write_fd >= 0 )) &&
      kill -0 $_shell_sense_worker_pid 2>/dev/null; then
    return 0
  fi
  _shell_sense_worker_pid=0
  _shell_sense_disconnect
  _shell_sense_start_worker
}

_shell_sense_abort_worker_start() {
  emulate -L zsh
  (( _shell_sense_worker_pid <= 0 )) || kill $_shell_sense_worker_pid 2>/dev/null
  _shell_sense_worker_pid=0
  _shell_sense_disconnect
  [[ -z $_shell_sense_fifo_in ]] || command unlink -- "$_shell_sense_fifo_in" 2>/dev/null
  [[ -z $_shell_sense_fifo_out ]] || command unlink -- "$_shell_sense_fifo_out" 2>/dev/null
  _shell_sense_fifo_in=
  _shell_sense_fifo_out=
}

_shell_sense_cleanup() {
  emulate -L zsh
  autoload -Uz add-zle-hook-widget add-zsh-hook
  add-zle-hook-widget -d line-pre-redraw _shell_sense_line_pre_redraw 2>/dev/null
  add-zle-hook-widget -d line-init _shell_sense_line_init 2>/dev/null
  add-zle-hook-widget -d line-finish _shell_sense_line_finish 2>/dev/null
  add-zsh-hook -d precmd _shell_sense_arm_interrupt_key 2>/dev/null
  add-zsh-hook -d preexec _shell_sense_restore_terminal_interrupt 2>/dev/null
  add-zsh-hook -d zshexit _shell_sense_cleanup 2>/dev/null
  _shell_sense_restore_terminal_interrupt
  _shell_sense_teardown_synchronized_redraw
  local key map logical sequence original
  for key in ${(k)_shell_sense_bound_sequences}; do
    map=${key%%:*}
    logical=${key#*:}
    sequence=$_shell_sense_bound_sequences[$key]
    original=$_shell_sense_original_names[$key]
    [[ -n $sequence && -n $original ]] && bindkey -M "$map" "$sequence" "$original" 2>/dev/null
  done
  (( _shell_sense_ready )) && _shell_sense_send goodbye
  _shell_sense_disconnect
  [[ -n $_shell_sense_fifo_in ]] && command unlink -- "$_shell_sense_fifo_in" 2>/dev/null
  [[ -n $_shell_sense_fifo_out ]] && command unlink -- "$_shell_sense_fifo_out" 2>/dev/null
}

_shell_sense_start_worker() {
  emulate -L zsh
  setopt localoptions no_aliases
  local root=${_shell_sense_plugin_dir:h:h}
  local -a command_line worker_args
  if [[ -n ${SHELL_SENSE_COMMAND:-} ]]; then
    command_line=( ${(z)SHELL_SENSE_COMMAND} )
  elif (( $+commands[shell-sense] )); then
    command_line=( "$commands[shell-sense]" )
  elif [[ -x $root/target/release/shell-sense ]]; then
    command_line=( "$root/target/release/shell-sense" )
  elif [[ -x $root/target/debug/shell-sense ]]; then
    command_line=( "$root/target/debug/shell-sense" )
  else
    return 1
  fi

  _shell_sense_runtime_directory || return 1
  local runtime_dir=$REPLY
  local token="${sysparams[pid]}-${RANDOM}-${RANDOM}"
  _shell_sense_fifo_in="$runtime_dir/shell-$token.in"
  _shell_sense_fifo_out="$runtime_dir/shell-$token.out"
  command mkfifo -m 600 -- "$_shell_sense_fifo_in" "$_shell_sense_fifo_out" || return 1

  local state_base=${XDG_STATE_HOME:-$HOME/.local/state}
  command mkdir -p -m 700 -- "$state_base/shell-sense" 2>/dev/null
  _shell_sense_log_file="$state_base/shell-sense/worker-${sysparams[pid]}.log"
  worker_args=( worker --shell-input-fifo "$_shell_sense_fifo_in" \
    --shell-output-fifo "$_shell_sense_fifo_out" --shell zsh \
    --shell-process-id "${sysparams[pid]}" \
    --shell-executable "${commands[zsh]:-$SHELL}" --shell-version "$ZSH_VERSION" \
    --shell-patchlevel "$ZSH_PATCHLEVEL" )
  [[ -n ${SHELL_SENSE_SOCKET:-} ]] && worker_args+=( --socket "$SHELL_SENSE_SOCKET" )
  [[ -n ${SHELL_SENSE_CONFIG:-} ]] && worker_args+=( --config "$SHELL_SENSE_CONFIG" )
  [[ -n ${SHELL_SENSE_PROFILE:-} ]] && worker_args+=( --profile "$SHELL_SENSE_PROFILE" )
  [[ ${SHELL_SENSE_NO_DAEMON_AUTOSTART:-0} == 1 ]] && worker_args+=( --no-daemon-autostart )
  "${command_line[@]}" "${worker_args[@]}" </dev/null >>"$_shell_sense_log_file" 2>&1 &!
  _shell_sense_worker_pid=$!

  local -i attempt
  for (( attempt = 1; attempt <= 200; attempt++ )); do
    sysopen -w -o cloexec,nonblock -u _shell_sense_write_fd "$_shell_sense_fifo_in" 2>/dev/null && break
    kill -0 $_shell_sense_worker_pid 2>/dev/null || break
    zselect -t 1 >/dev/null 2>&1
  done
  (( _shell_sense_write_fd >= 0 )) || { _shell_sense_abort_worker_start; return 1; }
  sysopen -r -o cloexec,nonblock -u _shell_sense_read_fd "$_shell_sense_fifo_out" 2>/dev/null || {
    _shell_sense_abort_worker_start
    return 1
  }
  command unlink -- "$_shell_sense_fifo_in" 2>/dev/null
  command unlink -- "$_shell_sense_fifo_out" 2>/dev/null
  _shell_sense_fifo_in=
  _shell_sense_fifo_out=
  zle -N .shell-sense-fd-callback _shell_sense_fd_callback
  zle -Fw $_shell_sense_read_fd .shell-sense-fd-callback
}

_shell_sense_init() {
  emulate -L zsh
  [[ -o interactive ]] || return 0
  zmodload zsh/system zsh/zselect zsh/terminfo 2>/dev/null || return 1
  autoload -Uz add-zle-hook-widget add-zsh-hook
  _shell_sense_rebuild_styles
  _shell_sense_zsh_init || return 1
  add-zle-hook-widget line-pre-redraw _shell_sense_line_pre_redraw
  add-zle-hook-widget line-init _shell_sense_line_init
  add-zle-hook-widget line-finish _shell_sense_line_finish
  # Change VINTR before ZLE snapshots terminal state, then restore it after
  # ZLE exits and before the accepted command starts. This keeps Ctrl-C a ZLE
  # key at the prompt without changing signal delivery for external programs.
  add-zsh-hook precmd _shell_sense_arm_interrupt_key
  add-zsh-hook preexec _shell_sense_restore_terminal_interrupt
  add-zsh-hook zshexit _shell_sense_cleanup
  _shell_sense_last_buffer=
  _shell_sense_last_cursor=-1
  _shell_sense_setup_synchronized_redraw
  _shell_sense_start_worker
}
