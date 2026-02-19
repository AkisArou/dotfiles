case "${THEME:-tokyonight}" in
  tokyonight)
    local BLUE="#7aa2f7"
    local BLUE0="#3d59a1"
    local BLUE1="#2ac3de"
    local BLUE2="#0db9d7"
    local BLUE5="#89ddff"
    local BLUE6="#b4f9f8"
    local BLUE7="#394b70"
    local COMMENT="#565f89"
    local CYAN="#7dcfff"
    local DARK3="#545c7e"
    local DARK5="#737aa2"
    local FG="#c0caf5"
    local FG_DARK="#a9b1d6"
    local FG_GUTTER="#3b4261"
    local GREEN="#9ece6a"
    local GREEN1="#73daca"
    local GREEN2="#41a6b5"
    local MAGENTA="#bb9af7"
    local MAGENTA2="#ff007c"
    local ORANGE="#ff9e64"
    local PURPLE="#9d7cd8"
    local RED="#f7768e"
    local RED1="#db4b4b"
    local TEAL="#1abc9c"
    local TERMINAL_BLACK="#414868"
    local YELLOW="#e0af68"
    ;;
  vscode)
    local BLUE="#569CD6"
    local BLUE0="#569CD6"
    local BLUE1="#4FC1FF"
    local BLUE2="#9CDCFE"
    local BLUE5="#4FC1FF"
    local BLUE6="#9CDCFE"
    local BLUE7="#264F78"
    local COMMENT="#6A9955"
    local CYAN="#4EC9B0"
    local DARK3="#636369"
    local DARK5="#808080"
    local FG="#D4D4D4"
    local FG_DARK="#BBBBBB"
    local FG_GUTTER="#5A5A5A"
    local GREEN="#6A9955"
    local GREEN1="#81b88b"
    local GREEN2="#B5CEA8"
    local MAGENTA="#C586C0"
    local MAGENTA2="#C586C0"
    local ORANGE="#C48081"
    local YELLOW="#DCDCAA"
    local PURPLE="#C586C0"
    local RED="#F44747"
    local RED1="#D16969"
    local TEAL="#4EC9B0"
    local TERMINAL_BLACK="#121314"
    ;;
  nevermore|*)
    local BLUE="#7aa2f7"
    local BLUE0="#7aa2f7"
    local BLUE1="#7dcfff"
    local BLUE2="#7dcfff"
    local BLUE5="#7dcfff"
    local BLUE6="#aaaaaa"
    local BLUE7="#555555"
    local COMMENT="#555555"
    local CYAN="#7dcfff"
    local DARK3="#555555"
    local DARK5="#666666"
    local FG="#999999"
    local FG_DARK="#555555"
    local FG_GUTTER="#555555"
    local GREEN="#009966"
    local GREEN1="#009966"
    local GREEN2="#009966"
    local MAGENTA="#9d7cd8"
    local MAGENTA2="#9d7cd8"
    local ORANGE="#aa5500"
    local YELLOW="#AAAA00"
    local PURPLE="#9d7cd8"
    local RED="#f7768e"
    local RED1="#f7768e"
    local TEAL="#7dcfff"
    local TERMINAL_BLACK="#000000"
    ;;
esac

# =====================[ Powerlevel10k Config ]=====================
'builtin' 'local' '-a' 'p10k_config_opts'
[[ ! -o 'aliases'         ]] || p10k_config_opts+=('aliases')
[[ ! -o 'sh_glob'         ]] || p10k_config_opts+=('sh_glob')
[[ ! -o 'no_brace_expand' ]] || p10k_config_opts+=('no_brace_expand')
'builtin' 'setopt' 'no_aliases' 'no_sh_glob' 'brace_expand'

() {
  emulate -L zsh -o extended_glob

  # Unset previous config
  unset -m '(POWERLEVEL9K_*|DEFAULT_USER)~POWERLEVEL9K_GITSTATUS_DIR'

  [[ $ZSH_VERSION == (5.<1->*|<6->.*) ]] || return

  # --------------------- Left Prompt ---------------------
  typeset -g POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(
    dir
    vcs
    newline
    prompt_char
  )

  # --------------------- Right Prompt ---------------------
  typeset -g POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(
    status
    background_jobs
    vim_shell
    per_directory_history
    newline
    # command_execution_time
    # node_version
    # package
  )

  # --------------------- General Options ---------------------
  typeset -g POWERLEVEL9K_MODE=nerdfont-v3
  typeset -g POWERLEVEL9K_ICON_PADDING=none
  typeset -g POWERLEVEL9K_BACKGROUND=
  typeset -g POWERLEVEL9K_{LEFT,RIGHT}_{LEFT,RIGHT}_WHITESPACE=
  typeset -g POWERLEVEL9K_{LEFT,RIGHT}_SUBSEGMENT_SEPARATOR=' '
  typeset -g POWERLEVEL9K_{LEFT,RIGHT}_SEGMENT_SEPARATOR=
  typeset -g POWERLEVEL9K_ICON_BEFORE_CONTENT=true
  typeset -g POWERLEVEL9K_PROMPT_ADD_NEWLINE=true

  typeset -g POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=
  typeset -g POWERLEVEL9K_MULTILINE_NEWLINE_PROMPT_PREFIX=
  typeset -g POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX=
  typeset -g POWERLEVEL9K_MULTILINE_FIRST_PROMPT_SUFFIX=
  typeset -g POWERLEVEL9K_MULTILINE_NEWLINE_PROMPT_SUFFIX=
  typeset -g POWERLEVEL9K_MULTILINE_LAST_PROMPT_SUFFIX=

  typeset -g POWERLEVEL9K_LEFT_PROMPT_FIRST_SEGMENT_START_SYMBOL=
  typeset -g POWERLEVEL9K_RIGHT_PROMPT_LAST_SEGMENT_END_SYMBOL=

  typeset -g POWERLEVEL9K_SHOW_RULER=false
  typeset -g POWERLEVEL9K_RULER_CHAR='─'
  typeset -g POWERLEVEL9K_RULER_FOREGROUND=$DARK5
  typeset -g POWERLEVEL9K_MULTILINE_FIRST_PROMPT_GAP_CHAR=' '

  # --------------------- Prompt Char ---------------------
  typeset -g POWERLEVEL9K_PROMPT_CHAR_OK_{VIINS,VICMD,VIVIS,VIOWR}_FOREGROUND=$DARK5
  typeset -g POWERLEVEL9K_PROMPT_CHAR_ERROR_{VIINS,VICMD,VIVIS,VIOWR}_FOREGROUND=$RED
  typeset -g POWERLEVEL9K_PROMPT_CHAR_{OK,ERROR}_VIINS_CONTENT_EXPANSION='❯'
  typeset -g POWERLEVEL9K_PROMPT_CHAR_{OK,ERROR}_VICMD_CONTENT_EXPANSION='❮'
  typeset -g POWERLEVEL9K_PROMPT_CHAR_{OK,ERROR}_VIVIS_CONTENT_EXPANSION='V'
  typeset -g POWERLEVEL9K_PROMPT_CHAR_{OK,ERROR}_VIOWR_CONTENT_EXPANSION='▶'
  typeset -g POWERLEVEL9K_PROMPT_CHAR_OVERWRITE_STATE=true
  typeset -g POWERLEVEL9K_PROMPT_CHAR_LEFT_PROMPT_LAST_SEGMENT_END_SYMBOL=''
  typeset -g POWERLEVEL9K_PROMPT_CHAR_LEFT_PROMPT_FIRST_SEGMENT_START_SYMBOL=

  # --------------------- Directory ---------------------
  typeset -g POWERLEVEL9K_DIR_FOREGROUND=$BLUE
  typeset -g POWERLEVEL9K_DIR_SHORTENED_FOREGROUND=$BLUE2
  typeset -g POWERLEVEL9K_DIR_ANCHOR_FOREGROUND=$BLUE
  typeset -g POWERLEVEL9K_DIR_ANCHOR_BOLD=false
  typeset -g POWERLEVEL9K_SHORTEN_STRATEGY=truncate_to_unique
  typeset -g POWERLEVEL9K_SHORTEN_DELIMITER=
  local anchor_files=(.git .node-version .tool-versions .mise.toml .shorten_folder_marker Cargo.toml package.json)
  typeset -g POWERLEVEL9K_SHORTEN_FOLDER_MARKER="(${(j:|:)anchor_files})"
  typeset -g POWERLEVEL9K_DIR_TRUNCATE_BEFORE_MARKER=false
  typeset -g POWERLEVEL9K_SHORTEN_DIR_LENGTH=1
  typeset -g POWERLEVEL9K_DIR_MAX_LENGTH=80
  typeset -g POWERLEVEL9K_DIR_MIN_COMMAND_COLUMNS=40
  typeset -g POWERLEVEL9K_DIR_MIN_COMMAND_COLUMNS_PCT=50
  typeset -g POWERLEVEL9K_DIR_HYPERLINK=false
  typeset -g POWERLEVEL9K_DIR_SHOW_WRITABLE=v3
  typeset -g POWERLEVEL9K_DIR_CLASSES=()

  # --------------------- Git Status Formatter ---------------------
  typeset -g POWERLEVEL9K_VCS_MAX_INDEX_SIZE_DIRTY=-1
  typeset -g POWERLEVEL9K_VCS_DISABLED_WORKDIR_PATTERN='~'
  typeset -g POWERLEVEL9K_VCS_DISABLE_GITSTATUS_FORMATTING=true
  typeset -g POWERLEVEL9K_VCS_CONTENT_EXPANSION='${$((my_git_formatter(1)))+${my_git_format}}'
  typeset -g POWERLEVEL9K_VCS_LOADING_CONTENT_EXPANSION='${$((my_git_formatter(0)))+${my_git_format}}'
  typeset -g POWERLEVEL9K_VCS_STAGED_MAX_NUM=-1
  typeset -g POWERLEVEL9K_VCS_UNSTAGED_MAX_NUM=-1
  typeset -g POWERLEVEL9K_VCS_UNTRACKED_MAX_NUM=-1
  typeset -g POWERLEVEL9K_VCS_CONFLICTED_MAX_NUM=-1
  typeset -g POWERLEVEL9K_VCS_COMMITS_AHEAD_MAX_NUM=-1
  typeset -g POWERLEVEL9K_VCS_COMMITS_BEHIND_MAX_NUM=-1
  typeset -g POWERLEVEL9K_VCS_VISUAL_IDENTIFIER_COLOR=76
  typeset -g POWERLEVEL9K_VCS_LOADING_VISUAL_IDENTIFIER_COLOR=244
  typeset -g POWERLEVEL9K_VCS_VISUAL_IDENTIFIER_EXPANSION=
  typeset -g POWERLEVEL9K_VCS_BACKENDS=(git)

  function my_git_formatter() {
    emulate -L zsh

    # Convert hex to true color ANSI
    hex_to_truecolor() {
      local hex=$1
      [[ $hex == \#* ]] && hex=${hex#\#}
      local r=$((16#${hex[1,2]}))
      local g=$((16#${hex[3,4]}))
      local b=$((16#${hex[5,6]}))
      echo -n "\x1b[38;2;${r};${g};${b}m"
    }

    local meta=$(hex_to_truecolor $FG)
    local clean=$(hex_to_truecolor $DARK5)
    local modified=$(hex_to_truecolor $YELLOW)
    local untracked=$(hex_to_truecolor $BLUE)
    local conflicted=$(hex_to_truecolor $RED)
    local reset=$'\x1b[0m'

    local res=""

    if [[ -n $VCS_STATUS_LOCAL_BRANCH ]]; then
      local branch=${(V)VCS_STATUS_LOCAL_BRANCH}
      (( $#branch > 32 )) && branch[13,-13]="…"
      res+="${clean}${branch//\%/%%}${reset} "
    fi

    if [[ -n $VCS_STATUS_TAG && -z $VCS_STATUS_LOCAL_BRANCH ]]; then
      res+="${meta}#${clean}${VCS_STATUS_TAG//\%/%%}${reset} "
    fi

    if [[ -z $VCS_STATUS_LOCAL_BRANCH && -z $VCS_STATUS_TAG ]]; then
      res+="${meta}@${clean}${VCS_STATUS_COMMIT[1,8]}${reset} "
    fi

    (( VCS_STATUS_COMMITS_BEHIND )) && res+="${clean}⇣${VCS_STATUS_COMMITS_BEHIND}${reset} "
    (( VCS_STATUS_COMMITS_AHEAD )) && res+="${clean}⇡${VCS_STATUS_COMMITS_AHEAD}${reset} "
    (( VCS_STATUS_NUM_STAGED )) && res+="${modified}+${VCS_STATUS_NUM_STAGED}${reset} "
    (( VCS_STATUS_NUM_UNSTAGED )) && res+="${modified}!${VCS_STATUS_NUM_UNSTAGED}${reset} "
    (( VCS_STATUS_NUM_UNTRACKED )) && res+="${untracked}?${VCS_STATUS_NUM_UNTRACKED}${reset} "
    (( VCS_STATUS_NUM_CONFLICTED )) && res+="${conflicted}~${VCS_STATUS_NUM_CONFLICTED}${reset} "

    # Remove trailing space
    res=${res%% }

    typeset -g my_git_format=$res
  }

  functions -M my_git_formatter 2>/dev/null

  # --------------------- Status Segment ---------------------
  typeset -g POWERLEVEL9K_STATUS_EXTENDED_STATES=true
  typeset -g POWERLEVEL9K_STATUS_OK=false
  typeset -g POWERLEVEL9K_STATUS_OK_FOREGROUND=$GREEN
  typeset -g POWERLEVEL9K_STATUS_OK_VISUAL_IDENTIFIER_EXPANSION='✔'
  typeset -g POWERLEVEL9K_STATUS_OK_PIPE=true
  typeset -g POWERLEVEL9K_STATUS_OK_PIPE_FOREGROUND=$GREEN
  typeset -g POWERLEVEL9K_STATUS_OK_PIPE_VISUAL_IDENTIFIER_EXPANSION='✔'
  typeset -g POWERLEVEL9K_STATUS_ERROR=false
  typeset -g POWERLEVEL9K_STATUS_ERROR_FOREGROUND=$RED
  typeset -g POWERLEVEL9K_STATUS_ERROR_VISUAL_IDENTIFIER_EXPANSION='✘'
  typeset -g POWERLEVEL9K_STATUS_ERROR_SIGNAL=true
  typeset -g POWERLEVEL9K_STATUS_ERROR_SIGNAL_FOREGROUND=$RED
  typeset -g POWERLEVEL9K_STATUS_ERROR_SIGNAL_VISUAL_IDENTIFIER_EXPANSION='✘'
  typeset -g POWERLEVEL9K_STATUS_ERROR_PIPE=true
  typeset -g POWERLEVEL9K_STATUS_ERROR_PIPE_FOREGROUND=$RED
  typeset -g POWERLEVEL9K_STATUS_ERROR_PIPE_VISUAL_IDENTIFIER_EXPANSION='✘'

  # --------------------- Command Execution Time ---------------------
  typeset -g POWERLEVEL9K_COMMAND_EXECUTION_TIME_THRESHOLD=3
  typeset -g POWERLEVEL9K_COMMAND_EXECUTION_TIME_PRECISION=0
  typeset -g POWERLEVEL9K_COMMAND_EXECUTION_TIME_FOREGROUND=$PURPLE
  typeset -g POWERLEVEL9K_COMMAND_EXECUTION_TIME_FORMAT='d h m s'

  # --------------------- Background Jobs ---------------------
  typeset -g POWERLEVEL9K_BACKGROUND_JOBS_VERBOSE=false
  typeset -g POWERLEVEL9K_BACKGROUND_JOBS_FOREGROUND=$GREEN

  # --------------------- Per-directory history ---------------------
  typeset -g POWERLEVEL9K_PER_DIRECTORY_HISTORY_LOCAL_FOREGROUND=$ORANGE
  typeset -g POWERLEVEL9K_PER_DIRECTORY_HISTORY_GLOBAL_FOREGROUND=$YELLOW

  # --------------------- Context ---------------------
  typeset -g POWERLEVEL9K_CONTEXT_ROOT_FOREGROUND=$RED
  typeset -g POWERLEVEL9K_CONTEXT_{REMOTE,REMOTE_SUDO}_FOREGROUND=$COMMENT
  typeset -g POWERLEVEL9K_CONTEXT_FOREGROUND=$FG
  typeset -g POWERLEVEL9K_CONTEXT_ROOT_TEMPLATE='%B%n@%m'
  typeset -g POWERLEVEL9K_CONTEXT_{REMOTE,REMOTE_SUDO}_TEMPLATE='%n@%m'
  typeset -g POWERLEVEL9K_CONTEXT_TEMPLATE='%n@%m'
  typeset -g POWERLEVEL9K_CONTEXT_{DEFAULT,SUDO}_{CONTENT,VISUAL_IDENTIFIER}_EXPANSION=

  # --------------------- Node.js / Package ---------------------
  typeset -g POWERLEVEL9K_NODE_VERSION_FOREGROUND=$GREEN
  typeset -g POWERLEVEL9K_NODE_VERSION_PROJECT_ONLY=true
  typeset -g POWERLEVEL9K_PACKAGE_FOREGROUND=$MAGENTA

  # --------------------- Transient Prompt ---------------------
  typeset -g POWERLEVEL9K_TRANSIENT_PROMPT=always
  typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet # verbose
  typeset -g POWERLEVEL9K_DISABLE_HOT_RELOAD=true

  (( ! $+functions[p10k] )) || p10k reload
}

typeset -g POWERLEVEL9K_CONFIG_FILE=${${(%):-%x}:a}
(( ${#p10k_config_opts} )) && setopt ${p10k_config_opts[@]}
'builtin' 'unset' 'p10k_config_opts'
