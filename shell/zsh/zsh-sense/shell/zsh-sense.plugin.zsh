# zsh-sense entry point. Source this after compinit and other ZLE plugins.

[[ -n ${ZSH_VERSION:-} ]] || return 0

typeset -g _zsh_sense_plugin_dir=${${(%):-%x}:A:h}
if (($+functions[_zsh_sense_cleanup])); then
  _zsh_sense_cleanup
fi
source "$_zsh_sense_plugin_dir/capture.zsh"
source "$_zsh_sense_plugin_dir/client.zsh"
_zsh_sense_init
