# Shell Sense entry point for Zsh. Source after compinit and other ZLE plugins.

[[ -n ${ZSH_VERSION:-} ]] || return 0

typeset -g _shell_sense_plugin_dir=${${(%):-%x}:A:h}
if (($+functions[_shell_sense_cleanup])); then
  _shell_sense_cleanup
fi
source "$_shell_sense_plugin_dir/provider.zsh"
source "$_shell_sense_plugin_dir/client.zsh"
_shell_sense_init
