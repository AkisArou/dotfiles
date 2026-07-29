# Shell Sense entry point for Bash 5.2 and newer.

[[ $- == *i* ]] || return

_shell_sense_bash_plugin_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
source "$_shell_sense_bash_plugin_dir/provider.bash"
source "$_shell_sense_bash_plugin_dir/client.bash"
_shell_sense_bash_start
