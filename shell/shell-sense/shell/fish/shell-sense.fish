# Shell Sense entry point for Fish 4.0 and newer.

status is-interactive; or return

set -g _shell_sense_fish_plugin_dir (path dirname (status filename))
source "$_shell_sense_fish_plugin_dir/provider.fish"
source "$_shell_sense_fish_plugin_dir/client.fish"
__shell_sense_fish_start
