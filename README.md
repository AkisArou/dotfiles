# akisarou dotfiles

This is the configuration of my Arch linux and other useful programs

## Layout

Files are organized by installation destination:

```text
home/                  -> $HOME/
  .config/             -> ~/.config/
  .local/bin/          -> ~/.local/bin/
  .local/libexec/      -> ~/.local/libexec/ (private executable helpers)
  .local/share/        -> ~/.local/share/
root/                  -> /
  etc/                 -> /etc/
  usr/share/           -> /usr/share/
install/                  Setup scripts and deployment rules
  lib/                    Sourced setup helpers
src/                      Source trees and build recipes
  neovim/build            Build/update Neovim
  st/build                Build st
  lib/                    Shared build helpers
```

`root/` represents the filesystem root, not the root user's home directory.

## Migrate an existing computer

After pulling this layout on each computer, preview and run:

```sh
python3 "$HOME/dotfiles/install/migrate" --dry-run
python3 "$HOME/dotfiles/install/migrate"
```

Run as your normal user. The migration uses sudo to retarget existing system
links. It updates old repository links, deploys the personal tree, updates Claude
hook paths, and reloads the active window manager, tmux, and user services.
Existing directories are merged; unrelated files remain in place. Conflicting files are preserved beside
their destinations as `.before-dotfiles` backups, with numbered suffixes as needed.
Local application settings remain regular files. Old repository directory links
containing managed settings are converted into real local directories.
Links into the former `scripts/` directory are also retargeted.

The migration can be repeated. Use `--skip-root` for personal files only or
`--no-reload` to leave running services alone. If the checkout itself moved,
pass its previous absolute location with `--old-repo /previous/path/to/dotfiles`.

The next fresh launch through `brave-launch` updates the saved Video Repeater
extension path. A running Brave process keeps its previous path until then.

## Deploy files

The deployment tools replace the old `create_symlink` function. Directories are
merged recursively, so adding a file beneath `home/` automatically gives it the
corresponding destination. Git-ignored local state is excluded.

```sh
python3 "$HOME/dotfiles/install/deploy-home"
sudo python3 "$HOME/dotfiles/install/link-root"
```

Both commands accept `--dry-run`. Files use symlinks unless the `DEPLOYMENT_RULES`
table in `install/link_tree.py` specifies another method. Defaults live at their destination paths even when
they need merging or importing:

| Source under `home/` | Deployment |
| --- | --- |
| `.claude/settings.json` | Merge defaults while preserving local choices |
| `.claude.json` | Register MCP defaults through Claude's CLI; preserve account state |
| `.config/muse/settings.json` | Merge managed hook settings |
| `.config/BraveSoftware/Brave-Browser/` | Merge profile defaults when Brave is stopped |
| `.config/dconf/user` | Import the text dump with `dconf load /` |
| `.local/bin/codex` | Preserve the CLI in `~/.local/libexec/codex`, then install the wrapper |
| Other files | Symlink into the corresponding home path |

The low-level `link_tree.py` skips files managed by merge/import rules so it
cannot replace live application settings with repository links.

The Codex wrapper overrides `codex` on PATH and retains the project-trust setting.
Setup and `update` install the underlying CLI in `~/.local/libexec`; the wrapper
executes it directly. User Codex settings stay local, with shared defaults in
`root/etc/codex/config.toml`.

System deployment copies networkd files, sudoers files, and `/etc/issue`.
Console variants live in `root/etc/vconsole/`; the matching configuration is
selected for `/etc/vconsole.conf`. The complete setup remains in
`install/0-preconfig` through `install/4-config`.

Everyday commands such as `work`, `ssh-desktop`, and `start-genymotion-emulator`
live in `home/.local/bin/`. Tmux initialization lives in `home/.config/tmux/start`;
sourced shell functions and hardware checks live in `home/.config/shell/common/`.
Brave's startup configuration helper and its support module live in
`home/.local/libexec/`, so browser startup does not import setup code.

Completion generation, environment initialization, SSH key generation, hosts
configuration, and swap configuration live in `install/`. Build recipes live
under `src/`; setup and `update` both call `src/neovim/build`.

## Packages

Wayland

- WM: sway
- Bar: waybar
- Terminal: foot

X11

- WM: i3
- Bar: polybar
- Terminal: st

Common

- Editor:
  nvim,
  vim (minimal config),
  vscode
- Browsers: brave, librewolf
- Terminal: alacritty, kitty
- Filemanager: nautilus
- Notifications: dunst
- Shell: zsh
- Screenshot: flameshot

Also:

- git config
- fzf, fd, rg
- various helper scripts
