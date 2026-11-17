#!/usr/bin/env python3
"""Merge Flameshot defaults while preserving local settings and deletions."""

import configparser
import fcntl
import os
from pathlib import Path
import sys
import tempfile

from configure_claude import merge


def ini_parser():
    # Qt keys are case-sensitive, and values may contain literal % signs or colons.
    parser = configparser.ConfigParser(interpolation=None, delimiters=('=',), default_section='')
    parser.optionxform = str
    return parser


def read_ini(path, *, missing_ok=True):
    try:
        contents = path.read_text()
    except FileNotFoundError:
        if not missing_ok or path.is_symlink():
            raise
        return {}
    parser = ini_parser()
    try:
        parser.read_string(contents, source=str(path))
    except configparser.Error as error:
        raise ValueError(f'Invalid Flameshot settings in {path}: {error}') from error
    return {section: dict(parser[section]) for section in parser.sections()}


def write_ini(path, value, *, backup=False):
    if path.is_file() and not path.is_symlink() and read_ini(path) == value:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    if backup and path.exists():
        descriptor, saved = tempfile.mkstemp(
            prefix=f'{path.name}.before-dotfiles.', suffix='.bak', dir=path.parent,
        )
        with os.fdopen(descriptor, 'wb') as output:
            output.write(path.read_bytes())
        print(f'Previous Flameshot settings backed up to {saved}')
    parser = ini_parser()
    parser.read_dict(value)
    descriptor, temporary = tempfile.mkstemp(prefix=f'.{path.name}.', dir=path.parent)
    try:
        with os.fdopen(descriptor, 'w') as output:
            parser.write(output, space_around_delimiters=False)
            output.flush()
            os.fsync(output.fileno())
        os.replace(temporary, path)
    finally:
        Path(temporary).unlink(missing_ok=True)


def install(settings_source, config_dir, state_dir):
    if config_dir.is_symlink():
        raise ValueError(f'Flameshot settings need a real directory: {config_dir}')
    defaults = read_ini(settings_source, missing_ok=False)
    previous = read_ini(state_dir / 'flameshot.ini')
    settings_path = config_dir / 'flameshot.ini'
    current = read_ini(settings_path)
    updated = merge(previous, defaults, current) if settings_path.exists() else defaults
    if read_ini(settings_path) != current:
        raise RuntimeError('Flameshot settings changed during installation; rerun the installer')
    write_ini(settings_path, updated, backup=True)
    # Advance the baseline only after the local config has been written.
    write_ini(state_dir / 'flameshot.ini', defaults)
    print(f'Flameshot defaults applied; local settings stay in {settings_path}.')


def main(source=None):
    source = source or Path(__file__).resolve().parent.parent / 'home'
    config_root = Path(os.environ.get('XDG_CONFIG_HOME') or Path.home() / '.config').expanduser()
    state_root = Path(os.environ.get('XDG_STATE_HOME') or Path.home() / '.local/state').expanduser()
    state_dir = state_root / 'dotfiles/flameshot'
    state_dir.mkdir(parents=True, exist_ok=True)
    with (state_dir / 'install.lock').open('a') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        install(source / '.config/flameshot/flameshot.ini', config_root / 'flameshot', state_dir)


if __name__ == '__main__':
    try:
        main()
    except (OSError, ValueError, RuntimeError) as error:
        print(f'Flameshot installation failed: {error}', file=sys.stderr)
        sys.exit(1)
