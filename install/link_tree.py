#!/usr/bin/env python3
"""Merge a source tree into a destination using links, preserving local files."""

import argparse
from pathlib import Path
import os
import shutil
import subprocess
import sys
import tempfile


# Destination-relative exceptions to ordinary symlinking. Keep policy beside
# the code that applies it; application handlers live in deploy-home.
DEPLOYMENT_RULES = {
    'home': {
        'merge': {
            '.claude/settings.json': 'claude',
            '.claude.json': 'claude',
            '.config/muse/settings.json': 'muse',
            '.config/BraveSoftware': 'brave',
        },
        'import': {'.config/dconf/user': 'dconf'},
        'wrap': {'.local/bin/codex': '.local/libexec/codex'},
        'mkdir': {
            '.config/nable': 0o700,
            '.config/nable/huly-agent': 0o700,
        },
    },
    'root': {
        'copy': {
            'etc/systemd/network': 0o644,
            'etc/sudoers.d': 0o440,
            'etc/issue': 0o644,
        },
    },
}


def deployment_rules(source):
    """Find rules for a home/ or root/ tree, including individual source files."""
    source = Path(source).absolute()
    for tree in (source, *source.parents):
        installer = tree.parent / 'install/link_tree.py'
        if tree.name in DEPLOYMENT_RULES and installer.is_file():
            return tree, DEPLOYMENT_RULES[tree.name]
    return source, {}


def backup(path, *, dry_run=False):
    saved = path.with_name(path.name + '.before-dotfiles')
    suffix = 1
    while os.path.lexists(saved):
        saved = path.with_name(f'{path.name}.before-dotfiles.{suffix}')
        suffix += 1
    print(f'Backup: {path} -> {saved}')
    if not dry_run:
        path.rename(saved)
    return saved


def replace_link(source, destination, *, dry_run=False):
    print(f'Link: {destination} -> {source}')
    if dry_run:
        return
    destination.parent.mkdir(parents=True, exist_ok=True)
    # Rename a temporary link so a replaced link is never briefly missing.
    with tempfile.TemporaryDirectory(prefix='.dotfiles-', dir=destination.parent) as directory:
        temporary = Path(directory) / 'link'
        temporary.symlink_to(source, target_is_directory=source.is_dir())
        os.replace(temporary, destination)


def preserve_executable(source, destination, original, *, dry_run=False):
    """Keep the installed command available before its wrapper replaces it."""
    if not destination.exists() or destination.resolve() == source.resolve():
        return
    if original.exists() and original.resolve() == destination.resolve():
        return
    if destination.is_symlink():
        if os.path.lexists(original):
            backup(original, dry_run=dry_run)
        target = Path(os.path.abspath(destination.parent / destination.readlink()))
        replace_link(target, original, dry_run=dry_run)
    else:
        print(f'Preserve CLI: {destination} -> {original}')
        if os.path.lexists(original):
            backup(original, dry_run=dry_run)
        if not dry_run:
            original.parent.mkdir(parents=True, exist_ok=True)
            destination.rename(original)


def detach_config(path, *, dry_run=False):
    """Keep a mutable application config independent of its previous source."""
    if not path.is_symlink():
        return
    print(f'Keep local config as a regular file: {path}')
    if dry_run:
        return
    descriptor, temporary = tempfile.mkstemp(prefix='.dotfiles-', dir=path.parent)
    os.close(descriptor)
    try:
        shutil.copy2(path, temporary)
        os.replace(temporary, path)
    finally:
        Path(temporary).unlink(missing_ok=True)


def create_local_directory(destination, relative, mode, *, dry_run=False):
    """Create a missing local directory without following or replacing links."""
    path = destination
    for part in Path(relative).parts:
        path = path / part
        if path.is_symlink() or (path.exists() and not path.is_dir()):
            raise ValueError(f'Local directory requires a real directory: {path}')
    if path.is_dir():
        return
    print(f'Create local directory: {path}')
    if not dry_run:
        path.mkdir(mode=mode, parents=True)


def deploy(source, destination, *, dry_run=False, copies=None, planned_links=None):
    """Merge directories; back up conflicting files before linking or copying."""
    source = Path(source).absolute()
    destination = Path(destination).absolute()
    tree, rules = deployment_rules(source)
    copies = rules.get('copy', {}) if copies is None else copies
    planned_links = planned_links or {}
    if not source.exists():
        raise ValueError(f'Source does not exist: {source}')
    if source == destination:
        raise ValueError('Source and destination must differ')
    if destination.is_relative_to(source):
        raise ValueError('Destination cannot be inside the source tree')

    # Deploy repository content without generated caches or ignored local state.
    allowed = None
    if source.is_dir() and shutil.which('git'):
        result = subprocess.run(
            ['git', '-C', str(source), 'ls-files', '--cached', '--others',
             '--exclude-standard', '-z', '--', '.'], capture_output=True,
        )
        if result.returncode == 0:
            allowed = {source}
            for name in result.stdout.split(b'\0'):
                if name:
                    path = source / os.fsdecode(name)
                    allowed.add(path)
                    allowed.update(parent for parent in path.parents if parent.is_relative_to(source))

    managed = [name for method in ('merge', 'import', 'mkdir') for name in rules.get(method, {})]
    destination_tree = destination
    for _ in source.relative_to(tree).parts:
        destination_tree = destination_tree.parent
    for name, mode in rules.get('mkdir', {}).items():
        if (tree / name).is_relative_to(source):
            create_local_directory(destination_tree, name, mode, dry_run=dry_run)

    def visit(current, target, relative):
        policy_path = current.relative_to(tree).as_posix()
        if any(policy_path == name or policy_path.startswith(name + '/') for name in managed):
            return
        if policy_path in rules.get('wrap', {}):
            original = destination_tree / rules['wrap'][policy_path]
            preserve_executable(current, target, original, dry_run=dry_run)
        copy = next((mode for name, mode in copies.items()
                     if policy_path == name or policy_path.startswith(name + '/')), None)
        if dry_run and planned_links.get(target) == current and copy is None:
            return
        if current.is_dir() and not current.is_symlink():
            if target.is_symlink():
                if target.resolve() == current.resolve() and copy is None:
                    if any(name.startswith(policy_path + '/') for name in managed):
                        raise ValueError(f'Managed settings need a real directory: {target}; run install/migrate')
                    return
                raise ValueError(f'Directory is a different symlink: {target}; run install/migrate for old dotfiles links')
            if target.exists() and not target.is_dir():
                backup(target, dry_run=dry_run)
            if not dry_run:
                target.mkdir(parents=True, exist_ok=True)
            for child in sorted(current.iterdir()):
                if child.name in {'.git', '__pycache__', 'node_modules'}:
                    continue
                if allowed is not None and child not in allowed:
                    continue
                visit(child, target / child.name, f'{relative}/{child.name}'.lstrip('/'))
            return

        if copy is not None:
            if target.is_file() and not target.is_symlink() and target.read_bytes() == current.read_bytes():
                if not dry_run:
                    target.chmod(copy)
                return
            if os.path.lexists(target):
                backup(target, dry_run=dry_run)
            print(f'Copy: {current} -> {target}')
            if not dry_run:
                target.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(current, target)
                target.chmod(copy)
            return

        if target.is_symlink() and target.resolve() == current.resolve():
            return
        if os.path.lexists(target):
            backup(target, dry_run=dry_run)
        replace_link(current, target, dry_run=dry_run)

    visit(source, destination, '')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--dry-run', action='store_true')
    parser.add_argument('source', type=Path)
    parser.add_argument('destination', type=Path)
    args = parser.parse_args()
    deploy(args.source, args.destination, dry_run=args.dry_run)


if __name__ == '__main__':
    try:
        main()
    except (OSError, ValueError) as error:
        print(f'Deployment failed: {error}', file=sys.stderr)
        sys.exit(1)
