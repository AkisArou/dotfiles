import fcntl
import hashlib
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path


MISSING = object()


def same(left, right):
    if left is MISSING or right is MISSING:
        return left is right
    # JSON distinguishes booleans and numbers, unlike Python's True == 1.
    return json.dumps(left, sort_keys=True) == json.dumps(right, sort_keys=True)


def merge(previous, defaults, local):
    """Update inherited values while preserving local edits and deletions."""
    if same(local, previous):
        return defaults
    if (
        isinstance(local, dict)
        and (previous is MISSING or isinstance(previous, dict))
        and (defaults is MISSING or isinstance(defaults, dict))
    ):
        previous = {} if previous is MISSING else previous
        defaults = {} if defaults is MISSING else defaults
        result = {}
        for key in dict.fromkeys([*defaults, *previous, *local]):
            value = merge(
                previous.get(key, MISSING),
                defaults.get(key, MISSING),
                local.get(key, MISSING),
            )
            if value is not MISSING:
                result[key] = value
        return result
    # Lists are one value: an edited list stays intact, including removals.
    return local


def read_json(path, *, missing_ok=True):
    try:
        value = json.loads(path.read_text())
    except FileNotFoundError:
        if not missing_ok or path.is_symlink():
            raise
        return {}
    if not isinstance(value, dict):
        raise ValueError(f'{path} must contain a JSON object')
    return value


def write_json(path, value, *, backup=False):
    if path.is_file() and not path.is_symlink() and same(read_json(path), value):
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    if backup and path.exists():
        fd, backup_name = tempfile.mkstemp(
            prefix=f'{path.name}.before-dotfiles.', suffix='.bak', dir=path.parent
        )
        with os.fdopen(fd, 'wb') as output:
            output.write(path.read_bytes())
        print(f'Previous settings backed up to {backup_name}')
    fd, temporary = tempfile.mkstemp(prefix=f'.{path.name}.', dir=path.parent)
    try:
        with os.fdopen(fd, 'w') as output:
            json.dump(value, output, indent=2, ensure_ascii=False)
            output.write('\n')
            output.flush()
            os.fsync(output.fileno())
        os.replace(temporary, path)
    finally:
        Path(temporary).unlink(missing_ok=True)


def mcp_command(action, name, definition=MISSING):
    command = ['claude', 'mcp', action, '--scope', 'user', name]
    if definition is not MISSING:
        command.append(json.dumps(definition))
    result = subprocess.run(command, capture_output=True, text=True)
    if result.returncode:
        # Do not include argv: MCP definitions may contain credentials.
        raise RuntimeError(f'Claude could not {action} MCP server {name}: {result.stderr.strip()}')


def apply_mcp(previous, defaults, current, state_path):
    for name in dict.fromkeys([*defaults, *previous]):
        old = current.get(name, MISSING)
        new = defaults.get(name, MISSING) if same(old, previous.get(name, MISSING)) else old
        if same(old, new):
            continue
        actual = read_json(state_path).get('mcpServers', {}).get(name, MISSING)
        if not same(actual, old):
            raise RuntimeError(f'MCP server {name} changed during installation; rerun the installer')
        if old is not MISSING:
            mcp_command('remove', name)
        if new is not MISSING:
            try:
                mcp_command('add-json', name, new)
            except Exception:
                if old is not MISSING:
                    mcp_command('add-json', name, old)
                raise
        print(f'MCP server {name}: {"removed" if new is MISSING else "installed"}')


def install(base_dir, config_dir, state_dir, claude_state):
    defaults = read_json(base_dir / 'settings.json', missing_ok=False)
    mcp_defaults = read_json(base_dir / 'mcp.json', missing_ok=False).get('mcpServers')
    if not isinstance(mcp_defaults, dict):
        raise ValueError('mcpServers must contain a JSON object')
    previous = read_json(state_dir / 'settings.json')
    previous_mcp = read_json(state_dir / 'mcp.json').get('mcpServers', {})
    settings_path = config_dir / 'settings.json'
    current = read_json(settings_path)
    # A missing settings file is a fresh install, even with an old snapshot.
    updated = merge(previous, defaults, current) if settings_path.exists() else defaults
    servers = read_json(claude_state).get('mcpServers', {})
    if not isinstance(previous_mcp, dict) or not isinstance(servers, dict):
        raise ValueError('Stored mcpServers must contain a JSON object')

    apply_mcp(previous_mcp, mcp_defaults, servers, claude_state)
    if not same(read_json(settings_path), current):
        raise RuntimeError('Claude settings changed during installation; rerun the installer')
    write_json(settings_path, updated, backup=True)
    # Record the new baseline only after all updates have succeeded.
    write_json(state_dir / 'settings.json', defaults)
    write_json(state_dir / 'mcp.json', {'mcpServers': mcp_defaults})
    print(f'Claude defaults applied; local settings stay in {settings_path}.')


def main():
    base_dir = Path(__file__).resolve().parent
    custom_config = os.environ.get('CLAUDE_CONFIG_DIR')
    config_dir = Path(custom_config).expanduser() if custom_config else Path.home() / '.claude'
    claude_state = config_dir / '.claude.json' if custom_config else Path.home() / '.claude.json'
    state_root = Path(os.environ.get('XDG_STATE_HOME') or Path.home() / '.local/state').expanduser()
    # Different Claude accounts/config directories need independent baselines.
    state_dir = state_root / 'dotfiles/claude'
    if custom_config:
        state_dir /= hashlib.sha256(str(config_dir.resolve()).encode()).hexdigest()[:16]
    state_dir.mkdir(parents=True, exist_ok=True)
    with (state_dir / 'install.lock').open('a') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        install(base_dir, config_dir, state_dir, claude_state)


if __name__ == '__main__':
    try:
        main()
    except (OSError, ValueError, RuntimeError) as error:
        print(f'Claude installation failed: {error}', file=sys.stderr)
        sys.exit(1)
