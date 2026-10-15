#!/usr/bin/env python3
"""Install Muse's tmux hook settings while preserving the user's other settings."""

import fcntl
import json
import os
from pathlib import Path
import sys
import tempfile


def install(settings_source):
    defaults = json.loads(settings_source.read_text())
    directory = Path(os.environ.get("XDG_CONFIG_HOME") or Path.home() / ".config") / "muse"
    directory.mkdir(parents=True, exist_ok=True)
    path = directory / "settings.json"
    with (directory / ".settings.json.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        original = path.read_bytes() if path.exists() else b"{}"
        current = json.loads(original)
        if not isinstance(current, dict) or current.get("schema_version", 1) != 1:
            raise ValueError("Expected Muse settings schema version 1")
        managed = current.get("managed_hooks_path")
        if managed and managed != defaults["managed_hooks_path"]:
            raise ValueError("Muse already uses a different managed hooks file; keep it and combine the hooks before installing")
        variables = current.get("managed_hooks_env_vars", [])
        if not isinstance(variables, list) or not all(isinstance(name, str) for name in variables):
            raise ValueError("managed_hooks_env_vars must be an array of names")
        updated = {**current, **defaults}
        # Ordinary Muse hooks omit TMUX. Only managed hooks can forward these names.
        updated["managed_hooks_env_vars"] = list(dict.fromkeys([*variables, *defaults["managed_hooks_env_vars"]]))
        if updated == current and not path.is_symlink():
            print("Muse tmux hooks already configured.")
            return
        if path.exists():
            descriptor, backup = tempfile.mkstemp(prefix="settings.json.before-dotfiles.", suffix=".bak", dir=directory)
            with os.fdopen(descriptor, "wb") as output:
                output.write(original)
            print(f"Previous Muse settings backed up to {backup}")
        descriptor, temporary = tempfile.mkstemp(prefix=".settings.json.", dir=directory)
        try:
            with os.fdopen(descriptor, "w") as output:
                json.dump(updated, output, indent=2)
                output.write("\n")
            os.replace(temporary, path)
        finally:
            Path(temporary).unlink(missing_ok=True)
        print(f"Muse tmux hooks configured in {path}.")


if __name__ == "__main__":
    try:
        if os.geteuid() == 0:
            sys.exit('Run this installer as your normal user.')
        install(Path(__file__).resolve().parent.parent / 'home/.config/muse/settings.json')
    except (OSError, ValueError) as error:
        print(f"Muse installation failed: {error}", file=sys.stderr)
        sys.exit(1)
