"""Serialize pane hooks and send tmux only changed options, in one command."""

import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess


class PaneOptions:
    def __init__(self, agent):
        self.socket = os.environ.get("TMUX", "").rsplit(",", 2)[0]
        self.pane = os.environ.get("TMUX_PANE", "")
        if not self.socket or not re.fullmatch(r"%\d+", self.pane):
            raise ValueError("Not in a tmux pane")
        info = os.stat(self.socket)
        # Pane IDs repeat after a server restart, so the pathname alone is unsafe.
        identity = f"{self.socket}:{info.st_ino}:{info.st_ctime_ns}"
        server = hashlib.sha256(identity.encode()).hexdigest()[:16]
        runtime = Path(os.environ.get("XDG_RUNTIME_DIR") or Path.home() / ".cache")
        directory = runtime / "dotfiles-tmux"
        directory.mkdir(mode=0o700, parents=True, exist_ok=True)
        self.path = directory / f"{server}-{self.pane[1:]}-{agent}.json"
        self.lock_path = self.path.with_suffix(".lock")

    def __enter__(self):
        self.lock = os.fdopen(os.open(self.lock_path, os.O_CREAT | os.O_RDWR, 0o600), "a")
        fcntl.flock(self.lock, fcntl.LOCK_EX)
        try:
            self.values = json.loads(self.path.read_text())
        except (FileNotFoundError, ValueError):
            self.values = {}
        if not isinstance(self.values, dict):
            self.values = {}
        self.bootstrapped = False
        return self

    def __exit__(self, *exception):
        self.lock.close()

    def tmux(self, *args):
        return subprocess.run(
            ["tmux", "-S", self.socket, *args], check=True,
            capture_output=True, text=True, timeout=0.5,
        ).stdout.rstrip("\n")

    def get(self, name):
        if name not in self.values:
            # Bootstrap an already running session once; later hooks use the cache.
            self.values[name] = self.tmux("show-option", "-pqv", "-t", self.pane, name) or None
            self.bootstrapped = True
        return self.values[name]

    def apply(self, values):
        commands = []
        for name, value in values.items():
            if name in self.values and self.values[name] == value:
                continue
            if commands:
                commands.append(";")
            commands.extend(["set-option", "-pu" if value is None else "-p", "-t", self.pane, name])
            if value is not None:
                commands.append(value)
        if not commands and not self.bootstrapped:
            return
        if commands:
            self.tmux(*commands)
        self.values.update(values)
        temporary = self.path.with_suffix(".tmp")
        try:
            with os.fdopen(os.open(temporary, os.O_CREAT | os.O_TRUNC | os.O_WRONLY, 0o600), "w") as output:
                json.dump(self.values, output, separators=(",", ":"))
            temporary.replace(self.path)
        finally:
            temporary.unlink(missing_ok=True)
