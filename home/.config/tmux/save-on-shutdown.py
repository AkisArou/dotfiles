"""Save tmux on logind's shutdown notification, before login sessions are stopped."""

import os
from pathlib import Path
import signal
import subprocess
import sys

from gi.repository import Gio, GLib


class ShutdownSaver:
    def __init__(self, proxy):
        self.proxy = proxy
        self.inhibitor = None
        self.command = ["/usr/bin/bash", str(Path.home() / ".config/tmux/save-session")]

    def acquire(self):
        if self.inhibitor is not None:
            return
        result, descriptors = self.proxy.call_with_unix_fd_list_sync(
            "Inhibit",
            GLib.Variant("(ssss)", (
                "shutdown", "tmux snapshot", "Save tmux sessions before shutdown", "delay",
            )),
            Gio.DBusCallFlags.NONE, 5000, None, None,
        )
        self.inhibitor = descriptors.get(result.unpack()[0])

    def release(self):
        if self.inhibitor is not None:
            os.close(self.inhibitor)
            self.inhibitor = None

    def save(self):
        delay = self.proxy.get_cached_property("InhibitDelayMaxUSec")
        seconds = delay.unpack() / 1_000_000 if delay is not None else 5
        timeout = max(0.1, seconds - 0.5)
        process = subprocess.Popen(self.command, start_new_session=True, stdin=subprocess.DEVNULL)
        try:
            code = process.wait(timeout=timeout)
            if code:
                print(f"tmux snapshot failed (exit {code}).", file=sys.stderr, flush=True)
            else:
                print("tmux shutdown snapshot completed.", flush=True)
        except subprocess.TimeoutExpired:
            # Stop the whole save process group, including flock and plugin children.
            try:
                os.killpg(process.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            try:
                process.wait(timeout=0.2)
            except subprocess.TimeoutExpired:
                pass
            # A child can outlive flock, so also terminate any remaining children.
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            process.wait()
            print("tmux snapshot exceeded the shutdown delay.", file=sys.stderr, flush=True)

    def on_signal(self, _proxy, _sender, name, parameters):
        if name != "PrepareForShutdown":
            return
        if parameters.unpack()[0]:
            try:
                self.save()
            except OSError as error:
                print(f"tmux snapshot failed: {error}", file=sys.stderr, flush=True)
            finally:
                self.release()
        else:
            # Rearm if the reboot or shutdown was cancelled.
            self.acquire()


def main():
    proxy = Gio.DBusProxy.new_for_bus_sync(
        Gio.BusType.SYSTEM, Gio.DBusProxyFlags.NONE, None,
        "org.freedesktop.login1", "/org/freedesktop/login1",
        "org.freedesktop.login1.Manager", None,
    )
    saver = ShutdownSaver(proxy)
    proxy.connect("g-signal", saver.on_signal)
    saver.acquire()
    print("Watching for shutdown/reboot; tmux is not started by this service.", flush=True)
    try:
        GLib.MainLoop().run()
    finally:
        saver.release()


if __name__ == "__main__":
    main()
