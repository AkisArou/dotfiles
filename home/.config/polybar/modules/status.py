#!/usr/bin/env python3
"""Waybar-style modules that Polybar cannot provide through its built-ins."""

import argparse
import configparser
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import time


CONFIG = Path(__file__).resolve().parents[1]
ENV = dict(os.environ, LC_ALL="C")


def load_colours():
    config = configparser.ConfigParser(interpolation=None)
    config.read([CONFIG / "theme-dark.ini", CONFIG / "theme.ini"])
    return dict(config["colors"])


COLOURS = load_colours()


def run(*command):
    try:
        return subprocess.check_output(
            command, text=True, stderr=subprocess.DEVNULL, timeout=3, env=ENV
        ).strip()
    except (OSError, subprocess.SubprocessError):
        return ""


def read_json(*command, default=None):
    try:
        return json.loads(run(*command))
    except ValueError:
        return default


def colour(text, name):
    return f"%{{F{COLOURS[name]}}}{text}%{{F-}}"


def literal(text):
    # Device aliases must not inject Polybar formatting or extra output lines.
    return " ".join(text.splitlines()).replace("%{", "% {")


def volume(device):
    channels = device.get("volume", {}).values()
    return int(sum(channel["value"] for channel in channels) / max(len(channels), 1) / 65536 * 100 + 0.5)


def audio_text(info, sinks, sources):
    sink = next((device for device in sinks if device["name"] == info.get("default_sink_name")), None)
    source = next((device for device in sources if device["name"] == info.get("default_source_name")), None)
    if sink is None:
        return colour("VOL", "primary-subtle") + " unavailable"

    properties = sink.get("properties", {})
    form = properties.get("device.form_factor", "")
    port = sink.get("active_port") or ""
    if "headphone" in port.lower():
        form = "headphone"
    elif "headset" in port.lower():
        form = "headset"
    icon = {"headphone": "", "hands-free": "", "headset": "",
            "phone": "", "portable": "", "car": ""}.get(form, "VOL")
    bluetooth = properties.get("device.bus") == "bluetooth" or sink["name"].startswith("bluez_")
    microphone = "" if source is None else ("" if source.get("mute") else f"{volume(source)}% ")

    if bluetooth:
        output = "" if sink.get("mute") else f"{volume(sink)}%"
        return f"{output} {icon} {microphone}".rstrip()
    if sink.get("mute"):
        return f" {microphone}".rstrip()
    return f"{colour(icon, 'primary-subtle')} {volume(sink)}%"


def audio():
    return audio_text(
        read_json("pactl", "-f", "json", "info", default={}),
        read_json("pactl", "-f", "json", "list", "sinks", default=[]),
        read_json("pactl", "-f", "json", "list", "sources", default=[]),
    )


def watch_audio():
    previous = None
    while True:
        # Subscribe before reading the initial state, so no changes are missed.
        try:
            events = subprocess.Popen(
                ["pactl", "subscribe"], stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL, text=True, env=ENV,
            )
        except OSError:
            print(audio(), flush=True)
            time.sleep(5)
            continue
        try:
            current = audio()
            if current != previous:
                print(current, flush=True)
                previous = current
            for event in events.stdout:
                if re.search(r" on (sink|source|server|card) #", event):
                    current = audio()
                    if current != previous:
                        print(current, flush=True)
                        previous = current
        finally:
            events.terminate()
            try:
                events.wait(timeout=1)
            except subprocess.TimeoutExpired:
                events.kill()
                events.wait()
        time.sleep(2)


def battery_state(root=Path("/sys/class/power_supply")):
    for battery in sorted(root.glob("BAT*")):
        try:
            return int((battery / "capacity").read_text()), (battery / "status").read_text().strip()
        except (OSError, ValueError):
            continue
    return None


def battery_text(capacity, status, flash=False):
    if capacity <= 15 and status != "Charging":
        critical = capacity <= 5
        foreground = COLOURS["alert" if critical else "warning"]
        background = COLOURS["critical-background" if critical else "warning-background"]
        if critical and flash:
            background = COLOURS["blink-background"]
        return f"%{{B{background}}}%{{F{foreground}}} BAT {capacity}% %{{F-}}%{{B-}}"
    return f"{colour('BAT', 'primary')} {capacity}%"


def battery():
    state = battery_state()
    return battery_text(*state) if state else ""


def watch_battery():
    previous, flash = None, False
    while True:
        state = battery_state()
        current = battery_text(*state, flash=flash) if state else ""
        if current != previous:
            print(current, flush=True)
            previous = current
        critical = state is not None and state[0] <= 5 and state[1] != "Charging"
        time.sleep(0.5 if critical else 5)
        flash = not flash if critical else False


def bluetooth_properties(output):
    return dict(re.findall(r"^\s*([\w ]+): (.*)$", output, re.MULTILINE))


def bluetooth():
    controller = bluetooth_properties(run("bluetoothctl", "show"))
    label = colour("BT", "primary-subtle")
    if "Powered" not in controller:
        return label + " no-controller"
    if controller["Powered"] != "yes":
        return label + " off"

    devices = []
    for line in run("bluetoothctl", "devices", "Connected").splitlines():
        fields = line.split(maxsplit=2)
        if len(fields) < 3 or fields[0] != "Device":
            continue
        address, name = fields[1:]
        properties = bluetooth_properties(run("bluetoothctl", "info", address))
        if properties.get("Connected") == "yes":
            devices.append((address, properties.get("Alias", name), properties))
    if not devices:
        return label + " on"

    # Keep the active headphones visible when a mouse is also connected.
    sink = run("pactl", "get-default-sink").upper().replace("_", ":")
    selected = next((device for device in devices if device[0].upper() in sink), devices[0])
    _, alias, properties = selected
    battery = re.search(r"\((\d+)\)", properties.get("Battery Percentage", ""))
    return literal(alias) + (f" {battery[1]}%" if battery else "")


def scratchpad_count(tree):
    if tree.get("name") == "__i3_scratch":
        return len(tree.get("nodes", [])) + len(tree.get("floating_nodes", []))
    return sum(scratchpad_count(child) for key in ("nodes", "floating_nodes") for child in tree.get(key, []))


def scratchpad():
    count = scratchpad_count(read_json("i3-msg", "-t", "get_tree", default={}))
    return f" {count}" if count else ""


def systemd_failed():
    counts = [run("systemctl", *scope, "show", "--property=NFailedUnits", "--value")
              for scope in ((), ("--user",))]
    count = sum(int(value) for value in counts if value.isdigit())
    return f"✗ Systemd failed units: {count}" if count else ""


def main():
    modules = {"audio": audio, "battery": battery, "bluetooth": bluetooth,
               "scratchpad": scratchpad, "systemd-failed": systemd_failed}
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("module", choices=modules)
    parser.add_argument("--watch", action="store_true")
    arguments = parser.parse_args()
    if arguments.watch:
        watchers = {"audio": watch_audio, "battery": watch_battery}
        if arguments.module not in watchers:
            parser.error("--watch is supported only for audio and battery")
        watchers[arguments.module]()
    else:
        print(modules[arguments.module]())


if __name__ == "__main__":
    signal.signal(signal.SIGTERM, lambda *_: sys.exit(0))
    try:
        main()
    except (KeyboardInterrupt, BrokenPipeError):
        pass
