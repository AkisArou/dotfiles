"""Regression checks for the custom Polybar modules and theme integration."""

import configparser
import copy
import importlib.util
from pathlib import Path
import tempfile
import types
import unittest
from unittest.mock import patch
from unittest.mock import Mock


ROOT = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location("polybar_status", ROOT / "modules/status.py")
status = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(status)


def colours(theme):
    config = configparser.ConfigParser(interpolation=None)
    config.read(ROOT / f"theme-{theme}.ini")
    return dict(config["colors"])


def without_markup(text):
    return status.re.sub(r"%\{[^}]*\}", "", text)


class StatusTests(unittest.TestCase):
    def setUp(self):
        self.theme = patch.object(status, "COLOURS", colours("dark"))
        self.theme.start()
        self.addCleanup(self.theme.stop)

    def test_battery_thresholds_and_charging_override(self):
        for theme in ("light", "dark"):
            with patch.object(status, "COLOURS", colours(theme)):
                for capacity in (100, 16):
                    self.assertNotIn("%{B", status.battery_text(capacity, "Discharging"))
                for capacity in (15, 6):
                    output = status.battery_text(capacity, "Discharging")
                    self.assertIn(status.COLOURS["warning"], output)
                    self.assertIn(status.COLOURS["warning-background"], output)
                for capacity in (5, 0):
                    normal = status.battery_text(capacity, "Discharging")
                    flash = status.battery_text(capacity, "Discharging", flash=True)
                    self.assertIn(status.COLOURS["alert"], normal)
                    self.assertIn(status.COLOURS["critical-background"], normal)
                    self.assertIn(status.COLOURS["blink-background"], flash)
                    self.assertEqual(without_markup(normal), without_markup(flash))
                    self.assertNotIn("%{B", status.battery_text(capacity, "Charging"))

    def test_battery_discovery_and_missing_battery(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.assertIsNone(status.battery_state(root))
            battery = root / "BAT1"
            battery.mkdir()
            (battery / "capacity").write_text("15\n")
            (battery / "status").write_text("Discharging\n")
            self.assertEqual(status.battery_state(root), (15, "Discharging"))

    def test_critical_battery_watch_flashes_background_without_hiding_text(self):
        with patch.object(status, "battery_state", return_value=(5, "Discharging")), \
                patch.object(status.time, "sleep", side_effect=[None, StopIteration]), \
                patch("builtins.print") as output:
            with self.assertRaises(StopIteration):
                status.watch_battery()
        frames = [call.args[0] for call in output.call_args_list]
        self.assertEqual(len(frames), 2)
        self.assertNotEqual(frames[0], frames[1])
        self.assertEqual(without_markup(frames[0]), " BAT 5% ")
        self.assertEqual(without_markup(frames[0]), without_markup(frames[1]))

    def audio_devices(self):
        return (
            {"default_sink_name": "speakers", "default_source_name": "microphone"},
            [{"name": "speakers", "mute": False,
              "volume": {"left": {"value": 32768}, "right": {"value": 32768}}}],
            [{"name": "microphone", "mute": False, "volume": {"mono": {"value": 16384}}}],
        )

    def test_audio_speakers_and_wired_headphones(self):
        info, sinks, sources = self.audio_devices()
        self.assertEqual(without_markup(status.audio_text(info, sinks, sources)), "VOL 50%")
        sinks[0]["active_port"] = "analog-output-headphones"
        self.assertEqual(without_markup(status.audio_text(info, sinks, sources)), " 50%")

    def test_audio_bluetooth_and_microphone_mute(self):
        info, sinks, sources = self.audio_devices()
        sinks[0]["properties"] = {"device.bus": "bluetooth", "device.form_factor": "headphone"}
        self.assertEqual(status.audio_text(info, sinks, sources), "50%  25% ")
        sources[0]["mute"] = True
        sinks[0]["mute"] = True
        self.assertEqual(status.audio_text(info, sinks, sources), "  ")

    def test_audio_muted_speakers_and_missing_source(self):
        info, sinks, sources = self.audio_devices()
        sinks[0]["mute"] = True
        self.assertEqual(status.audio_text(info, sinks, sources), " 25% ")
        self.assertEqual(status.audio_text(info, sinks, []), "")

    def test_audio_follows_default_sink_and_handles_server_loss(self):
        info, sinks, sources = self.audio_devices()
        other = copy.deepcopy(sinks[0])
        other.update(name="other", mute=True)
        info["default_sink_name"] = "other"
        self.assertEqual(status.audio_text(info, sinks + [other], sources), " 25% ")
        self.assertEqual(without_markup(status.audio_text({}, [], [])), "VOL unavailable")

    def test_audio_watch_emits_initial_state_and_follows_sink_and_source_events(self):
        events = Mock()
        events.stdout = ["Event 'new' on client #1\n", "Event 'change' on sink #2\n",
                         "Event 'change' on source #3\n"]
        with patch.object(status.subprocess, "Popen", return_value=events), \
                patch.object(status, "audio", side_effect=["VOL 50%", "VOL 51%", " "]), \
                patch.object(status.time, "sleep", side_effect=StopIteration), \
                patch("builtins.print") as output:
            with self.assertRaises(StopIteration):
                status.watch_audio()
        self.assertEqual([call.args[0] for call in output.call_args_list], ["VOL 50%", "VOL 51%", " "])
        events.terminate.assert_called_once()
        events.wait.assert_called_once()

    def bluetooth_commands(self):
        return {
            ("bluetoothctl", "show"): "Controller 00:00:00:00:00:00\n\tPowered: yes",
            ("bluetoothctl", "devices", "Connected"):
                "Device AA:BB:CC:DD:EE:01 Mouse\nDevice AA:BB:CC:DD:EE:02 Headphones With Spaces",
            ("bluetoothctl", "info", "AA:BB:CC:DD:EE:01"):
                "\tAlias: Mouse\n\tConnected: yes\n\tBattery Percentage: 0x64 (100)",
            ("bluetoothctl", "info", "AA:BB:CC:DD:EE:02"):
                "\tAlias: Headphones With Spaces\n\tConnected: yes\n\tBattery Percentage: 0x3c (60)",
            ("pactl", "get-default-sink"): "bluez_output.AA_BB_CC_DD_EE_02.1",
        }

    def test_bluetooth_selects_active_headphones_among_multiple_devices(self):
        commands = self.bluetooth_commands()
        with patch.object(status, "run", side_effect=lambda *command: commands[command]):
            self.assertEqual(status.bluetooth(), "Headphones With Spaces 60%")

    def test_bluetooth_without_battery_and_without_audio_sink(self):
        commands = self.bluetooth_commands()
        commands[("pactl", "get-default-sink")] = "speakers"
        commands[("bluetoothctl", "info", "AA:BB:CC:DD:EE:01")] = "\tAlias: Mouse\n\tConnected: yes"
        with patch.object(status, "run", side_effect=lambda *command: commands[command]):
            self.assertEqual(status.bluetooth(), "Mouse")

    def test_bluetooth_controller_states_and_disconnect_race(self):
        commands = self.bluetooth_commands()
        for address in ("AA:BB:CC:DD:EE:01", "AA:BB:CC:DD:EE:02"):
            commands[("bluetoothctl", "info", address)] = "Connected: no"
        with patch.object(status, "run", side_effect=lambda *command: commands[command]):
            self.assertEqual(without_markup(status.bluetooth()), "BT on")
            commands[("bluetoothctl", "show")] = "Powered: no"
            self.assertEqual(without_markup(status.bluetooth()), "BT off")
            commands[("bluetoothctl", "show")] = "No default controller available"
            self.assertEqual(without_markup(status.bluetooth()), "BT no-controller")

    def test_scratchpad_counts_containers_without_counting_child_windows_twice(self):
        tree = {"nodes": [{"nodes": [{"name": "__i3_scratch", "nodes": [],
                                     "floating_nodes": [{"nodes": [{"window": 123}]}, {"window": 456}]}]}]}
        self.assertEqual(status.scratchpad_count(tree), 2)
        with patch.object(status, "read_json", return_value=tree):
            self.assertEqual(status.scratchpad(), " 2")
        with patch.object(status, "read_json", return_value={}):
            self.assertEqual(status.scratchpad(), "")

    def test_systemd_combines_scopes_and_hides_when_healthy(self):
        with patch.object(status, "run", side_effect=["2", "1"]):
            self.assertEqual(status.systemd_failed(), "✗ Systemd failed units: 3")
        with patch.object(status, "run", side_effect=["0", "0"]):
            self.assertEqual(status.systemd_failed(), "")
        with patch.object(status, "run", side_effect=["1", ""]):
            self.assertEqual(status.systemd_failed(), "✗ Systemd failed units: 1")

    def test_device_alias_cannot_add_markup_or_output_lines(self):
        self.assertEqual(status.literal("Name %{A:command:}\nextra"), "Name % {A:command:} extra")


class ThemeTests(unittest.TestCase):
    def test_theme_switch_links_polybar_variant_and_restarts_it(self):
        theme = types.ModuleType("desktop_theme")
        path = ROOT.parents[1] / ".local/bin/theme"
        exec(compile(path.read_text(), str(path), "exec"), theme.__dict__)
        with tempfile.TemporaryDirectory() as directory:
            home = Path(directory)
            (home / ".config/sway").mkdir(parents=True)
            for variant in ("light", "dark"):
                (home / f".config/sway/theme-{variant}").write_text("")
            with patch.object(theme.Path, "home", return_value=home), \
                    patch.object(theme, "STATE", home / "state"), \
                    patch.object(theme, "run") as run:
                for variant in ("light", "dark"):
                    theme.apply(variant, force=True)
                    self.assertEqual((home / ".config/polybar/theme.ini").readlink(), Path(f"theme-{variant}.ini"))
                    run.assert_any_call("polybar-msg", "cmd", "restart")


if __name__ == "__main__":
    unittest.main()
