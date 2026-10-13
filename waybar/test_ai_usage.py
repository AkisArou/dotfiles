"""Regression checks for quota shapes, caching and credential isolation."""

import importlib.util
import io
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import urllib.error
from xml.etree import ElementTree

spec = importlib.util.spec_from_file_location("ai_usage", Path(__file__).with_name("ai-usage.py"))
usage = importlib.util.module_from_spec(spec)
spec.loader.exec_module(usage)


class ProviderTests(unittest.TestCase):
    def test_claude_uses_reported_windows_and_excludes_fable(self):
        result = usage.parse_usage("claude", {
            "five_hour": {"utilization": 0, "resets_at": "2030-01-01T00:00:00Z"},
            "seven_day": {"utilization": 37, "reset_at": "2030-01-07T00:00:00Z"},
            "seven_day_fable": {"utilization": 80},
            "limits": [{"kind": "weekly_scoped", "percent": 90, "scope": {"model": {"display_name": "Fable"}}}],
        }, "Max 20x", 1000)
        self.assertEqual([row["label"] for row in result["metrics"]], ["5-hour", "Weekly"])
        self.assertEqual(result["metrics"][0]["percent"], 0)
        self.assertIsNotNone(result["metrics"][1]["reset_at"])

    def test_openai_classifies_weekly_primary_without_inventing_five_hours(self):
        result = usage.parse_usage("openai", {
            "plan_type": "prolite",
            "rate_limit": {"primary_window": {"used_percent": 11, "limit_window_seconds": 604800, "reset_at": 2000}, "secondary_window": None},
        }, "", 1000)
        self.assertEqual(result["plan"], "Prolite")
        self.assertEqual(result["metrics"], [{"label": "Weekly", "percent": 11, "reset_at": 2000}])

    def test_openai_relative_reset_and_missing_window(self):
        result = usage.parse_usage("openai", {
            "rate_limit": {"primary_window": None, "secondary_window": {"used_percent": "42.5", "reset_after_seconds": 3600}},
        }, "", 1000)
        self.assertEqual(result["metrics"], [{"label": "Weekly", "percent": 42.5, "reset_at": 4600}])

    def test_muse_keeps_only_usage_and_plan(self):
        result = usage.parse_usage("muse", {
            "api_key": "SECRET-KEY", "user_email": "PRIVATE-EMAIL", "payment_method": {"id": "PRIVATE-PAYMENT"},
            "is_subs_active": True, "subs_tier_name": "Muse Code Power Usage",
            "subs_usage": {"window": {"used_percent": 2, "resets_at": 2000}, "weekly": {"used_percent": 9, "resets_at": 3000}},
        }, "", 1000)
        self.assertEqual(result["plan"], "Power Usage")
        self.assertEqual([row["percent"] for row in result["metrics"]], [2, 9])
        self.assertNotIn("SECRET", json.dumps(result))
        self.assertNotIn("PRIVATE", json.dumps(result))

    def test_missing_muse_usage_is_unavailable_not_zero(self):
        result = usage.parse_usage("muse", {"is_subs_active": True, "subs_tier_name": "Power"}, "", 1000)
        self.assertEqual(result["metrics"], [])
        self.assertIn("did not report", result["note"])
        self.assertEqual(usage.parse_usage("muse", {"is_subs_active": False}, "", 1000)["metrics"], [])

    def test_invalid_percent_and_reset_are_rejected(self):
        for value in (float("nan"), float("inf"), -1, True):
            with self.subTest(value=value), self.assertRaises(usage.UsageError):
                usage.window("Weekly", {"used_percent": value}, "used_percent", 1000)
        with self.assertRaises(usage.UsageError):
            usage.window("Weekly", {"used_percent": 1, "reset_after_seconds": float("inf")}, "used_percent", 1000)

    def test_http_errors_do_not_expose_remote_body_or_credentials(self):
        for status in (401, 429, 500, 302):
            failure = urllib.error.HTTPError("https://provider.invalid", status, "PRIVATE", {}, io.BytesIO(b"SECRET-KEY"))
            with self.subTest(status=status), patch.object(usage.HTTP, "open", side_effect=failure), self.assertRaises(usage.UsageError) as caught:
                usage.fetch_json("https://provider.invalid", {"Authorization": "SECRET-TOKEN"}, None)
            self.assertNotIn("SECRET", str(caught.exception))
            self.assertNotIn("PRIVATE", str(caught.exception))

    def test_tooltip_escapes_markup_and_keeps_robot_separate_from_usage(self):
        report = {"entries": [{"id": "claude", "plan": "Max <20x> & friends", "metrics": [{"label": "Weekly", "percent": 19, "reset_at": 5000}]}]}
        text = usage.tooltip(report)
        ElementTree.fromstring("<tooltip>" + text + "</tooltip>")
        self.assertIn("&lt;20x&gt; &amp; friends", text)
        self.assertIn("█", text)
        self.assertIn("Meta / Muse", text)
        self.assertEqual(usage.countdown(1000, 1000), "now")
        self.assertEqual(usage.countdown(4660, 1000), "1h 01m")


class CacheTests(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.directory = Path(directory.name)
        environment = patch.dict(usage.os.environ, {"XDG_CACHE_HOME": directory.name})
        environment.start()
        self.addCleanup(environment.stop)
        self.credentials = patch.object(usage, "credentials", return_value=("https://provider.invalid", {"Authorization": "SECRET-TOKEN"}, None, "Max", "identity-one"))
        self.credentials.start()
        self.addCleanup(self.credentials.stop)
        self.payload = {"five_hour": {"utilization": 19, "resets_at": "2030-01-01T00:00:00Z"}}

    def test_fresh_cache_prevents_duplicate_requests_and_stores_no_secrets(self):
        with patch.object(usage, "fetch_json", return_value=self.payload) as fetch, patch.object(usage.time, "time", return_value=1000):
            self.assertEqual(usage.load_provider("claude"), usage.load_provider("claude"))
        fetch.assert_called_once()
        filename = self.directory / "waybar-ai-usage/claude.json"
        self.assertNotIn("SECRET", filename.read_text())
        self.assertEqual(filename.stat().st_mode & 0o777, 0o600)

    def test_failed_refresh_retains_usage_and_backs_off(self):
        with patch.object(usage, "fetch_json", return_value=self.payload), patch.object(usage.time, "time", return_value=1000):
            usage.load_provider("claude")
        with patch.object(usage, "fetch_json", side_effect=usage.UsageError("Rate limited")) as fetch, patch.object(usage.time, "time", return_value=1400):
            result = usage.load_provider("claude")
            self.assertEqual(usage.load_provider("claude"), result)
        self.assertTrue(result["stale"])
        self.assertEqual(result["metrics"][0]["percent"], 19)
        fetch.assert_called_once()

    def test_changed_login_does_not_reuse_previous_accounts_usage(self):
        with patch.object(usage, "fetch_json", return_value=self.payload), patch.object(usage.time, "time", return_value=1000):
            usage.load_provider("claude")
        with patch.object(usage, "credentials", return_value=("https://provider.invalid", {}, None, "Pro", "identity-two")), patch.object(usage, "fetch_json", side_effect=usage.UsageError("Offline")), patch.object(usage.time, "time", return_value=1001):
            result = usage.load_provider("claude")
        self.assertEqual(result["metrics"], [])
        self.assertFalse(result["stale"])


if __name__ == "__main__":
    unittest.main()
