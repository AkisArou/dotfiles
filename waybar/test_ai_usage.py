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

    def test_reset_credits_exclude_redemption_ids_and_used_credits(self):
        result = usage.parse_usage("openai", {
            "rate_limit": {},
            "rate_limit_reset_credits": {"available_count": 1, "credits": [
                {"id": "SECRET-REDEMPTION-ID", "status": "available", "title": "Full reset", "expires_at": "2026-11-06T23:21:40Z"},
                {"id": "SECRET-USED-ID", "status": "redeemed", "title": "Already used"},
            ]},
        }, "", 1000)
        self.assertEqual(result["reset_credits"]["available"], 1)
        self.assertEqual(len(result["reset_credits"]["credits"]), 1)
        self.assertNotIn("SECRET", json.dumps(result))
        self.assertNotIn("Already used", json.dumps(result))

    def test_exact_reset_dates_follow_local_daylight_saving_time(self):
        try:
            with patch.dict(usage.os.environ, {"TZ": "Europe/Athens"}):
                usage.time.tzset()
                self.assertEqual(usage.local_datetime(usage.timestamp("2026-10-19T13:00:00Z")), "Mon 19 Oct, 16:00 EEST")
                self.assertEqual(usage.local_datetime(usage.timestamp("2026-11-06T23:21:40Z")), "Sat 07 Nov, 01:21 EET")
                self.assertEqual(usage.local_datetime(None), "—")
        finally:
            usage.time.tzset()

    def test_tooltip_shows_remaining_allowance_and_only_weekly_rows(self):
        text = usage.provider_tooltip({"metrics": [
            {"label": "Weekly", "percent": 37, "reset_at": 1792414800},
            {"label": "Sonnet", "percent": 110, "reset_at": None},
            {"label": "5-hour", "percent": 20, "reset_at": None},
            {"label": "Fable", "percent": 90, "reset_at": None},
        ]}, "Claude")
        plain = "".join(ElementTree.fromstring("<tooltip>" + text + "</tooltip>").itertext())
        self.assertIn("Left", plain)
        self.assertIn("63%", plain)
        self.assertIn("  0%", plain)
        self.assertIn("Reset", plain)
        self.assertNotIn("5-hour", plain)
        self.assertNotIn("Fable", plain)

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

    def test_reset_expiry_failure_keeps_live_usage_and_available_count(self):
        payload = {"rate_limit": {"primary_window": {"used_percent": 11, "limit_window_seconds": 604800}},
                   "rate_limit_reset_credits": {"available_count": 1}}
        with patch.object(usage, "fetch_json", side_effect=[payload, usage.UsageError("Offline")]) as fetch, patch.object(usage.time, "time", return_value=1000):
            result = usage.load_provider("openai")
            self.assertEqual(usage.load_provider("openai"), result)
        self.assertEqual(fetch.call_count, 2)
        self.assertEqual(result["metrics"][0]["percent"], 11)
        self.assertEqual(result["reset_credits"], {"available": 1, "credits": []})
        self.assertNotIn("error", result)

    def test_legacy_openai_cache_refreshes_reset_data_once(self):
        filename = self.directory / "waybar-ai-usage/openai.json"
        filename.parent.mkdir()
        old = {"id": "openai", "metrics": [], "plan": "Prolite"}
        usage.write_cache(filename, {"identity": "identity-one", "checked_at": 1000, "entry": old})
        payload = {"rate_limit": {}, "rate_limit_reset_credits": {"available_count": 0}}
        with patch.object(usage, "fetch_json", return_value=payload) as fetch, patch.object(usage.time, "time", return_value=1001):
            result = usage.load_provider("openai")
            self.assertEqual(usage.load_provider("openai"), result)
        fetch.assert_called_once()
        self.assertEqual(result["reset_credits"]["available"], 0)

if __name__ == "__main__":
    unittest.main()
