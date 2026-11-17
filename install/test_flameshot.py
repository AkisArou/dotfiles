import contextlib
import io
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

import configure_flameshot as install


class FlameshotTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.source = self.root / 'source/.config/flameshot/flameshot.ini'
        self.config = self.root / 'config/flameshot'
        self.settings = self.config / 'flameshot.ini'
        self.state = self.root / 'state'
        self.write(self.source, '[General]\nshowDesktopNotification=false\nshowStartupLaunchMessage=false\n')
        output = contextlib.redirect_stdout(io.StringIO())
        output.__enter__()
        self.addCleanup(output.__exit__, None, None, None)

    def write(self, path, contents):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(contents)

    def run_install(self):
        install.install(self.source, self.config, self.state)

    def test_fresh_install_and_rerun_leave_a_regular_file_unchanged(self):
        self.run_install()
        self.assertFalse(self.settings.is_symlink())
        self.assertEqual(install.read_ini(self.settings), {
            'General': {'showDesktopNotification': 'false', 'showStartupLaunchMessage': 'false'},
        })
        # Keep application formatting and comments when the values already match.
        self.write(self.settings, '# Local comment\n' + self.settings.read_text())
        before = (self.settings.read_bytes(), self.settings.stat().st_mtime_ns)
        self.run_install()
        self.assertEqual((self.settings.read_bytes(), self.settings.stat().st_mtime_ns), before)
        self.assertEqual(list(self.config.glob('*.bak')), [])

    def test_first_install_preserves_local_values_case_and_qt_syntax(self):
        self.write(self.settings, r'''[General]
showDesktopNotification=true
savePath=/tmp/100% screenshots
filenamePattern=%F_%H-%M
disabledTrayIcon=
[Shortcuts]
TYPE_COPY=Ctrl+C
TYPE_SAVE=Ctrl+S
custom\path=@ByteArray(a:b=c)
[DEFAULT]
personal=value
''')
        original = self.settings.read_bytes()
        source = self.source.read_bytes()
        self.run_install()
        self.assertEqual(install.read_ini(self.settings), {
            'General': {'showDesktopNotification': 'true', 'showStartupLaunchMessage': 'false',
                        'savePath': '/tmp/100% screenshots', 'filenamePattern': '%F_%H-%M',
                        'disabledTrayIcon': ''},
            'Shortcuts': {'TYPE_COPY': 'Ctrl+C', 'TYPE_SAVE': 'Ctrl+S',
                          'custom\\path': '@ByteArray(a:b=c)'},
            'DEFAULT': {'personal': 'value'},
        })
        self.assertEqual(self.source.read_bytes(), source)
        backups = list(self.config.glob('*.bak'))
        self.assertEqual(len(backups), 1)
        self.assertEqual(backups[0].read_bytes(), original)

    def test_updates_inherited_defaults_and_preserves_edits_and_deletions(self):
        self.write(self.source, '[General]\nshowDesktopNotification=false\nshowStartupLaunchMessage=false\n'
                   'deletedLocally=old\nretired=old\n[Retired]\nvalue=old\n')
        self.run_install()
        self.write(self.settings, '[General]\nshowDesktopNotification=true\nshowStartupLaunchMessage=false\n'
                   'retired=old\npersonal=keep\n[Retired]\nvalue=old\n')
        self.write(self.source, '[General]\nshowDesktopNotification=false\nshowStartupLaunchMessage=true\n'
                   'deletedLocally=new\nadded=new\n')
        self.run_install()
        self.assertEqual(install.read_ini(self.settings), {
            'General': {'showDesktopNotification': 'true', 'showStartupLaunchMessage': 'true',
                        'added': 'new', 'personal': 'keep'},
        })
        self.assertEqual(install.read_ini(self.state / 'flameshot.ini'), install.read_ini(self.source))
        backups = list(self.config.glob('*.bak'))
        self.run_install()
        self.assertEqual(list(self.config.glob('*.bak')), backups)

    def test_detaches_a_file_symlink_without_changing_its_target(self):
        self.config.mkdir(parents=True)
        self.settings.symlink_to(self.source)
        original = self.source.read_bytes()
        self.run_install()
        self.assertFalse(self.settings.is_symlink())
        self.assertEqual(self.source.read_bytes(), original)
        self.write(self.settings, '[General]\npersonal=true\n')
        self.assertEqual(self.source.read_bytes(), original)

    def test_directory_symlink_requires_manual_conversion(self):
        self.config.parent.mkdir()
        self.config.symlink_to(self.source.parent)
        original = self.source.read_bytes()
        with self.assertRaises(ValueError):
            self.run_install()
        self.assertTrue(self.config.is_symlink())
        self.assertEqual(self.source.read_bytes(), original)
        self.assertFalse(self.state.exists())

    def test_invalid_ini_leaves_local_settings_and_baseline_untouched(self):
        self.run_install()
        baseline = (self.state / 'flameshot.ini').read_bytes()
        self.write(self.settings, 'invalid settings without a section\n')
        with self.assertRaises(ValueError):
            self.run_install()
        self.assertEqual(self.settings.read_text(), 'invalid settings without a section\n')
        self.assertEqual((self.state / 'flameshot.ini').read_bytes(), baseline)
        self.assertEqual(list(self.config.glob('*.bak')), [])

    def test_missing_settings_are_restored_even_with_an_old_baseline(self):
        self.run_install()
        self.settings.unlink()
        self.write(self.source, '[General]\nshowDesktopNotification=true\n')
        self.run_install()
        self.assertEqual(install.read_ini(self.settings), install.read_ini(self.source))

    def test_main_honors_xdg_config_and_state_directories(self):
        with patch.dict(os.environ, {'XDG_CONFIG_HOME': str(self.config.parent),
                                     'XDG_STATE_HOME': str(self.state)}):
            install.main(self.root / 'source')
        self.assertFalse(self.settings.is_symlink())
        self.assertEqual(install.read_ini(self.settings), install.read_ini(self.source))
        self.assertEqual(install.read_ini(self.state / 'dotfiles/flameshot/flameshot.ini'),
                         install.read_ini(self.source))


if __name__ == '__main__':
    unittest.main()
