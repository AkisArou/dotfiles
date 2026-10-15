import contextlib
import importlib.machinery
import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

from link_tree import deploy


class DeploymentTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.base = Path(temporary.name)
        self.repo = self.base / 'dotfiles'
        self.home = self.base / 'home'
        self.home.mkdir()
        installer = self.repo / 'install'
        installer.mkdir(parents=True)
        shutil.copy2(Path(__file__).with_name('link_tree.py'), installer / 'link_tree.py')
        output = contextlib.redirect_stdout(io.StringIO())
        output.__enter__()
        self.addCleanup(output.__exit__, None, None, None)

    def write(self, path, value):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(value) if not isinstance(value, str) else value)
        return path

    def test_linking_preserves_all_managed_settings_and_their_source_defaults(self):
        sources = {
            '.claude/settings.json': {'model': 'default'},
            '.claude.json': {'mcpServers': {}},
            '.config/muse/settings.json': {'schema_version': 1},
            '.config/BraveSoftware/Brave-Browser/Default/Preferences': {'browser': {}},
            '.config/BraveSoftware/Brave-Browser/Local State': {'brave': {}},
            '.config/dconf/user': '[org/gnome/desktop/interface]\ncolor-scheme=\'prefer-dark\'\n',
        }
        originals = {}
        for relative, value in sources.items():
            self.write(self.repo / 'home' / relative, value)
            target = self.write(self.home / relative, f'local data for {relative}')
            originals[relative] = target.read_bytes()
        ordinary = self.write(self.repo / 'home/.config/muse/dotfiles-hooks.json', {'hooks': {}})
        deploy(self.repo / 'home', self.home)
        for relative, contents in originals.items():
            target = self.home / relative
            self.assertFalse(target.is_symlink(), relative)
            self.assertEqual(target.read_bytes(), contents)
        self.assertEqual((self.home / '.config/muse/dotfiles-hooks.json').resolve(), ordinary)
        self.write(self.home / '.claude/settings.json', {'model': 'personal'})
        self.assertEqual(json.loads((self.repo / 'home/.claude/settings.json').read_text()), {'model': 'default'})

    def test_individual_managed_file_is_also_protected_from_linking(self):
        source = self.write(self.repo / 'home/.claude.json', {'mcpServers': {}})
        target = self.write(self.home / '.claude.json', {'accountState': 'personal'})
        deploy(source, target)
        self.assertFalse(target.is_symlink())
        self.assertEqual(json.loads(target.read_text()), {'accountState': 'personal'})

    def test_root_copy_rules_are_applied_from_the_table(self):
        network = self.write(self.repo / 'root/etc/systemd/network/20-test.network', '[Match]\nName=en*\n')
        environment = self.write(self.repo / 'root/etc/environment', 'EDITOR=vim\n')
        destination = self.base / 'filesystem'
        deploy(self.repo / 'root', destination)
        copied = destination / 'etc/systemd/network/20-test.network'
        self.assertFalse(copied.is_symlink())
        self.assertEqual(copied.read_bytes(), network.read_bytes())
        self.assertEqual(copied.stat().st_mode & 0o777, 0o644)
        self.assertEqual((destination / 'etc/environment').resolve(), environment)

    def test_full_deployment_merges_defaults_without_linking_live_agent_state(self):
        for name in ('deploy-home', 'link_tree.py', 'configure_claude.py', 'configure_muse.py'):
            shutil.copy2(Path(__file__).with_name(name), self.repo / 'install' / name)
        self.write(self.repo / 'home/.claude/settings.json', {'theme': 'dark', 'model': 'default'})
        self.write(self.repo / 'home/.claude.json', {'mcpServers': {}})
        self.write(self.repo / 'home/.config/muse/settings.json', {
            'schema_version': 1, 'managed_hooks_path': 'dotfiles-hooks.json',
            'managed_hooks_env_vars': ['TMUX'],
        })
        self.write(self.repo / 'home/.config/muse/dotfiles-hooks.json', {'hooks': {}})
        claude = self.write(self.home / '.claude/settings.json', {'model': 'personal'})
        account = self.write(self.home / '.claude.json', {'accountState': 'keep'})
        muse = self.write(self.home / '.config/muse/settings.json', {
            'personal': True, 'managed_hooks_env_vars': ['EXTRA'],
        })
        defaults = self.write(self.repo / 'root/etc/codex/config.toml', 'model = "default"\n')
        codex = self.home / '.codex/config.toml'
        codex.parent.mkdir()
        codex.symlink_to(defaults)
        environment = {**os.environ, 'HOME': str(self.home),
                       'CODEX_HOME': str(self.home / '.codex'),
                       'XDG_CONFIG_HOME': str(self.home / '.config'),
                       'XDG_STATE_HOME': str(self.home / '.local/state')}
        environment.pop('CLAUDE_CONFIG_DIR', None)
        command = [sys.executable, str(self.repo / 'install/deploy-home')]
        before = {path: path.read_bytes() for path in (claude, account, muse, codex)}
        preview = subprocess.run([*command, '--dry-run'], env=environment, check=True,
                                 capture_output=True, text=True)
        self.assertIn('Merge defaults: claude', preview.stdout)
        self.assertFalse((self.home / '.local/state/dotfiles/claude').exists())
        for path, contents in before.items():
            self.assertEqual(path.read_bytes(), contents)
        self.assertTrue(codex.is_symlink())
        subprocess.run(command, env=environment, check=True, capture_output=True)
        subprocess.run(command, env=environment, check=True, capture_output=True)
        self.assertEqual(json.loads(claude.read_text()), {'model': 'personal', 'theme': 'dark'})
        self.assertEqual(json.loads(account.read_text()), {'accountState': 'keep'})
        self.assertEqual(json.loads(muse.read_text())['managed_hooks_env_vars'], ['EXTRA', 'TMUX'])
        self.assertTrue(json.loads(muse.read_text())['personal'])
        for path in (claude, account, muse, codex):
            self.assertFalse(path.is_symlink())
        codex.write_text('model = "personal"\n')
        self.assertEqual(defaults.read_text(), 'model = "default"\n')
        self.assertTrue((self.home / '.config/muse/dotfiles-hooks.json').is_symlink())
        self.assertEqual(json.loads((self.repo / 'home/.claude/settings.json').read_text())['model'], 'default')

    def test_brave_merges_both_mirrored_files_and_preserves_profile_data(self):
        path = Path(__file__).parent.parent / 'home/.local/libexec/configure-brave'
        loader = importlib.machinery.SourceFileLoader('configure_brave', str(path))
        spec = importlib.util.spec_from_loader(loader.name, loader)
        module = importlib.util.module_from_spec(spec)
        loader.exec_module(module)
        defaults = self.repo / 'home/.config/BraveSoftware/Brave-Browser'
        preferences = self.write(defaults / 'Default/Preferences', {'browser': {'show_forward_button': True}})
        self.write(defaults / 'Local State', {'brave': {'compact': True}})
        profile = self.home / '.config/BraveSoftware/Brave-Browser'
        target = self.write(profile / 'Default/Preferences', {'personal': 'keep', 'browser': {'custom': True}})
        state = self.write(profile / 'Local State', {'local': 'keep'})
        module.configure_brave(profile, defaults)
        module.configure_brave(profile, defaults)
        self.assertEqual(json.loads(target.read_text()), {
            'personal': 'keep', 'browser': {'custom': True, 'show_forward_button': True},
        })
        self.assertEqual(json.loads(state.read_text()), {'local': 'keep', 'brave': {'compact': True}})
        self.assertFalse(target.is_symlink())
        self.assertFalse(state.is_symlink())
        self.assertEqual(json.loads(preferences.read_text()), {'browser': {'show_forward_button': True}})

    def test_installed_brave_helper_runs_without_importing_setup_code(self):
        source = self.repo / 'home/.local/libexec'
        source.mkdir(parents=True)
        for name in ('configure-brave', 'browser_paths.py'):
            shutil.copy2(Path(__file__).parent.parent / 'home/.local/libexec' / name, source / name)
        defaults = self.repo / 'home/.config/BraveSoftware/Brave-Browser'
        self.write(defaults / 'Default/Preferences', {'browser': {'show_forward_button': True}})
        self.write(defaults / 'Local State', {'brave': {'compact': True}})
        self.write(self.repo / 'home/.local/share/brave/extensions/video-repeater/manifest.json',
                   {'name': 'Video Repeater'})
        preferences = self.write(self.home / '.config/BraveSoftware/Brave-Browser/Default/Preferences', {
            'personal': 'keep', 'extensions': {'settings': {'id': {
                'path': str(self.repo / 'brave/extensions/video-repeater'),
            }}},
        })
        original = preferences.read_bytes()
        pgrep = self.write(self.base / 'bin/pgrep', '#!/bin/sh\nexit 1\n')
        pgrep.chmod(0o755)
        deploy(self.repo / 'home', self.home)
        environment = {**os.environ, 'HOME': str(self.home),
                       'PATH': str(pgrep.parent) + os.pathsep + os.defpath}
        environment.pop('PYTHONPATH', None)
        command = [sys.executable, str(self.home / '.local/libexec/configure-brave')]
        subprocess.run(command, cwd=self.home, env=environment, check=True, capture_output=True)
        subprocess.run(command, cwd=self.home, env=environment, check=True, capture_output=True)
        updated = json.loads(preferences.read_text())
        self.assertEqual(updated['personal'], 'keep')
        self.assertTrue(updated['browser']['show_forward_button'])
        self.assertEqual(updated['extensions']['settings']['id']['path'],
                         str(self.home / '.local/share/brave/extensions/video-repeater'))
        self.assertEqual(preferences.with_name('Preferences.before-dotfiles').read_bytes(), original)
        self.assertFalse(preferences.with_name('Preferences.before-dotfiles.1').exists())


if __name__ == '__main__':
    unittest.main()
