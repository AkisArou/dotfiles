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
from unittest.mock import patch

from legacy_paths import relocate
from link_tree import deploy


loader = importlib.machinery.SourceFileLoader('migrate', str(Path(__file__).with_name('migrate')))
spec = importlib.util.spec_from_loader(loader.name, loader)
migrate = importlib.util.module_from_spec(spec)
loader.exec_module(migrate)
migrate_extension = migrate.migrate_extension


class LayoutTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.base = Path(temporary.name)
        self.source = self.base / 'source'
        self.target = self.base / 'target'
        self.source.mkdir()
        self.target.mkdir()
        output = contextlib.redirect_stdout(io.StringIO())
        output.__enter__()
        self.addCleanup(output.__exit__, None, None, None)

    def write(self, path, text='managed'):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
        return path

    def test_merge_preserves_local_files_and_backs_up_conflicts_once(self):
        source = self.write(self.source / '.config/app/settings')
        target = self.write(self.target / '.config/app/settings', 'local settings')
        other = self.write(target.with_name('local-only'), 'keep me')
        deploy(self.source, self.target)
        deploy(self.source, self.target)
        self.assertEqual(target.resolve(), source)
        self.assertEqual(other.read_text(), 'keep me')
        self.assertEqual(target.with_name('settings.before-dotfiles').read_text(), 'local settings')
        self.assertFalse(target.with_name('settings.before-dotfiles.1').exists())

    def test_dry_run_changes_nothing(self):
        self.write(self.source / '.config/app/settings')
        original = self.write(self.target / '.config/app/settings', 'local')
        deploy(self.source, self.target, dry_run=True)
        self.assertFalse(original.is_symlink())
        self.assertEqual(original.read_text(), 'local')
        self.assertFalse(original.with_name('settings.before-dotfiles').exists())

    def test_existing_managed_directory_link_is_preserved(self):
        source = self.write(self.source / 'app/settings')
        (self.target / 'app').symlink_to(source.parent)
        deploy(self.source, self.target)
        self.assertTrue((self.target / 'app').is_symlink())
        self.assertFalse(source.is_symlink())

    def test_foreign_directory_link_is_not_followed(self):
        self.write(self.source / 'app/settings')
        foreign = self.base / 'foreign'
        foreign.mkdir()
        (self.target / 'app').symlink_to(foreign)
        with self.assertRaises(ValueError):
            deploy(self.source, self.target)
        self.assertEqual(list(foreign.iterdir()), [])

    def test_copy_policy_produces_regular_files_with_required_permissions(self):
        source = self.write(self.source / 'etc/sudoers.d/local')
        deploy(self.source, self.target, copies={'etc/sudoers.d': 0o440})
        target = self.target / 'etc/sudoers.d/local'
        self.assertFalse(target.is_symlink())
        self.assertEqual(target.read_bytes(), source.read_bytes())
        self.assertEqual(target.stat().st_mode & 0o777, 0o440)
        deploy(self.source, self.target, copies={'etc/sudoers.d': 0o440})
        self.assertFalse(target.with_name('local.before-dotfiles').exists())

    def test_git_ignored_state_is_not_deployed(self):
        subprocess.run(['git', 'init', '--quiet', str(self.base)], check=True)
        self.write(self.base / '.gitignore', '*.lock\n')
        self.write(self.source / '.config/app/settings')
        self.write(self.source / '.config/app/state.lock')
        self.write(self.source / '__pycache__/cached.pyc')
        deploy(self.source, self.target)
        self.assertTrue((self.target / '.config/app/settings').is_symlink())
        self.assertFalse((self.target / '.config/app/state.lock').exists())
        self.assertFalse((self.target / '__pycache__').exists())

    def test_migration_handles_old_directory_links_and_renamed_files(self):
        repo = self.base / 'checkout'
        old_repo = self.base / 'previous-checkout'
        source = self.write(repo / 'home/.config/nvim/init.lua')
        vim = self.write(repo / 'home/.ideavimrc')
        app_link = self.target / '.config/nvim'
        app_link.parent.mkdir()
        app_link.symlink_to(old_repo / 'nvim')
        file_link = self.target / '.ideavimrc'
        file_link.symlink_to(old_repo / 'vim/.ideavimrc')
        planned = migrate.migrate_links([app_link, file_link], repo, old_repo, dry_run=True)
        deploy(repo / 'home', self.target, dry_run=True, planned_links=planned)
        self.assertEqual(app_link.readlink(), old_repo / 'nvim')
        migrate.migrate_links([app_link, file_link], repo, old_repo)
        deploy(repo / 'home', self.target)
        self.assertEqual(app_link.resolve(), source.parent)
        self.assertEqual(file_link.resolve(), vim)

    def test_migration_leaves_unrelated_and_preexisting_broken_links_alone(self):
        repo = self.base / 'checkout'
        unrelated = self.target / 'unrelated'
        unrelated.symlink_to(self.base / 'other/settings')
        broken = self.target / 'old-missing'
        broken.symlink_to(repo / 'systemd/user/removed.service')
        migrate.migrate_links([unrelated, broken], repo, repo)
        self.assertEqual(unrelated.readlink(), self.base / 'other/settings')
        self.assertEqual(broken.readlink(), repo / 'systemd/user/removed.service')

    def test_hook_migration_preserves_local_settings_and_modes(self):
        home = self.target
        settings = self.write(home / '.claude/settings.json', json.dumps({
            'model': 'personal', 'hooks': [{'command': 'python3 "$HOME/dotfiles/tmux/claude-status.py"'}],
        }))
        settings.chmod(0o640)
        with patch.dict(os.environ, {'CLAUDE_CONFIG_DIR': str(home / '.claude'),
                                     'XDG_STATE_HOME': str(home / '.local/state')}):
            migrate.migrate_claude_hooks(home, self.source, dry_run=True)
            self.assertIn('dotfiles/tmux', settings.read_text())
            migrate.migrate_claude_hooks(home, self.source)
            migrate.migrate_claude_hooks(home, self.source)
        result = json.loads(settings.read_text())
        self.assertEqual(result['model'], 'personal')
        self.assertIn('$HOME/.config/tmux/', result['hooks'][0]['command'])
        self.assertEqual(settings.stat().st_mode & 0o777, 0o640)
        self.assertFalse(settings.with_name('settings.json.before-dotfiles.1').exists())

    def test_full_personal_migration_can_be_repeated_on_another_computer(self):
        repo = self.base / 'computer/dotfiles'
        installer = repo / 'install'
        installer.mkdir(parents=True)
        for name in ('migrate', 'link_tree.py', 'legacy_paths.py'):
            shutil.copy2(Path(__file__).with_name(name), installer / name)
        browser_paths = repo / 'home/.local/libexec/browser_paths.py'
        browser_paths.parent.mkdir(parents=True)
        shutil.copy2(Path(__file__).parent.parent / 'home/.local/libexec/browser_paths.py', browser_paths)
        self.write(repo / 'home/.config/nvim/init.lua')
        self.write(repo / 'home/.local/bin/nable-private-access-connect')
        self.write(repo / 'home/.local/share/nable-private-access/resolved.conf')
        work = self.write(repo / 'home/.local/bin/work')
        (self.target / '.local/bin').mkdir(parents=True)
        old_work = self.target / '.local/bin/work'
        old_work.symlink_to(repo / 'scripts/work')
        (self.target / '.codex').mkdir()
        old_wrapper = self.target / '.codex/codex.py'
        old_wrapper.symlink_to(repo / 'codex/codex.py')
        (self.target / '.config').mkdir()
        (self.target / '.config/nvim').symlink_to(repo / 'nvim')
        environment = {**os.environ, 'HOME': str(self.target),
                       'CLAUDE_CONFIG_DIR': str(self.target / '.claude'),
                       'CODEX_HOME': str(self.target / '.codex'),
                       'XDG_STATE_HOME': str(self.target / '.local/state')}
        command = [sys.executable, str(installer / 'migrate'), '--skip-root', '--no-reload']
        subprocess.run([*command, '--dry-run'], env=environment, check=True, capture_output=True)
        self.assertTrue(old_wrapper.is_symlink())
        self.assertEqual(old_work.readlink(), repo / 'scripts/work')
        self.assertEqual((self.target / '.config/nvim').readlink(), repo / 'nvim')
        subprocess.run(command, env=environment, check=True, capture_output=True)
        result = subprocess.run(command, env=environment, check=True, capture_output=True, text=True)
        self.assertEqual(result.stdout.strip(), 'Migration complete.')
        self.assertEqual((self.target / '.config/nvim').resolve(), repo / 'home/.config/nvim')
        self.assertTrue((self.target / '.local/share/nable-private-access/resolved.conf').is_file())
        self.assertFalse(os.path.lexists(old_wrapper))
        self.assertEqual(old_work.resolve(), work)

    def test_specific_legacy_paths_take_precedence(self):
        self.assertEqual(relocate('systemd/user/example.service'), 'home/.config/systemd/user/example.service')
        self.assertEqual(relocate('systemd/system/example.service'), 'root/etc/systemd/system/example.service')
        self.assertEqual(relocate('shell/zsh/.zshenv'), 'home/.zshenv')
        self.assertEqual(relocate('install/data/vconsole/desktop.conf'), 'root/etc/vconsole/desktop.conf')
        self.assertEqual(relocate('claude/mcp.json'), 'home/.claude.json')
        self.assertEqual(relocate('codex/codex.py'), 'home/.local/bin/codex')
        self.assertEqual(relocate('home/.codex/codex.py'), 'home/.local/bin/codex')

    def test_managed_directory_links_are_detached_before_deployment(self):
        repo = self.source
        installer = repo / 'install'
        installer.mkdir()
        shutil.copy2(Path(__file__).with_name('link_tree.py'), installer / 'link_tree.py')
        defaults = self.write(repo / 'home/.config/muse/settings.json', '{"schema_version":1}')
        directory = self.target / '.config/muse'
        directory.parent.mkdir()
        directory.symlink_to(defaults.parent)
        with self.assertRaises(ValueError):
            deploy(repo / 'home', self.target)
        planned = {}
        migrate.detach_managed_directories(repo, self.target, dry_run=True, planned_links=planned)
        deploy(repo / 'home', self.target, dry_run=True, planned_links=planned)
        self.assertTrue(directory.is_symlink())
        migrate.detach_managed_directories(repo, self.target)
        deploy(repo / 'home', self.target)
        self.assertFalse(directory.is_symlink())
        (directory / 'settings.json').write_text('{"personal":true}')
        self.assertEqual(json.loads(defaults.read_text()), {'schema_version': 1})

    def test_legacy_agent_config_links_become_independent_local_files(self):
        repo = self.source
        defaults = self.write(repo / 'root/etc/codex/config.toml', 'model = "default"\n')
        local = self.target / 'config.toml'
        local.symlink_to(defaults)
        migrate.detach_managed_configs([local], repo, dry_run=True)
        self.assertTrue(local.is_symlink())
        migrate.detach_managed_configs([local], repo)
        self.assertFalse(local.is_symlink())
        self.assertEqual(local.read_text(), defaults.read_text())
        local.write_text('model = "personal"\n')
        self.assertEqual(defaults.read_text(), 'model = "default"\n')

    def test_running_browser_does_not_recreate_legacy_paths(self):
        target = self.target / '.local/share/brave/extensions/video-repeater'
        self.write(target / 'manifest.json', '{"name":"Video Repeater"}')
        old = self.source / 'brave/extensions/video-repeater'
        profile = self.target / '.config/BraveSoftware/Brave-Browser'
        preferences = self.write(profile / 'Default/Preferences', json.dumps({
            'extensions': {'settings': {'id': {'path': str(old)}}},
            'personal': 'preserve',
        }))
        before = preferences.read_bytes()
        migrate_extension(profile, target, running=True, dry_run=True)
        self.assertFalse(old.is_symlink())
        migrate_extension(profile, target, running=True)
        self.assertFalse(os.path.lexists(old))
        self.assertEqual(preferences.read_bytes(), before)
        migrate_extension(profile, target, running=False)
        updated = json.loads(preferences.read_text())
        self.assertEqual(updated['extensions']['settings']['id']['path'], str(target))
        self.assertEqual(updated['personal'], 'preserve')
        self.assertEqual(preferences.with_name('Preferences.before-dotfiles').read_bytes(), before)
        self.assertFalse(os.path.lexists(old))
        migrate_extension(profile, target, running=False)
        self.assertFalse(preferences.with_name('Preferences.before-dotfiles.1').exists())


if __name__ == '__main__':
    unittest.main()
