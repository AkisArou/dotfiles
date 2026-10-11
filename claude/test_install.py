import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import call, patch

import install


class SettingsMergeTests(unittest.TestCase):
    def test_updates_defaults_and_preserves_local_choices(self):
        previous = {'model': 'opus', 'theme': 'dark', 'permissions': {'defaultMode': 'auto'}}
        defaults = {'model': 'new-opus', 'theme': 'light', 'permissions': {'defaultMode': 'plan'}}
        local = {'model': 'sonnet', 'theme': 'dark', 'permissions': {'defaultMode': 'auto'}, 'autoMode': {'environment': ['project context']}}
        expected = {'model': 'sonnet', 'theme': 'light', 'permissions': {'defaultMode': 'plan'}, 'autoMode': {'environment': ['project context']}}
        self.assertEqual(install.merge(previous, defaults, local), expected)

    def test_removes_only_unchanged_retired_defaults(self):
        previous = {'enabledPlugins': {'inherited': True, 'overridden': True}}
        local = {'enabledPlugins': {'inherited': True, 'overridden': False, 'personal': True}}
        self.assertEqual(install.merge(previous, {}, local), {'enabledPlugins': {'overridden': False, 'personal': True}})

    def test_preserves_local_deletions_and_edited_lists(self):
        previous = {'theme': 'dark', 'permissions': {'allow': ['Read', 'Bash(git *)']}}
        defaults = {'theme': 'light', 'permissions': {'allow': ['Read', 'Bash(*)']}}
        local = {'permissions': {'allow': ['Read']}}
        self.assertEqual(install.merge(previous, defaults, local), local)

    def test_updates_an_unchanged_list_as_a_whole(self):
        previous = {'permissions': {'allow': ['Read', 'Bash(git *)']}}
        defaults = {'permissions': {'allow': ['Read']}}
        self.assertEqual(install.merge(previous, defaults, previous), defaults)

    def test_first_install_preserves_existing_preferences(self):
        defaults = {'model': 'opus', 'theme': 'dark', 'permissions': {'defaultMode': 'auto'}}
        local = {'model': 'sonnet', 'permissions': {'deny': ['Read(.env)']}}
        self.assertEqual(install.merge({}, defaults, local), {'model': 'sonnet', 'theme': 'dark', 'permissions': {'defaultMode': 'auto', 'deny': ['Read(.env)']}})

    def test_boolean_and_number_are_different_values(self):
        self.assertEqual(install.merge({'custom': True}, {'custom': False}, {'custom': 1}), {'custom': 1})


class InstallerTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        root = Path(self.temporary.name)
        self.base = root / 'base'
        self.config = root / 'config'
        self.state = root / 'state'
        self.claude_state = root / '.claude.json'
        self.settings = self.config / 'settings.json'
        self.write(self.base / 'settings.json', {'model': 'opus', 'theme': 'dark'})
        self.write(self.base / 'mcp.json', {'mcpServers': {}})

    def write(self, path, value):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(value))

    def run_install(self):
        install.install(self.base, self.config, self.state, self.claude_state)

    def test_fresh_install_and_unchanged_rerun(self):
        self.run_install()
        self.assertFalse(self.settings.is_symlink())
        self.assertEqual(json.loads(self.settings.read_text()), {'model': 'opus', 'theme': 'dark'})
        self.assertEqual(self.settings.stat().st_mode & 0o777, 0o600)
        before = (self.settings.read_bytes(), self.settings.stat().st_mtime_ns)
        self.run_install()
        self.assertEqual((self.settings.read_bytes(), self.settings.stat().st_mtime_ns), before)
        self.assertEqual(list(self.config.glob('*.bak')), [])

    def test_base_update_preserves_override_and_backs_up_settings(self):
        self.run_install()
        self.write(self.settings, {'model': 'sonnet', 'theme': 'dark', 'autoMode': {'environment': ['local context']}})
        original = self.settings.read_bytes()
        self.write(self.base / 'settings.json', {'model': 'new-opus', 'theme': 'light'})
        self.run_install()
        self.assertEqual(json.loads(self.settings.read_text()), {'model': 'sonnet', 'theme': 'light', 'autoMode': {'environment': ['local context']}})
        backups = list(self.config.glob('*.bak'))
        self.assertEqual(len(backups), 1)
        self.assertEqual(backups[0].read_bytes(), original)
        self.assertEqual(json.loads((self.state / 'settings.json').read_text()), {'model': 'new-opus', 'theme': 'light'})
        self.run_install()
        self.assertEqual(list(self.config.glob('*.bak')), backups)

    def test_detaches_symlink_without_changing_its_target(self):
        self.config.mkdir()
        target = self.base / 'settings.json'
        original = target.read_bytes()
        self.settings.symlink_to(target)
        self.run_install()
        self.assertFalse(self.settings.is_symlink())
        self.assertEqual(target.read_bytes(), original)

    def test_invalid_settings_do_not_change_files_or_register_mcp(self):
        self.config.mkdir()
        self.settings.write_text('{broken JSON')
        with patch('install.mcp_command') as command:
            with self.assertRaises(ValueError):
                self.run_install()
        command.assert_not_called()
        self.assertEqual(self.settings.read_text(), '{broken JSON')
        self.assertFalse(self.state.exists())

    def test_missing_defaults_are_an_error(self):
        self.write(self.settings, {'model': 'sonnet'})
        original = self.settings.read_bytes()
        (self.base / 'settings.json').unlink()
        with self.assertRaises(FileNotFoundError):
            self.run_install()
        self.assertEqual(self.settings.read_bytes(), original)
        self.assertFalse(self.state.exists())

    def test_unchanged_mcp_keeps_claude_state_byte_for_byte(self):
        server = {'type': 'stdio', 'command': 'orcode', 'args': ['mcp']}
        self.write(self.base / 'mcp.json', {'mcpServers': {'orcode': server}})
        self.write(self.claude_state, {'mcpServers': {'orcode': server}, 'projects': {'/local': {'trusted': True}}, 'oauthAccount': {'local': 'account data'}})
        original = self.claude_state.read_bytes()
        with patch('install.mcp_command') as command:
            self.run_install()
            self.run_install()
        command.assert_not_called()
        self.assertEqual(self.claude_state.read_bytes(), original)

    def test_mcp_additions_and_removals_use_user_cli(self):
        inherited = {'type': 'stdio', 'command': 'old-server'}
        addition = {'type': 'stdio', 'command': 'new-server'}
        self.write(self.state / 'mcp.json', {'mcpServers': {'retired': inherited}})
        self.write(self.claude_state, {'mcpServers': {'retired': inherited, 'personal': inherited}})
        self.write(self.base / 'mcp.json', {'mcpServers': {'added': addition}})
        with patch('install.mcp_command') as command:
            self.run_install()
        self.assertEqual(command.call_args_list, [call('add-json', 'added', addition), call('remove', 'retired')])

    def test_locally_modified_and_deleted_mcp_servers_stay_local(self):
        original = {'type': 'stdio', 'command': 'original'}
        modified = {'type': 'stdio', 'command': 'personal'}
        self.write(self.state / 'mcp.json', {'mcpServers': {'modified': original, 'deleted': original}})
        self.write(self.claude_state, {'mcpServers': {'modified': modified}})
        self.write(self.base / 'mcp.json', {'mcpServers': {'modified': original, 'deleted': original}})
        with patch('install.mcp_command') as command:
            self.run_install()
        command.assert_not_called()

    def test_failed_mcp_update_restores_server_and_keeps_baseline(self):
        original = {'type': 'stdio', 'command': 'original'}
        updated = {'type': 'stdio', 'command': 'updated'}
        self.write(self.state / 'mcp.json', {'mcpServers': {'server': original}})
        self.write(self.claude_state, {'mcpServers': {'server': original}})
        self.write(self.base / 'mcp.json', {'mcpServers': {'server': updated}})
        self.write(self.settings, {'model': 'sonnet'})
        with patch('install.mcp_command', side_effect=[None, RuntimeError('registration failed'), None]) as command:
            with self.assertRaisesRegex(RuntimeError, 'registration failed'):
                self.run_install()
        self.assertEqual(command.call_args_list, [call('remove', 'server'), call('add-json', 'server', updated), call('add-json', 'server', original)])
        self.assertEqual(json.loads((self.state / 'mcp.json').read_text()), {'mcpServers': {'server': original}})
        self.assertEqual(json.loads(self.settings.read_text()), {'model': 'sonnet'})
        self.assertFalse((self.state / 'settings.json').exists())


if __name__ == '__main__':
    unittest.main()
