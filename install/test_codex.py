import contextlib
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import tomllib
import unittest

from link_tree import deploy


class CodexTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.base = Path(temporary.name)
        self.repo = self.base / 'dotfiles'
        self.home = self.base / 'home'
        self.home.mkdir()
        (self.repo / 'install').mkdir(parents=True)
        shutil.copy2(Path(__file__).with_name('link_tree.py'), self.repo / 'install/link_tree.py')
        self.wrapper = self.repo / 'home/.local/bin/codex'
        self.wrapper.parent.mkdir(parents=True)
        shutil.copy2(Path(__file__).parent.parent / 'home/.local/bin/codex', self.wrapper)
        self.target = self.home / '.local/bin/codex'
        self.target.parent.mkdir(parents=True)
        self.original = self.home / '.local/libexec/codex'
        self.environment = {**os.environ, 'HOME': str(self.home),
                            'PATH': str(self.target.parent) + os.pathsep + os.defpath}
        output = contextlib.redirect_stdout(io.StringIO())
        output.__enter__()
        self.addCleanup(output.__exit__, None, None, None)

    def real_cli(self, path):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(f'#!{sys.executable}\n'
                        'import json, os, sys\n'
                        'print(json.dumps({"argv":sys.argv[1:], "cwd":os.getcwd(),\n'
                        '  "install":os.environ["CODEX_INSTALL_DIR"]}))\n'
                        'sys.exit(23)\n')
        path.chmod(0o755)
        return path

    def run_wrapper(self, arguments):
        project = self.base / 'project with "quotes" and λ'
        project.mkdir(exist_ok=True)
        result = subprocess.run([str(self.target), *arguments], cwd=project,
                                env=self.environment, capture_output=True, text=True, timeout=5)
        self.assertEqual(result.returncode, 23, result.stderr)
        captured = json.loads(result.stdout)
        self.assertEqual(captured['argv'][0], '-c')
        self.assertEqual(tomllib.loads(captured['argv'][1]), {
            'projects': {str(project): {'trust_level': 'trusted'}},
        })
        self.assertEqual(captured['argv'][2:], arguments)
        self.assertEqual(captured['cwd'], str(project))
        self.assertEqual(captured['install'], str(self.original.parent))

    def test_existing_launcher_is_preserved_and_wrapper_forwards_arguments_and_exit_code(self):
        real = self.real_cli(self.home / '.codex/packages/release/bin/codex')
        current = real.parents[1].with_name('current')
        current.symlink_to(real.parents[1])
        self.target.symlink_to(current / 'bin/codex')
        deploy(self.repo / 'home', self.home, dry_run=True)
        self.assertEqual(self.target.readlink(), current / 'bin/codex')
        self.assertFalse(self.original.exists())
        deploy(self.repo / 'home', self.home)
        deploy(self.repo / 'home', self.home)
        self.assertEqual(self.target.resolve(), self.wrapper)
        self.assertEqual(self.original.readlink(), current / 'bin/codex')
        self.assertFalse(self.target.with_name('codex.before-dotfiles.1').exists())
        self.run_wrapper(['resume', '--last', 'prompt with spaces'])

        updated = self.real_cli(self.home / '.codex/packages/new-release/bin/codex')
        current.unlink()
        current.symlink_to(updated.parents[1])
        self.assertEqual(self.original.resolve(), updated)
        self.run_wrapper(['--version'])

    def test_regular_executable_is_moved_without_losing_its_contents_or_mode(self):
        self.real_cli(self.target)
        contents = self.target.read_bytes()
        deploy(self.repo / 'home', self.home, dry_run=True)
        self.assertEqual(self.target.read_bytes(), contents)
        self.assertFalse(self.original.exists())
        deploy(self.repo / 'home', self.home)
        self.assertFalse(self.original.is_symlink())
        self.assertEqual(self.original.read_bytes(), contents)
        self.assertEqual(self.original.stat().st_mode & 0o777, 0o755)
        self.run_wrapper(['--help'])

    def test_fallback_skips_all_links_to_the_wrapper_on_path(self):
        deploy(self.repo / 'home', self.home)
        duplicate = self.base / 'duplicate'
        duplicate.mkdir()
        (duplicate / 'codex').symlink_to(self.wrapper)
        fallback = self.base / 'fallback'
        self.real_cli(fallback / 'codex')
        self.environment['PATH'] = os.pathsep.join((str(self.target.parent), str(duplicate),
                                                  str(fallback), os.defpath))
        self.run_wrapper(['exec', 'hello'])

    def test_missing_cli_exits_instead_of_recursing(self):
        deploy(self.repo / 'home', self.home)
        result = subprocess.run([str(self.target), '--version'], env=self.environment,
                                capture_output=True, text=True, timeout=5)
        self.assertEqual(result.returncode, 127)
        self.assertIn('underlying CLI not found', result.stderr)


if __name__ == '__main__':
    unittest.main()
