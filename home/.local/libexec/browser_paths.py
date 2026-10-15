"""Move the unpacked Brave extension without editing a running browser's settings."""

import json
import os
from pathlib import Path
import shutil
import tempfile


def migrate_extension(profile_root, target, *, running=False, dry_run=False):
    target = Path(target)
    manifest = target / 'manifest.json'
    if not manifest.is_file():
        return
    name = json.loads(manifest.read_text())['name']
    for preferences in sorted(profile_root.glob('*/Preferences')):
        original = json.loads(preferences.read_text())
        aliases = []
        for extension in original.get('extensions', {}).get('settings', {}).values():
            old = Path(extension.get('path', ''))
            if (old == target or not old.is_absolute()
                    or old.parts[-3:] != ('brave', 'extensions', 'video-repeater')
                    or extension.get('manifest', {}).get('name') not in (None, name)):
                continue
            aliases.append(old)
            if not running:
                extension['path'] = str(target)
        if not aliases:
            continue
        if running:
            print('Brave extension path will update on the next fresh launch through brave-launch.')
            continue
        print(f'Update unpacked extension path: {preferences}')
        saved = preferences.with_name(preferences.name + '.before-dotfiles')
        suffix = 1
        while os.path.lexists(saved):
            saved = preferences.with_name(f'{preferences.name}.before-dotfiles.{suffix}')
            suffix += 1
        print(f'Backup: {preferences} -> {saved}')
        if not dry_run:
            shutil.copy2(preferences, saved)
            descriptor, temporary = tempfile.mkstemp(prefix='.dotfiles-', dir=preferences.parent)
            try:
                with os.fdopen(descriptor, 'w') as output:
                    json.dump(original, output, separators=(',', ':'))
                os.chmod(temporary, saved.stat().st_mode & 0o777)
                os.replace(temporary, preferences)
            finally:
                Path(temporary).unlink(missing_ok=True)
        for alias in aliases:
            if alias.is_symlink() and alias.resolve() == target.resolve():
                print(f'Remove temporary extension link: {alias}')
                if not dry_run:
                    alias.unlink()
                    for directory in (alias.parent, alias.parent.parent):
                        try:
                            directory.rmdir()
                        except OSError:
                            break
