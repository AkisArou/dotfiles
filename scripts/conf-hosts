#!/usr/bin/python3

import os
from pathlib import Path
import shutil
import sys
import tempfile


def configure_hosts(blocklist, allowlist, hosts_file=Path("/etc/hosts")):
    block_lines = [
        line.strip() for line in blocklist.read_text().splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    ]
    blocked = {name for line in block_lines for name in line.split()[1:]}
    allowed = {
        line.split("#", 1)[0].strip() for line in allowlist.read_text().splitlines()
        if line.split("#", 1)[0].strip()
    }
    if blocked & allowed:
        raise ValueError("Hosts blocklist and allowlist overlap.")

    hosts_file = hosts_file.resolve()
    original = hosts_file.read_text() if hosts_file.exists() else ""
    begin = "# BEGIN dotfiles Brave hosts"
    end = "# END dotfiles Brave hosts"
    managed = blocked | allowed
    kept = []
    in_block = False
    for line in original.splitlines():
        if line == begin:
            if in_block:
                raise ValueError("Nested dotfiles hosts block.")
            in_block = True
            continue
        if line == end:
            if not in_block:
                raise ValueError("Unmatched dotfiles hosts block marker.")
            in_block = False
            continue
        if in_block:
            continue

        body, separator, comment = line.partition("#")
        fields = body.split()
        if len(fields) > 1 and managed.intersection(fields[1:]):
            aliases = [name for name in fields[1:] if name not in managed]
            if aliases:
                line = fields[0] + "\t" + " ".join(aliases)
                if separator:
                    line += " #" + comment
            elif separator:
                line = "#" + comment
            else:
                continue
        kept.append(line)
    if in_block:
        raise ValueError("Unterminated dotfiles hosts block.")

    updated = "\n".join(kept).rstrip() + "\n\n"
    updated += begin + "\n" + "\n".join(block_lines) + "\n" + end + "\n"
    if updated == original:
        return

    metadata = hosts_file.stat() if hosts_file.exists() else None
    backup = hosts_file.with_name(hosts_file.name + ".dotfiles-backup")
    if metadata and not backup.exists():
        shutil.copy2(hosts_file, backup)
    descriptor, temporary = tempfile.mkstemp(dir=hosts_file.parent, prefix=".hosts-")
    try:
        with os.fdopen(descriptor, "w") as output:
            output.write(updated)
            os.fchmod(output.fileno(), metadata.st_mode & 0o777 if metadata else 0o644)
            if metadata:
                os.fchown(output.fileno(), metadata.st_uid, metadata.st_gid)
        os.replace(temporary, hosts_file)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit("Usage: conf-hosts <hosts-blocklist> <hosts-allowlist>")
    configure_hosts(Path(sys.argv[1]), Path(sys.argv[2]))
