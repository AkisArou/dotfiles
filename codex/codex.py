import json
import os
import sys
from pathlib import Path


def main() -> None:
    project = json.dumps(str(Path.cwd()))
    config = f'projects={{{project}={{trust_level="trusted"}}}}'
    os.execvp("codex", ["codex", "-c", config, *sys.argv[1:]])


if __name__ == "__main__":
    main()
