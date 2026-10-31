#!/usr/bin/env python3
"""Measure real helper latency and peak RSS against an isolated tmux server.

Run from any directory:
  python3 benchmark_vim_copy.py --rows 1000 50000 --samples 7

Each sample starts a fresh helper with the normal yank highlight enabled.
Timings cover helper completion, excluding keyboard input, browser startup,
and the intentional 150 ms highlight expiry. GNU time supplies RSS when
available. Fixtures and the tmux server are removed on exit. Pass --script
with a saved vim-copy.py, from before the C rewrite, to compare against it.
"""

import argparse
import fcntl
import json
import os
from pathlib import Path
import shutil
import signal
import statistics
import struct
import subprocess
import sys
import termios
import time

import test_vim_copy as tests


class Benchmark(tests.Terminal):
    @classmethod
    def tm(cls, *args):
        if args[0] == '-f':
            config = Path(args[1])
            config.write_text('set -g history-limit 1000000\n' + config.read_text())
        return super().tm(*args)


def place(benchmark, row, column, top):
    """Put the copy cursor on an absolute row with the viewport at top."""
    numbered, limit, height = benchmark.tm(
        'display-message', '-p', '#{&&:#{copy_line_numbers},#{m/r:^(absolute|relative|hybrid)$,'
        '#{copy-mode-line-numbers}}}:#{copy_position_limit}:#{pane_height}').split(':')
    history = int(limit) - (int(height) if numbered == '1' else 0)
    benchmark.tm('send-keys', '-X', 'goto-line', str(top + 1 if numbered == '1' else history - top))
    benchmark.tm('send-keys', '-X', 'top-line')
    if row > top:
        benchmark.tm('send-keys', '-X', '-N', str(row - top), 'cursor-down')
    benchmark.tm('send-keys', '-X', 'start-of-line')
    if column:
        benchmark.tm('send-keys', '-X', '-N', str(column), 'cursor-right')


def positive(value):
    value = int(value)
    if value < 1:
        raise argparse.ArgumentTypeError('must be positive')
    return value


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rows', type=positive, nargs='+', default=[1000, 50000])
    parser.add_argument('--samples', type=positive, default=7)
    parser.add_argument('--text', choices=['ascii', 'unicode', 'both'], default='both')
    parser.add_argument('--script', type=Path, default=tests.HELPER,
                        help='helper to benchmark; a .py file runs with this Python')
    args = parser.parse_args()
    Benchmark.setUpClass()
    benchmark = Benchmark()
    try:
        fcntl.ioctl(benchmark.master, termios.TIOCSWINSZ, struct.pack('HHHH', 55, 190, 0, 0))
        os.kill(benchmark.client.pid, signal.SIGWINCH)
        benchmark.until(lambda: benchmark.tm('display-message', '-p', '#{pane_width}x#{pane_height}') == '190x55')
        client = benchmark.client_name()
        env = {**os.environ, 'TMUX': f'{benchmark.directory / "socket"},0,0'}
        timer = shutil.which('/usr/bin/time') if sys.platform.startswith('linux') else None
        rss_log = benchmark.directory / 'rss'
        # Columns: 19 is in gamma, 25 inside the parentheses, 17 starts gamma.
        operations = [
            ('ge', 19, ['execute', '%0', client, 'move', '1', '0', '-', 'ge'], None),
            ('yiw', 19, ['execute', '%0', client, 'yank', '1', '0', 'i', 'w'], 'gamma'),
            ('yib', 25, ['execute', '%0', client, 'yank', '1', '0', 'i', 'b'], 'one two'),
            # Before any block, Vim takes the next one; only the whole
            # history shows that no block encloses the cursor.
            ('yib ahead', 19, ['execute', '%0', client, 'yank', '1', '0', 'i', 'b'], 'one two'),
            ('yi"', 19, ['execute', '%0', client, 'yank', '1', '0', 'i', '"'], 'quoted text'),
            ('yy', 19, ['execute', '%0', client, 'yank', '1', '0', '-', 'y'], 'line'),
            ('visual y', 17, ['visual-yank', '%0', client], 'gamma'),
        ]
        print(json.dumps({'tmux': benchmark.tm('display-message', '-p', '#{version}'),
                          'script': str(args.script.resolve()), 'pane': '190x55', 'samples': args.samples}), flush=True)
        for unicode in ([False, True] if args.text == 'both' else [args.text == 'unicode']):
            for rows in args.rows:
                suffix = ' 界e\u0301 δείγμα' if unicode else ''
                lines = [f'{i:05} alpha.beta gamma (one two) "quoted text" tail{suffix}' for i in range(rows)]
                benchmark.fixture('\n'.join([*lines, 'BENCH_END']))
                benchmark.until(lambda: 'BENCH_END' in benchmark.tm('capture-pane', '-p'), seconds=30)
                benchmark.tm('send-keys', '-X', 'cancel')
                benchmark.tm('copy-mode')
                if int(benchmark.tm('display-message', '-p', '#{copy_position_limit}')) < rows:
                    raise RuntimeError('The fixture did not fit in tmux history')
                time.sleep(.1)  # Allow the normal background cache warmup to finish.
                for name, column, operation, expected in operations:
                    timings, memory = [], []
                    for _ in range(args.samples):
                        benchmark.tm('send-keys', '-X', 'clear-selection')
                        benchmark.tm('send-keys', '-X', 'rectangle-off')
                        benchmark.tm('set-option', '-pu', '-t', '%0', '@vim-copy-flash')
                        benchmark.tm('set-option', '-pu', '-t', '%0', '@vim-copy-flash-selection')
                        place(benchmark, rows // 2, column, max(0, rows // 2 - 27))
                        if name == 'visual y':
                            benchmark.tm('send-keys', '-X', 'begin-selection')
                            benchmark.tm('send-keys', '-X', '-N', '4', 'cursor-right')
                        benchmark.drain()
                        command = [str(args.script.resolve()), *operation]
                        if args.script.suffix == '.py':
                            command.insert(0, sys.executable)
                        if timer:
                            command = [timer, '-f', '%M', '-o', str(rss_log), *command]
                        start = time.perf_counter()
                        subprocess.run(command, env=env, capture_output=True, text=True, timeout=30, check=True)
                        timings.append((time.perf_counter() - start) * 1000)
                        if timer:
                            memory.append(int(rss_log.read_text()))
                        if expected is not None:
                            wanted = lines[rows // 2] + '\n' if expected == 'line' else expected
                            if benchmark.buffer() != wanted:
                                raise AssertionError(f'{name} copied the wrong text')
                        elif benchmark.tm('display-message', '-p', '#{copy_cursor_x}') != '15':
                            raise AssertionError('ge moved to the wrong word end')
                        time.sleep(.2)  # Let highlight cleanup finish outside the timed sample.
                        benchmark.drain()
                    print(json.dumps({'operation': name, 'rows': rows, 'unicode': unicode,
                                      'median_ms': round(statistics.median(timings), 1),
                                      'min_ms': round(min(timings), 1), 'max_ms': round(max(timings), 1),
                                      'peak_rss_mib': round(max(memory) / 1024, 1) if memory else None}), flush=True)
    finally:
        Benchmark.tearDownClass()


if __name__ == '__main__':
    main()
