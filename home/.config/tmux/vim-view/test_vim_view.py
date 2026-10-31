#!/usr/bin/env python3
"""Tests for vim-view.so, the C side of vim-view.lua, loaded with ctypes.

Run from any directory: python3 test_vim_view.py
"""

import ctypes
import itertools
import os
from pathlib import Path
import shlex
import shutil
import subprocess
import tempfile
import time
import unittest

HERE = Path(__file__).resolve().parent
BUILD = tempfile.TemporaryDirectory(prefix='test-vim-view-build-')
LIBRARY = Path(BUILD.name) / 'vim-view.so'
# CC may carry flags, such as -fsanitize=undefined.
SIGNAL = Path(BUILD.name) / 'vim-view-signal'
for output, source, shared in ((LIBRARY, 'vim-view.c', ['-fPIC', '-shared']), (SIGNAL, 'vim-view-signal.c', [])):
    subprocess.run([*shlex.split(os.environ.get('CC', 'cc')), '-std=c11', '-O2', '-Wall', '-Wextra', '-Werror', *shared,
                    '-o', str(output), str(HERE / source)], check=True)
COPIES = itertools.count()
FILL = 0xFFFFFFFF
BOLD, ITALIC, REVERSE, STRIKETHROUGH, UNDERLINE_SHIFT = 1, 2, 4, 8, 4
TRUE_COLOUR = 0x1000000


class Span(ctypes.Structure):
    _fields_ = [('start', ctypes.c_uint32), ('stop', ctypes.c_uint32), ('attr', ctypes.c_int32)]


class Attr(ctypes.Structure):
    _fields_ = [('fg', ctypes.c_int32), ('bg', ctypes.c_int32), ('sp', ctypes.c_int32), ('flags', ctypes.c_uint32)]


def load():
    """Each test loads its own copy, so library state starts fresh."""
    library = LIBRARY.with_name(f'vim-view-{next(COPIES)}.so')
    shutil.copyfile(LIBRARY, library)
    lib = ctypes.CDLL(str(library))
    size_p, long_p = ctypes.POINTER(ctypes.c_size_t), ctypes.POINTER(ctypes.c_long)
    for name, result, args in [
        ('vv_clear', None, [ctypes.c_long, ctypes.c_long]),
        ('vv_parse', ctypes.c_long, [ctypes.c_char_p, ctypes.c_size_t, ctypes.c_long, ctypes.c_int]),
        ('vv_colour', ctypes.c_long, [ctypes.c_char_p, ctypes.c_size_t, ctypes.c_long, ctypes.c_long]),
        ('vv_chunk', ctypes.c_long, [ctypes.c_long, ctypes.c_long, long_p]),
        ('vv_replace', ctypes.c_long, [ctypes.c_char_p, ctypes.c_size_t, ctypes.c_long, long_p, long_p]),
        ('vv_open_fifo', ctypes.c_int, [ctypes.c_char_p]),
        ('vv_drain', ctypes.c_long, [ctypes.c_int]),
        ('vv_count', ctypes.c_long, []),
        ('vv_text', ctypes.c_void_p, [ctypes.c_long, size_p]),
        ('vv_spans', ctypes.c_int, [ctypes.c_long, ctypes.POINTER(ctypes.POINTER(Span))]),
        ('vv_attribute', ctypes.POINTER(Attr), [ctypes.c_int]),
        ('vv_tail', ctypes.c_long, [ctypes.c_long, ctypes.c_long, ctypes.c_long, long_p]),
        ('vv_locate', ctypes.c_long, [ctypes.c_long, ctypes.c_long, ctypes.c_long, long_p]),
    ]:
        getattr(lib, name).restype = result
        getattr(lib, name).argtypes = args
    return lib


class Library:
    def __init__(self):
        self.lib = load()

    def capture(self, rows, width=20, text_width=16, first=0, prepend=False, clear=True, newline=True):
        """Parse rows as tmux prints them, each ending in a newline."""
        data = ''.join(row + '\n' for row in rows).encode()
        if not newline:
            data = data[:-1]
        if clear:
            self.lib.vv_clear(width, text_width)
        return self.lib.vv_parse(data, len(data), first, int(prepend))

    def text(self, i):
        length = ctypes.c_size_t()
        pointer = self.lib.vv_text(i, ctypes.byref(length))
        return ctypes.string_at(pointer, length.value).decode()

    def lines(self):
        return [self.text(i) for i in range(self.lib.vv_count())]

    def spans(self, i):
        pointer = ctypes.POINTER(Span)()
        count = self.lib.vv_spans(i, ctypes.byref(pointer))
        return [(pointer[k].start, pointer[k].stop, pointer[k].attr) for k in range(count)]

    def attr(self, id):
        a = self.lib.vv_attribute(id).contents
        return a.fg, a.bg, a.sp, a.flags


class Parse(unittest.TestCase):
    def setUp(self):
        self.v = Library()

    def test_colours_become_spans(self):
        v = self.v
        v.capture(['\x1b[31mred\x1b[0m plain \x1b[1;38;5;208mbold\x1b[39m still bold\x1b[0m'], width=80, text_width=75)
        self.assertEqual(v.lines(), ['red plain bold still bold'])
        (s1, e1, a1), (s2, e2, a2), (s3, e3, a3) = v.spans(0)
        self.assertEqual((s1, e1, s2, e2, s3, e3), (0, 3, 10, 14, 14, 25))
        self.assertEqual(v.attr(a1), (1, -1, -1, 0))
        self.assertEqual(v.attr(a2), (208, -1, -1, BOLD))
        self.assertEqual(v.attr(a3), (-1, -1, -1, BOLD))

    def test_colour_forms(self):
        v = self.v
        v.capture(['\x1b[38;2;1;2;3ma\x1b[38:2::4:5:6mb\x1b[38:2:7:8:9mc\x1b[48:5:17md\x1b[0;4:3;58;2;10;11;12me'
                   '\x1b[0;21;7;9;3mf\x1b[0;97;104mg'], width=80, text_width=75)
        self.assertEqual(v.lines(), ['abcdefg'])
        attrs = [v.attr(a) for _, _, a in v.spans(0)]
        self.assertEqual(attrs, [
            (TRUE_COLOUR | 0x010203, -1, -1, 0),
            (TRUE_COLOUR | 0x040506, -1, -1, 0),
            (TRUE_COLOUR | 0x070809, -1, -1, 0),
            (TRUE_COLOUR | 0x070809, 17, -1, 0),
            (-1, -1, TRUE_COLOUR | 0x0A0B0C, 3 << UNDERLINE_SHIFT),
            (-1, -1, -1, 2 << UNDERLINE_SHIFT | REVERSE | STRIKETHROUGH | ITALIC),
            (15, 12, -1, 0),
        ])

    def test_attributes_carry_over_lines(self):
        # tmux writes only changes: a block's background is set once, on its
        # first line, and a reset ends it.
        v = self.v
        v.capture(['\x1b[48;5;236m> first' + ' ' * 13, '  second' + ' ' * 12, '\x1b[1mbold\x1b[22m' + ' ' * 16,
                   '\x1b[49mplain'])
        (s1, e1, a1), (_, f1, _) = v.spans(0)
        self.assertEqual([(s, e) for s, e, _ in v.spans(1)], [(0, 8), (8, FILL)])
        self.assertEqual(v.attr(v.spans(1)[0][2]), (-1, 236, -1, 0))
        self.assertEqual(v.attr(v.spans(2)[0][2]), (-1, 236, -1, BOLD))
        self.assertEqual(v.spans(3), [])

    def test_same_attributes_share_an_id(self):
        v = self.v
        v.capture(['\x1b[32mone\x1b[0m two \x1b[32mthree'], width=80, text_width=75)
        (_, _, a), (_, _, b) = v.spans(0)
        self.assertEqual(a, b)

    def test_hyperlinks_carry_no_text(self):
        v = self.v
        v.capture(['see \x1b]8;id=1;http://a\x1b\\\x1b[34mlink\x1b]8;;\x1b\\ and \x1b]8;;http://b\x07there\x1b]8;;\x07.'],
                  width=80, text_width=75)
        self.assertEqual(v.lines(), ['see link and there.'])
        self.assertEqual([(s, e) for s, e, _ in v.spans(0)], [(4, 19)])

    def test_padding_is_dropped_and_coloured_padding_fills(self):
        v = self.v
        v.capture(['text' + ' ' * 16, '\x1b[42m+ added' + ' ' * 13 + '\x1b[0m', '\x1b[32mgreen  \x1b[0m', '   '])
        self.assertEqual(v.lines(), ['text', '+ added', 'green', ''])
        (start, stop, attr), (fill_start, fill_stop, fill_attr) = v.spans(1)
        self.assertEqual((start, stop, fill_start, fill_stop), (0, 7, 7, FILL))
        self.assertEqual(attr, fill_attr)
        self.assertEqual(v.spans(2)[0][:2], (0, 5))
        self.assertEqual(len(v.spans(2)), 1)

    def test_rules_lose_their_end(self):
        v = self.v
        v.capture(['─' * 20, '\x1b[90m' + '─' * 18 + '\x1b[0m', '─' * 10, 'ab' + '─' * 18])
        self.assertEqual(v.lines(), ['─' * 16, '─' * 16, '─' * 10, 'ab' + '─' * 18])
        self.assertEqual(v.spans(1)[0][:2], (0, 16 * 3))

    def test_right_aligned_text_moves_into_its_gap(self):
        v = self.v
        # 20 cells beside a 16-cell text area: gaps lose 4 spaces, keeping 2.
        v.capture(['left \x1b[31m' + ' ' * 10 + 'right\x1b[0m', 'a b c d e f g h i j', '  indent   x' + ' ' * 7 + 'y'])
        self.assertEqual(v.lines(), ['left' + ' ' * 7 + 'right', 'a b c d e f g h i j', '  indent   x   y'])
        self.assertEqual(v.spans(0)[0][:2], (5, 16))

    def test_lines_wider_than_the_pane_keep_their_text(self):
        v = self.v
        v.capture(['x' * 30, 'tabs\tare\tcells'])
        self.assertEqual(v.lines(), ['x' * 30, 'tabs\tare\tcells'])

    def test_wide_characters_count_two_cells(self):
        v = self.v
        v.capture(['界' * 6 + ' ' * 7 + 'z'])
        self.assertEqual(v.lines(), ['界' * 6 + ' ' * 3 + 'z'])

    def test_output_ending_inside_a_line(self):
        # tmux leaves out the last newline when the last row continues.
        self.assertEqual(self.v.capture(['a', 'b' * 20], newline=False), 2)
        self.assertEqual(self.v.lines(), ['a', 'b' * 20])
        self.assertEqual(self.v.capture([], newline=False), 0)

    def test_replace_and_prepend(self):
        v = self.v
        self.assertEqual(v.capture(['a', 'b', 'c']), 3)
        v.capture(['C', 'D'], first=2, clear=False)
        self.assertEqual(v.lines(), ['a', 'b', 'C', 'D'])
        v.capture(['x', '\x1b[31my'], prepend=True, clear=False)
        self.assertEqual(v.lines(), ['x', 'y', 'a', 'b', 'C', 'D'])
        self.assertEqual(v.spans(1)[0][:2], (0, 1))
        v.capture([], first=1, clear=False)
        self.assertEqual(v.lines(), ['x'])

    def colour(self, rows, first, stop):
        data = ''.join(row + '\n' for row in rows).encode()
        return self.v.lib.vv_colour(data, len(data), first, stop)

    def test_colours_arrive_for_lines_of_the_same_text(self):
        v = self.v
        v.capture(['a', 'b', 'c', 'd', 'e'])
        # The capture starts inside the line before b, and c's text changed.
        self.assertEqual(self.colour(['x', '\x1b[31mb', '\x1b[32mC', '\x1b[33md'], 1, 4), 2)
        self.assertEqual([len(v.spans(i)) for i in range(5)], [0, 1, 0, 1, 0])
        self.assertEqual(v.attr(v.spans(3)[0][2])[0], 3)
        # Starting a line late: the fragment matches nothing, the rest shifts.
        self.assertEqual(self.colour(['\x1b[34mfragment', '\x1b[34mb', '\x1b[34mc'], 0, 5), 2)
        self.assertEqual([len(v.spans(i)) for i in range(5)], [0, 1, 1, 1, 0])
        self.assertEqual(self.colour(['\x1b[35me'], 0, 4), 0)

    def test_chunks_take_whole_lines(self):
        v = self.v
        v.capture(['a', 'w' * 45, 'b', 'c'])
        taken = ctypes.c_long()
        self.assertEqual((v.lib.vv_chunk(4, 3, ctypes.byref(taken)), taken.value), (2, 2))
        self.assertEqual((v.lib.vv_chunk(2, 2, ctypes.byref(taken)), taken.value), (1, 3))
        self.assertEqual((v.lib.vv_chunk(4, 100, ctypes.byref(taken)), taken.value), (0, 6))
        self.assertEqual((v.lib.vv_chunk(0, 100, ctypes.byref(taken)), taken.value), (0, 0))

    def replace(self, rows, first):
        data = ''.join(row + '\n' for row in rows).encode()
        old_end, new_end = ctypes.c_long(), ctypes.c_long()
        changed = self.v.lib.vv_replace(data, len(data), first, ctypes.byref(old_end), ctypes.byref(new_end))
        return changed, old_end.value, new_end.value

    def test_replace_keeps_unchanged_lines(self):
        v = self.v
        v.capture(['a', 'b', 'c', 'd', 'e'])
        self.assertEqual(self.replace(['b', 'c', 'd', 'e'], 1), (5, 5, 5))
        self.assertEqual(self.replace(['b', 'X', 'd', 'e'], 1), (2, 3, 3))
        self.assertEqual(v.lines(), ['a', 'b', 'X', 'd', 'e'])
        # Output appended, and a line that only changed colour.
        self.assertEqual(self.replace(['b', 'X', 'd', '\x1b[31me\x1b[0m', 'f', 'g'], 1), (4, 5, 7))
        self.assertEqual(v.lines(), ['a', 'b', 'X', 'd', 'e', 'f', 'g'])
        self.assertEqual(len(v.spans(4)), 1)
        # Lines inserted above a footer that stays.
        self.assertEqual(self.replace(['b', 'X', 'd', 'new', '\x1b[31me\x1b[0m', 'f', 'g'], 1), (4, 4, 5))
        self.assertEqual(v.lines(), ['a', 'b', 'X', 'd', 'new', 'e', 'f', 'g'])
        self.assertEqual(self.replace(['b'], 1), (2, 8, 2))
        self.assertEqual(v.lines(), ['a', 'b'])
        self.assertEqual(self.replace([], 0), (0, 2, 0))
        self.assertEqual(v.lines(), [])

    def test_fifo_drains(self):
        path = Path(BUILD.name) / f'fifo-{next(COPIES)}'
        fd = self.v.lib.vv_open_fifo(str(path).encode())
        self.assertGreaterEqual(fd, 0)
        writer = os.open(path, os.O_WRONLY | os.O_NONBLOCK)
        os.write(writer, b'x' * 50000)
        self.assertEqual(self.v.lib.vv_drain(fd), 50000)
        self.assertEqual(self.v.lib.vv_drain(fd), 0)
        os.close(writer)
        # With the writer gone, the FIFO still has a writer: itself.
        self.assertEqual(self.v.lib.vv_drain(fd), 0)
        os.close(fd)

    def test_signal_helper_tells_changes_not_bytes(self):
        lib = self.v.lib
        path = Path(BUILD.name) / f'fifo-{next(COPIES)}'
        fd = lib.vv_open_fifo(str(path).encode())
        helper = subprocess.Popen([str(SIGNAL), str(path)], stdin=subprocess.PIPE)
        def told(within):
            deadline, total = time.monotonic() + within, 0
            while time.monotonic() < deadline:
                total += lib.vv_drain(fd)
                time.sleep(0.002)
            return total
        start = time.monotonic()
        helper.stdin.write(b'x'); helper.stdin.flush()
        while not lib.vv_drain(fd):
            self.assertLess(time.monotonic() - start, 0.05, 'the first output is told at once')
            time.sleep(0.001)
        # A flood is told a few times, and once more after it ends.
        start = time.monotonic()
        for _ in range(200):
            helper.stdin.write(b'y' * 65536)
        helper.stdin.flush()
        seconds = time.monotonic() - start
        bytes_told = told(0.1)
        self.assertGreaterEqual(bytes_told, 1)
        self.assertLessEqual(bytes_told, seconds / 0.02 + 3)
        self.assertEqual(told(0.1), 0)
        helper.stdin.close()
        self.assertEqual(helper.wait(timeout=2), 0)
        os.close(fd)

    def test_signal_helper_needs_a_viewer(self):
        path = Path(BUILD.name) / f'fifo-{next(COPIES)}'
        os.mkfifo(path)
        self.assertEqual(subprocess.run([str(SIGNAL), str(path)], stdin=subprocess.DEVNULL).returncode, 1)

    def test_tail_and_locate_count_tmux_rows(self):
        v = self.v
        # Width 20: the 45-cell line takes three rows, padding included.
        v.capture(['h0', 'h1', 'w' * 45, 'p' * 5 + ' ' * 30, 's0', 's1'])
        anchor = ctypes.c_long()
        first = v.lib.vv_tail(0, 4, 104, ctypes.byref(anchor))
        self.assertEqual((first, anchor.value), (3, 100))
        first = v.lib.vv_tail(0, 5, 104, ctypes.byref(anchor))
        self.assertEqual((first, anchor.value), (2, 97))
        offset = ctypes.c_long()
        self.assertEqual((v.lib.vv_locate(2, 97, 99, ctypes.byref(offset)), offset.value), (2, 40))
        self.assertEqual((v.lib.vv_locate(2, 97, 101, ctypes.byref(offset)), offset.value), (3, 20))
        self.assertEqual((v.lib.vv_locate(2, 97, 103, ctypes.byref(offset)), offset.value), (5, 0))


if __name__ == '__main__':
    unittest.main()
