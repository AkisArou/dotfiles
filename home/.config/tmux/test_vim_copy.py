"""Range tests and real terminal input against a separate tmux server."""

import importlib.util
import json
import os
from pathlib import Path
import pty
import random
import re
import signal
import shutil
import struct
import subprocess
import sys
import tempfile
import time
import unittest
import fcntl
import termios


SCRIPT = Path(__file__).with_name('vim-copy.py')
spec = importlib.util.spec_from_file_location('vim_copy', SCRIPT)
copy = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = copy
spec.loader.exec_module(copy)


class Ranges(unittest.TestCase):
    def yank(self, text, column, motion, expected, count=1, argument=None, explicit=False):
        document = copy.Document(text)
        span = document.motion(column, motion, count, argument, explicit)
        self.assertEqual(document.text[span.start:span.end], expected)

    def object(self, text, column, key, expected, around=False, count=1):
        document = copy.Document(text)
        span = document.text_object(column, key, around, count)
        self.assertEqual(document.text[span.start:span.end], expected)

    def test_word_motions_and_endpoint_rules(self):
        for key, expected in [('w', 'alpha'), ('W', 'alpha.beta '), ('e', 'alpha'), ('E', 'alpha.beta')]:
            with self.subTest(key=key):
                self.yank('alpha.beta next', 0, key, expected)
        self.yank('alpha beta gamma', 0, 'w', 'alpha beta ', 2)
        self.yank('alpha beta', 8, 'b', 'be')
        self.yank('alpha beta', 5, '0', 'alpha')
        self.yank('  alpha beta', 5, '^', 'alp')
        self.yank('alpha\n  beta', 0, 'w', 'alpha')
        self.yank('alpha\n  beta', 0, 'w', 'alpha\n  beta', 2)
        self.yank('abc', 2, 'l', 'c')
        self.yank('abc', 2, 'h', 'ab', 10)

    def test_character_searches(self):
        text = 'aa, bb, cc'
        for key, column, count, expected in [
            ('t', 0, 1, 'aa'), ('f', 0, 1, 'aa,'),
            ('t', 0, 2, 'aa, bb'), ('F', 9, 1, ', cc'), ('T', 9, 1, ' cc')]:
            with self.subTest(key=key, count=count):
                self.yank(text, column, key, expected, count, ',')
        with self.assertRaises(copy.NoMotion):
            copy.Document(text).motion(0, 'f', argument='!')

    def test_backward_word_end_motions_and_inclusive_yanks(self):
        document = copy.Document('alpha.beta gamma')
        for position, count, big, target in [
            (12, 1, False, 9), (12, 2, False, 5), (12, 3, False, 4),
            (8, 1, False, 5), (8, 1, True, 0), (12, 2, True, 0),
            (10, 1, False, 9), (9, 1, False, 5), (3, 10, False, 0),
        ]:
            with self.subTest(position=position, count=count, big=big):
                self.assertEqual(document.previous_word_end(position, count, big), target)
        self.yank(document.text, 12, 'ge', 'a ga')
        self.yank(document.text, 12, 'ge', '.beta ga', 2)
        self.yank(document.text, 8, 'gE', 'alpha.bet')
        document = copy.Document('one\n\n\nsecond')
        self.assertEqual(document.previous_word_end(document.starts[3] + 2), document.starts[2])
        self.assertEqual(document.previous_word_end(document.starts[3] + 2, 2), document.starts[1])
        self.assertEqual(copy.Document('one\n   \ntwo').previous_word_end(8), 2)
        self.assertEqual(copy.Document('界 e\u0301 word').previous_word_end(7), 2)
        with self.assertRaises(copy.NoMotion):
            copy.Document('word').motion(0, 'ge')

    def test_linewise_motions_and_counts(self):
        text = 'one\ntwo\nthree\nfour'
        self.yank(text, 5, 'y', 'two\nthree\n', 2)
        self.yank(text, 5, 'j', 'two\nthree\n')
        self.yank(text, 5, 'k', 'one\ntwo\n')
        self.yank(text, 5, 'G', 'two\nthree\nfour\n')
        self.yank(text, 5, 'G', 'two\nthree\n', 3, explicit=True)
        self.yank(text, 5, 'gg', 'one\ntwo\n')
        self.yank(text, 5, '$', 'wo\nthree', 2)

    def test_line_yanks_trim_right_padding_and_preserve_indentation(self):
        document = copy.Document('  first   \n   \n  second \t\n')
        self.assertEqual(document.copy_text(document.line_span(0, 2)), '  first\n   \n  second\n')
        # Explicit character selections can intentionally include spaces.
        self.assertEqual(document.copy_text(copy.Span(0, 10)), '  first   ')

    def test_word_objects_at_start_middle_whitespace_and_end(self):
        self.object('previous alpha.beta next', 9, 'W', 'alpha.beta')
        self.object('previous alpha.beta next', 13, 'W', 'alpha.beta')
        self.object('previous alpha.beta next', 13, 'w', 'alpha')
        self.object('alpha   beta', 6, 'w', '   ')
        self.object('alpha   beta', 6, 'w', '   beta', around=True)
        self.object('alpha   beta', 0, 'w', 'alpha   ', around=True)
        self.object('alpha   beta', 8, 'w', '   beta', around=True)
        self.object('alpha   beta gamma', 0, 'w', 'alpha   ', count=2)
        self.object('alpha   beta gamma', 0, 'w', 'alpha   beta ', around=True, count=2)

    def test_nested_multiline_objects_and_aliases(self):
        text = '(outer (inner) tail)'
        self.object(text, text.index('tail'), 'b', 'outer (inner) tail')
        self.object(text, text.index('inner'), '(', 'inner')
        self.object(text, text.index('inner'), ')', 'outer (inner) tail', count=2)
        self.object(text, text.index('tail'), 'b', text, around=True)
        for opening, closing, aliases in [('(', ')', 'b()'), ('{', '}', 'B{}'), ('[', ']', '[]'), ('<', '>', '<>')]:
            text = opening + 'one\n two' + closing
            for alias in aliases:
                with self.subTest(alias=alias):
                    self.object(text, 3, alias, 'one\n two')
                    self.object(text, 3, alias, text, around=True)
        self.object('(one ")" two)', 2, '(', 'one ")" two')

    def test_quotes_escape_characters_and_failed_objects(self):
        self.object('prefix "one \\" two" suffix', 10, '"', 'one \\" two')
        self.object("'one' 'two'", 8, "'", " 'two'", around=True)
        self.object('before `one` after', 9, '`', 'one')
        with self.assertRaises(copy.NoMotion):
            copy.Document('no brackets').text_object(3, '(')

    def test_bracket_queries_match_full_context_matching(self):
        rng = random.Random(43)
        for _ in range(400):
            document = copy.Document(''.join(rng.choice([
                'abc', ' ', '\n', '(', ')', '[', ']', '{', '}', '<', '>',
                "'", '"', '`', '\\', '界',
            ]) for _ in range(40)))
            position = rng.randrange(len(document.text))
            for key in '([{<':
                count = rng.choice([1, 2, 3])
                with self.subTest(text=document.text, position=position, key=key, count=count):
                    try:
                        expected = document.enclosing(document.brackets(key), position, count)
                    except copy.NoMotion:
                        expected = None
                    try:
                        actual = document.enclosing(document.brackets(key, position, count), position, count)
                    except copy.NoMotion:
                        actual = None
                    self.assertEqual(actual, expected)

    def test_any_quote_object(self):
        for delimiter in '\'"`':
            text = 'before ' + delimiter + 'quoted text' + delimiter + ' after'
            with self.subTest(delimiter=delimiter):
                self.object(text, text.index('quoted') + 2, 'q', 'quoted text')
                self.object(text, 0, 'q', 'quoted text')
                self.object(text, text.index('quoted'), 'q', delimiter + 'quoted text' + delimiter + ' ', around=True)
                self.object(text, text.index('quoted'), 'q', delimiter + 'quoted text' + delimiter, count=2)
        text = '''before "outer 'inner' tail" `last`'''
        self.object(text, text.index('inner'), 'q', 'inner')
        self.object(text, text.index('tail'), 'q', "outer 'inner' tail")
        self.object(text, text.index('last'), 'q', 'last')
        text = '''don't skip "it's quoted" then 'next' '''
        self.object(text, 0, 'q', "it's quoted")
        self.object(text, text.index('quoted'), 'q', "it's quoted")
        self.object('before "one \\" two" after', 12, 'q', 'one \\" two')
        with self.assertRaises(copy.NoMotion):
            copy.Document("don't match unmatched '").text_object(0, 'q')

    def test_paragraphs_and_sentences(self):
        text = 'first\nparagraph\n\nsecond\n\n'
        self.object(text, 8, 'p', 'first\nparagraph\n')
        self.object(text, 8, 'p', 'first\nparagraph\n\n', around=True)
        self.object(text, 1, 'p', text, around=True, count=2)
        self.object('One sentence.  Another! Last?', 4, 's', 'One sentence.')
        self.object('One sentence.  Another! Last?', 4, 's', 'One sentence.  ', around=True)
        self.object('One sentence.  Another! Last?', 4, 's', 'One sentence.  ', count=2)
        self.object('One sentence.  Another! Last?', 4, 's', 'One sentence.  Another! ', around=True, count=2)

    def test_sentence_motion_boundaries_and_counts(self):
        text = '''  One sentence.")  Another!\tLast?\ncontinued\n\n\n  New paragraph'''
        document = copy.Document(text)
        first, second, third = text.index('One'), text.index('Another'), text.index('Last')
        blank, paragraph = text.index('\n\n') + 1, text.index('New')
        for origin, key, count, expected in [
            (0, ')', 1, first), (first, ')', 1, second),
            (first, ')', 2, third), (second + 3, '(', 1, second),
            (second, '(', 1, first), (third, ')', 1, text.index('continued')),
            (text.index('continued'), ')', 1, blank),
            (blank, ')', 1, paragraph), (paragraph, '(', 1, blank + 1),
            (first, '(', 99, 0), (paragraph, ')', 99, len(document.text)),
        ]:
            with self.subTest(origin=origin, key=key, count=count):
                self.assertEqual(document.sentence_target(origin, key, count), expected)
        self.yank('One.  Two! Three?', 0, ')', 'One.  ')
        self.yank('One.  Two! Three?', 0, ')', 'One.  Two! ', 2)
        self.yank('One.  Two! Three?', 8, '(', 'Tw')
        self.yank('One.\nTwo!', 0, ')', 'One.\n')
        self.yank('Last sentence.', 0, ')', 'Last sentence.')
        self.assertEqual(copy.Document('version1.2 still going\n  \nnext').sentence_starts, [0])

    def test_vim_paragraph_boundaries_counts_and_endpoints(self):
        text = '  alpha\n   \nbeta\n\n\n  gamma\n.IP list\nitem\n.SH title\nsection\n'
        document = copy.Document(text)
        for row, column, key, count, target in [
            (0, 3, '}', 1, 3), (0, 3, '}', 2, 6),
            (3, 0, '}', 1, 6), (4, 0, '}', 1, 6),
            (5, 3, '{', 1, 4), (6, 4, '{', 1, 4),
            (7, 2, '{', 1, 6), (9, 2, '{', 1, 8),
            (6, 3, '}', 1, 8), (3, 0, '{', 1, 0),
            (0, 6, '{', 1, 0),
        ]:
            with self.subTest(row=row, key=key, count=count):
                self.assertEqual(document.paragraph_target(document.starts[row] + column, key, count),
                                 document.starts[target])
        for line, boundary in [('', True), (' ', False), ('\t', False),
                               ('.IP', True), ('.P text', True), ('.Pstuff', False),
                               ('.SH title', True), (' .IP', False), ('\fsection', True),
                               ('{', False), ('}', False)]:
            with self.subTest(line=line):
                self.assertEqual(copy.Document(line).paragraph_boundary(0), boundary)
        document = copy.Document('first\n界e\u0301')
        self.assertEqual(document.paragraph_target(0, '}'), document.starts[1] + 1)
        self.yank('first\nlast', 0, '}', 'first\nlast')
        self.yank('first\n   \nlast\n\nnext', 0, '}', 'first\n   \nlast\n')
        with self.assertRaises(copy.NoMotion):
            copy.Document('one\ntwo').paragraph_target(0, '}', 3)

    def test_tags_handle_nesting_attributes_comments_and_void_elements(self):
        text = '<div title="a>b"><p>first <b>bold</b> tail</p><!-- </div> --><br></div>'
        self.object(text, text.index('tail'), 't', 'first <b>bold</b> tail')
        self.object(text, text.index('bold'), 't', '<p>first <b>bold</b> tail</p>', around=True, count=2)
        self.object(text, text.index('tail'), 't', text, around=True, count=2)

    def test_layout_maps_wrapped_lines_wide_and_combining_characters(self):
        document = copy.Document('1234567890abcdefghijX\n界e\u0301abc\n')
        layout = copy.Layout(document, 10)
        self.assertEqual(layout.rows, [(0, 10), (10, 20), (20, 21), (22, 28)])
        self.assertEqual(layout.point(20), (2, 0))
        self.assertEqual(layout.point(25), (3, 3))
        self.assertEqual(layout.offset(3, 3), 25)

    def test_link_at_handles_markup_punctuation_and_url_delimiters(self):
        for text, expected in [
            ('See https://example.test/a?b=c&d=e#part.', 'https://example.test/a?b=c&d=e#part'),
            ('[docs](https://example.test/a_(b)).', 'https://example.test/a_(b)'),
            ('<https://example.test/>', 'https://example.test/'),
            ("'https://example.test/a'", 'https://example.test/a'),
            ('"https://example.test/a"', 'https://example.test/a'),
            ('`https://example.test/a`', 'https://example.test/a'),
            ('www.example.test/path!', 'https://www.example.test/path'),
            ('mailto:person@example.test', 'mailto:person@example.test'),
            ('https://[::1]:8000/a', 'https://[::1]:8000/a'),
            ('界 https://example.test/διαδρομή', 'https://example.test/διαδρομή'),
            ("https://example.test/a'b;$HOME?q=$(id)#%1", "https://example.test/a'b;$HOME?q=$(id)#%1"),
        ]:
            with self.subTest(text=text):
                position = text.index('example') if 'example' in text else text.index('8000')
                self.assertEqual(copy.link_at(text, position), expected)

    def test_link_at_requires_cursor_inside_the_specific_link(self):
        text = 'https://one.test/a and https://two.test/b.'
        self.assertEqual(copy.link_at(text, text.index('two')), 'https://two.test/b')
        for position in (text.index(' and ') + 1, len(text) - 1):
            with self.assertRaises(copy.NoMotion):
                copy.link_at(text, position)
        with self.assertRaises(copy.NoMotion):
            copy.link_at('no link here', 4)


class SnapshotRanges(unittest.TestCase):
    def test_adaptive_reads_match_full_buffer_ranges(self):
        class Slices(copy.Tmux):
            def __init__(self, document, width):
                self.document = document
                self.layout = copy.Layout(document, width)
                self.width = width
                self.first_offset = 0

            def snapshot(self, state, first=0, last=None):
                last = len(self.layout.rows) - 1 if last is None else last
                start, end = self.layout.rows[first][0], self.layout.rows[last][1]
                # capture-pane emits a newline for the last physical row,
                # including an empty row or a truncated wrapped line.
                document = copy.Document(self.document.text[start:end] + '\n')
                state['layout'] = copy.Layout(document, self.width, first)
                self.first_offset = start
                return document

        rng = random.Random(30)
        document = copy.Document('\n'.join(rng.choice([
            'alpha.beta gamma', '', '   ', '"one two" rest', '(one [two]) tail',
            'One. Two! Three?', '.IP item', '界 e\u0301 λέξη',
        ]) for _ in range(180)))
        for width in (17, 60):
            tmux = Slices(document, width)
            for _ in range(1200):
                row = rng.randrange(len(tmux.layout.rows))
                start, end = tmux.layout.rows[row]
                position = rng.randrange(start, end) if end > start else start
                while position > start and copy.cell_width(document.text[position]) == 0:
                    position -= 1
                _, column = tmux.layout.point(position)
                key = rng.choice(['y', 'j', 'k', 'w', 'W', 'b', 'B', 'e', 'E', 'ge', 'gE',
                                  '0', '^', '$', 'h', 'l', '{', '}', '(', ')', 'p', 's', 'q', '"', 't'])
                obj = (rng.choice(['i', 'a']) if key in ('w', 'W', 'p', 's', 'q', '"', 't', '(')
                       and rng.choice([True, False]) else None)
                count = rng.choice([1, 2, 5, 20])
                context = {'mode': 'yank', 'motion': key, 'object': obj, 'before': 1,
                           'digits': str(count) if count > 1 else ''}
                state = {'row': row, 'copy_cursor_x': column, 'history': len(tmux.layout.rows) - 12,
                         'pane_height': 12, 'pane_width': width, 'selection_present': 0}
                with self.subTest(width=width, row=row, key=key, object=obj, count=count):
                    try:
                        expected = (document.text_object(position, key, obj == 'a', count) if obj else
                                    document.motion(position, key, count, explicit=count > 1))
                    except copy.NoMotion:
                        expected = None
                    try:
                        _, actual = tmux.operation_snapshot(state, context, count)
                        actual = copy.Span(actual.start + tmux.first_offset, actual.end + tmux.first_offset,
                                           actual.linewise, actual.cursor + tmux.first_offset
                                           if actual.cursor is not None else None)
                    except copy.NoMotion:
                        actual = None
                    self.assertEqual(actual, expected)


@unittest.skipUnless(shutil.which('tmux'), 'tmux is required for terminal integration')
class Terminal(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temporary = tempfile.TemporaryDirectory(prefix='test-tmux-vim-copy-')
        cls.directory = Path(cls.temporary.name)
        cls.base = ['tmux', '-S', str(cls.directory / 'socket')]
        config = cls.directory / 'tmux.conf'
        bindings = subprocess.check_output(['python3', str(SCRIPT), 'bindings'], text=True)
        centering = SCRIPT.parents[1].parent / '.tmux.conf'
        configuration = centering.read_text()
        numbering = next(line for line in configuration.splitlines()
                         if line.startswith('set -gw copy-mode-line-numbers '))
        centering = configuration.split('# ALWAYS CENTERED', 1)[1]
        config.write_text('set -g mode-keys vi\nset -g status off\nset -g escape-time 0\nset -g set-clipboard off\n'
                          'set -as terminal-features ",xterm-256color:RGB"\n'
                          # Simulate tmux-open loading before our bindings.
                          'bind -T copy-mode-vi o run-shell "xdg-open SHOULD_NOT_OPEN"\n'
                          'bind -T copy-mode-vi v send -X begin-selection\n' + numbering + '\n' + bindings + centering)
        cls.tm('-f', str(config), 'new-session', '-d', '-x', '60', '-y', '12', '-s', 'test', 'sleep 1000')
        # Exercise the real gx binding without launching a browser.
        binaries = cls.directory / 'bin'
        binaries.mkdir()
        opener = binaries / 'xdg-open'
        cls.link_log = cls.directory / 'opened-links.jsonl'
        opener.write_text('#!/usr/bin/env python3\nimport json,sys\n'
                          'with open(' + repr(str(cls.link_log)) + ', "a") as output:\n'
                          '    output.write(json.dumps(sys.argv[1:]) + "\\n")\n')
        opener.chmod(0o755)
        cls.tm('set-environment', '-g', 'PATH', str(binaries) + os.pathsep + os.environ['PATH'])
        cls.tm('set-environment', 'PATH', str(binaries) + os.pathsep + os.environ['PATH'])
        cls.master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 12, 60, 0, 0))
        env = os.environ.copy()
        env.pop('TMUX', None)
        env['TERM'] = 'xterm-256color'
        cls.client = subprocess.Popen([*cls.base, 'attach-session', '-t', 'test'], stdin=slave,
                                      stdout=slave, stderr=slave, env=env, start_new_session=True)
        os.close(slave)
        os.set_blocking(cls.master, False)
        time.sleep(.1)

    @classmethod
    def tearDownClass(cls):
        cls.tm('kill-server')
        # A client flushing terminal output must not block on an unread PTY.
        os.close(cls.master)
        cls.client.wait(timeout=3)
        cls.temporary.cleanup()

    @classmethod
    def tm(cls, *args):
        return subprocess.run([*cls.base, *args], check=True, text=True,
                              capture_output=True, timeout=10).stdout.rstrip('\n')

    def drain(self):
        while True:
            try:
                output = os.read(self.master, 100000)
                if not output:
                    break
                if hasattr(self, 'terminal_output'):
                    self.terminal_output.extend(output)
            except BlockingIOError:
                break

    def fixture(self, text, row=0, column=0, later=None):
        client = self.tm('list-clients', '-F', '#{client_name}').splitlines()[0]
        self.tm('send-keys', '-K', '-c', client, 'Escape')
        self.tm('clear-history')
        self.tm('set-option', '-pu', '@vim-copy-flash')
        self.tm('set-option', '-pu', '@vim-copy-flash-selection')
        self.tm('set-option', '-pu', '@vim-copy-last-selection')
        self.drain()
        self.terminal_output = bytearray()
        script = self.directory / 'fixture.py'
        source = 'import sys,time\nfrom pathlib import Path\nsys.stdout.write(' + repr(text.replace('\n', '\r\n')) + ');sys.stdout.flush()\n'
        if later:
            trigger = self.directory / 'continue'
            trigger.unlink(missing_ok=True)
            source += 'while not Path(' + repr(str(trigger)) + ').exists(): time.sleep(.01)\n'
            source += 'sys.stdout.write(' + repr(later.replace('\n', '\r\n')) + ');sys.stdout.flush()\n'
        script.write_text(source + 'time.sleep(1000)\n')
        self.tm('respawn-pane', '-k', 'python3 ' + str(script))
        time.sleep(.07)
        self.tm('copy-mode')
        self.tm('send-keys', '-X', 'history-top')
        if row:
            self.tm('send-keys', '-X', '-N', str(row), 'cursor-down')
        self.tm('send-keys', '-X', 'start-of-line')
        if column:
            self.tm('send-keys', '-X', '-N', str(column), 'cursor-right')
        self.tm('set-buffer', '--', 'UNCHANGED')

    def buffer(self):
        return subprocess.check_output([*self.base, 'save-buffer', '-'], text=True)

    def opened_links(self):
        return [json.loads(line) for line in self.link_log.read_text().splitlines()] if self.link_log.exists() else []

    def open_link(self, expected):
        before = len(self.opened_links())
        self.keys('gx')
        self.until(lambda: len(self.opened_links()) > before)
        self.assertEqual(self.opened_links()[before:], [[expected]])

    def keys(self, sequence, expected=None):
        # Real key events, rather than one terminal paste. In particular a
        # single-character command-prompt needs a subsequent input event.
        previous_flash = self.tm('show-options', '-pqv', '@vim-copy-flash')
        for char in sequence:
            os.write(self.master, char.encode())
            time.sleep(.025)
        deadline = time.monotonic() + (4 if expected is not None else .4)
        while time.monotonic() < deadline:
            time.sleep(.025)
            self.drain()
            if expected is not None and self.buffer() == expected:
                flash = self.tm('show-options', '-pqv', '@vim-copy-flash')
                if flash and flash != previous_flash:
                    return
        if expected is not None:
            messages = '\n'.join(line for line in self.tm('show-messages').splitlines() if 'save-buffer' not in line)
            self.assertEqual(self.buffer(), expected, messages[:3000])

    def until(self, predicate, seconds=2):
        deadline = time.monotonic() + seconds
        while not predicate():
            self.assertLess(time.monotonic(), deadline, 'Timed out waiting for tmux')
            time.sleep(.015)
            self.drain()

    def test_yank_motions_and_operator_counts(self):
        cases = [('yt,', 'alpha.beta'), ('yf,', 'alpha.beta,'),
                 ('yw', 'alpha'), ('ye', 'alpha'), ('yW', 'alpha.beta, '),
                 ('y2w', 'alpha.'), ('2yw', 'alpha.'), ('2y2w', 'alpha.beta, '),
                 ('yy', 'alpha.beta, gamma delta\n'), ('3yy', 'alpha.beta, gamma delta\nsecond\nthird\n'),
                 ('yj', 'alpha.beta, gamma delta\nsecond\n')]
        for sequence, expected in cases:
            with self.subTest(sequence=sequence):
                self.fixture('alpha.beta, gamma delta\nsecond\nthird\nfourth')
                self.keys(sequence, expected)

    def test_ge_and_gE_counts_wrapping_and_empty_lines(self):
        for text, row, column, keys, target_row, target_column in [
            ('alpha.beta gamma', 0, 12, 'ge', 0, 9),
            ('alpha.beta gamma', 0, 12, '2ge', 0, 5),
            ('alpha.beta gamma', 0, 12, '3ge', 0, 4),
            ('alpha.beta gamma', 0, 8, 'gE', 0, 0),
            ('alpha.beta gamma', 0, 12, '2gE', 0, 0),
            ('first\n\n\nsecond', 3, 2, 'ge', 2, 0),
            ('first\n\n\nsecond', 3, 2, '2ge', 1, 0),
            ('one\n   \ntwo', 2, 0, 'ge', 0, 2),
            ('  first', 0, 5, '99ge', 0, 0),
            ('first', 0, 0, 'ge', 0, 0),
            ('a' * 58 + ' bbb ccc', 0, 64, 'ge', 1, 1),
            ('a' * 58 + ' bbb ccc', 0, 64, '2ge', 0, 57),
            ('界 e\u0301 beta gamma', 0, 10, 'ge', 0, 8),
            ('界 e\u0301 beta gamma', 0, 10, '2ge', 0, 3),
        ]:
            with self.subTest(keys=keys, text=text):
                self.fixture(text, row=row, column=column)
                self.keys(keys)
                self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'),
                                 f'{target_column}:{target_row}')
                self.assertEqual(self.buffer(), 'UNCHANGED')

    def test_ge_yanks_counts_and_visual_selections(self):
        for text, row, column, keys, expected in [
            ('alpha.beta gamma', 0, 12, 'yge', 'a ga'),
            ('alpha.beta gamma', 0, 12, 'y2ge', '.beta ga'),
            ('alpha.beta gamma', 0, 12, '2yge', '.beta ga'),
            ('alpha.beta gamma', 0, 12, '2y2ge', 'alpha.beta ga'),
            ('alpha.beta gamma', 0, 8, 'ygE', 'alpha.bet'),
            ('alpha.beta gamma', 0, 12, 'vgey', 'a ga'),
            ('alpha.beta gamma', 0, 12, 'v2gey', '.beta ga'),
            ('alpha.beta gamma', 0, 8, 'vgEy', 'alpha.bet'),
            ('one\n\ntwo', 2, 1, 'Vgey', '\ntwo\n'),
            ('界 e\u0301 beta gamma', 0, 10, 'y2ge', 'e\u0301 beta ga'),
        ]:
            with self.subTest(keys=keys):
                self.fixture(text, row=row, column=column)
                self.keys(keys, expected)
        self.fixture('first')
        self.keys('yge')
        self.assertEqual(self.buffer(), 'UNCHANGED')

    def test_ge_reads_nearby_history_and_keeps_the_viewport(self):
        self.fixture('\n'.join(f'{i:04} alpha.beta gamma' for i in range(1800)), row=1500, column=17)
        self.tm('send-keys', '-X', 'scroll-middle')
        viewport = '#{scroll_position}:#{copy_cursor_y}'
        before = self.tm('display-message', '-p', viewport)
        tmux = copy.Tmux('%0', self.tm('list-clients', '-F', '#{client_name}'))
        tmux.command = self.base
        calls, call = [], tmux.call
        def record(*args, **kwargs):
            calls.append(args)
            return call(*args, **kwargs)
        tmux.call = record
        copy.perform(tmux, {'mode': 'move', 'motion': 'ge', 'before': 1})
        captures = [args for args in calls if args[0] == 'capture-pane']
        self.assertEqual(len(captures), 1)
        capture = captures[0]
        self.assertLessEqual(int(capture[capture.index('-E') + 1]) - int(capture[capture.index('-S') + 1]), 4)
        self.assertEqual(self.tm('display-message', '-p', viewport), before)
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}'), '14')
        self.assertEqual(self.buffer(), 'UNCHANGED')

    def test_local_yanks_capture_bounded_history(self):
        self.fixture('\n'.join(f'{i:04} alpha.beta gamma "quoted text" 界e\u0301' for i in range(1800)),
                     row=1500, column=8)
        tmux = copy.Tmux('%0', self.tm('list-clients', '-F', '#{client_name}'))
        tmux.command = self.base
        calls, call = [], tmux.call
        def record(*args, **kwargs):
            calls.append(args)
            return call(*args, **kwargs)
        tmux.call = record
        for context, expected in [
            ({'motion': 'y'}, '1500 alpha.beta gamma "quoted text" 界e\u0301\n'),
            ({'motion': 'w', 'object': 'i'}, 'alpha'),
            ({'motion': 'q', 'object': 'i'}, 'quoted text'),
        ]:
            with self.subTest(context=context):
                calls.clear()
                copy.perform(tmux, {'mode': 'yank', 'before': 1, **context})
                captures = [args for args in calls if args[0] == 'capture-pane']
                self.assertEqual(len(captures), 1)
                capture = captures[0]
                self.assertLessEqual(int(capture[capture.index('-E') + 1]) - int(capture[capture.index('-S') + 1]), 8)
                self.assertEqual(self.buffer(), expected)
                self.drain()
        self.until(lambda: not self.tm('show-options', '-pqv', '@vim-copy-flash'))

    def test_text_objects_and_visual_objects(self):
        for sequence, expected in [('yi(', 'outer (inner) tail'), ('ya(', '(outer (inner) tail)'),
                                   ('vi(y', 'outer (inner) tail'), ('va(y', '(outer (inner) tail)')]:
            with self.subTest(sequence=sequence):
                self.fixture('(outer (inner) tail)', column=16)
                self.keys(sequence, expected)
        for sequence, expected in [('yiW', 'alpha.beta'), ('yaw', ' alpha'), ('viwy', 'alpha')]:
            with self.subTest(sequence=sequence):
                self.fixture('previous alpha.beta next', column=9)
                self.keys(sequence, expected)

    def test_visual_objects_preserve_scrolled_viewport(self):
        text = '\n'.join(f'{i:03} alpha (inner words) "quoted text" tail' for i in range(100))
        for row, column, sequence, expected in [
            (10, 7, 'viw', 'alpha'),
            (60, 7, 'vaw', 'alpha '),
            (90, 16, 'vib', 'inner words'),
            (60, 29, 'vaq', '"quoted text" '),
            (60, 7, 'vll', 'ha '),
        ]:
            with self.subTest(sequence=sequence, row=row):
                self.fixture(text, row=row, column=column)
                self.tm('send-keys', '-X', 'scroll-middle')
                viewport = '#{scroll_position}:#{copy_cursor_y}'
                before = self.tm('display-message', '-p', viewport)
                self.keys(sequence)
                self.assertEqual(self.tm('display-message', '-p', viewport), before)
                self.assertEqual(self.buffer(), 'UNCHANGED')
                self.keys('y', expected)

    def test_multiline_visual_object_preserves_viewport_until_selection_goes_offscreen(self):
        for last_row in (62, 80):
            with self.subTest(last_row=last_row):
                lines = [f'{i:03} filler' for i in range(100)]
                lines[59] = '(first'
                lines[last_row] = 'last)'
                self.fixture('\n'.join(lines), row=60, column=3)
                self.tm('send-keys', '-X', 'scroll-middle')
                before_y = int(self.tm('display-message', '-p', '#{copy_cursor_y}'))
                before_scroll = int(self.tm('display-message', '-p', '#{scroll_position}'))
                height = int(self.tm('display-message', '-p', '#{pane_height}'))
                scroll = max(0, last_row - 60 + before_y - height + 1)
                self.keys('vib')
                self.assertEqual(int(self.tm('display-message', '-p', '#{scroll_position}')),
                                 before_scroll - scroll)
                self.assertEqual(int(self.tm('display-message', '-p', '#{copy_cursor_y}')),
                                 last_row - 60 + before_y - scroll)
                self.keys('y', 'first\n' + '\n'.join(lines[60:last_row]) + '\nlast')

    def test_any_quote_yanks_and_visual_objects(self):
        for delimiter in '\'"`':
            for sequence, expected in [('yiq', 'quoted text'), ('viqy', 'quoted text'),
                                       ('yaq', delimiter + 'quoted text' + delimiter + ' '),
                                       ('vaqy', delimiter + 'quoted text' + delimiter + ' ')]:
                with self.subTest(delimiter=delimiter, sequence=sequence):
                    self.fixture('before ' + delimiter + 'quoted text' + delimiter + ' after', column=10)
                    self.keys(sequence, expected)
        self.fixture('"outer \'inner\' tail"', column=10)
        self.keys('yiq', 'inner')

    def test_text_object_yanks_move_to_vim_cursor_positions(self):
        # Cursor positions checked against nvim --headless -u NONE.
        for text, row, column, sequence, expected, target_row, target_column in [
            ('alpha beta', 0, 3, 'yiw', 'alpha', 0, 0),
            ('prefix alpha', 0, 10, 'yaw', ' alpha', 0, 6),
            ('(outer (inner) tail)', 0, 16, 'yib', 'outer (inner) tail', 0, 1),
            ('(outer (inner) tail)', 0, 16, 'yab', '(outer (inner) tail)', 0, 0),
            ('(outer (inner) tail)', 0, 10, 'y2ib', 'outer (inner) tail', 0, 1),
            ('[one two]', 0, 6, 'yi[', 'one two', 0, 1),
            ('(first\n  second)', 1, 4, 'yib', 'first\n  second', 0, 1),
            ('{\n  first\n  second\n}', 2, 5, 'yiB', '  first\n  second\n', 1, 0),
            ('  first\n  second\n\nnext', 1, 4, 'yip', '  first\n  second\n', 0, 0),
            ('One sentence.  Another!', 0, 7, 'yis', 'One sentence.', 0, 0),
            ('prefix "quoted text" end', 0, 15, 'yiq', 'quoted text', 0, 8),
            ('<p>\n  text\n</p>', 1, 3, 'yit', '\n  text\n', 0, 2),
        ]:
            with self.subTest(sequence=sequence, text=text):
                self.fixture(text, row=row, column=column)
                self.keys(sequence, expected)
                self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'),
                                 f'{target_column}:{target_row}')
                self.until(lambda: not self.tm('show-options', '-pqv', '@vim-copy-flash'))
                self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'),
                                 f'{target_column}:{target_row}')

    def test_gx_opens_printed_wrapped_and_unicode_urls_without_changing_copy_state(self):
        for text, row, column, expected in [
            ('See [docs](https://example.test/a_(b)).', 0, 24, 'https://example.test/a_(b)'),
            ('See https://example.test/' + 'long-path/' * 15 + ' end', 2, 5,
             'https://example.test/' + 'long-path/' * 15),
            ('界e\u0301 https://example.test/διαδρομή end', 0, 12, 'https://example.test/διαδρομή'),
            ("See https://example.test/a'b;$HOME?q=$(id)#%1 end", 0, 12,
             "https://example.test/a'b;$HOME?q=$(id)#%1"),
        ]:
            with self.subTest(text=text):
                self.fixture(text, row=row, column=column)
                self.tm('send-keys', '-X', 'begin-selection')
                before = self.tm('display-message', '-p', copy.SAVED_SELECTION)
                self.open_link(expected)
                self.assertEqual(self.tm('display-message', '-p', copy.SAVED_SELECTION), before)
                self.assertEqual(self.buffer(), 'UNCHANGED')

    def test_gx_opens_labelled_terminal_hyperlinks(self):
        target = 'https://example.test/#{pane_id}?q=$HOME&x=%1'
        self.fixture('See \033]8;;' + target + '\033\\click here\033]8;;\033\\ end', column=8)
        self.open_link(target)
        self.assertEqual(self.buffer(), 'UNCHANGED')

    def test_gx_with_no_link_preserves_selection_and_does_not_open_a_nearby_url(self):
        self.fixture('plain text https://example.test/', column=2)
        self.tm('send-keys', '-X', 'begin-selection')
        before = self.tm('display-message', '-p', copy.SAVED_SELECTION)
        opened = self.opened_links()
        self.keys('gx')
        self.assertEqual(self.opened_links(), opened)
        self.assertEqual(self.tm('display-message', '-p', copy.SAVED_SELECTION), before)
        self.assertEqual(self.buffer(), 'UNCHANGED')

    def test_o_swaps_visual_ends_without_opening_or_copying(self):
        for keys, row, column, expected, target_row, target_column in [
            ('vll', 0, 2, 'pha', 0, 2),
            ('vhh', 0, 4, 'pha', 0, 4),
            ('Vj', 0, 2, 'alpha beta\ngamma delta\n', 0, None),
            ('\x16jll', 0, 2, 'pha\nmma', 0, 2),
        ]:
            with self.subTest(keys=keys):
                self.fixture('alpha beta\ngamma delta\nlast line', row=row, column=column)
                self.keys(keys)
                cursor = self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}')
                opened = self.opened_links()
                self.keys('o')
                self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_y}'), str(target_row))
                if target_column is not None:
                    self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}'), str(target_column))
                self.assertEqual(self.tm('display-message', '-p', '#{pane_mode}:#{selection_active}'), 'copy-mode:1')
                self.assertEqual(self.buffer(), 'UNCHANGED')
                self.assertEqual(self.opened_links(), opened)
                self.keys('o')
                self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'), cursor)
                self.keys('y', expected)
        self.fixture('alpha beta', column=2)
        self.keys('vlloh')
        self.keys('y', 'lpha')

    def test_o_without_a_visual_selection_or_during_yank_flash_is_harmless(self):
        self.fixture('(https://neovim.io/doc/user/motion/)', column=10)
        before = self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}:#{scroll_position}')
        opened = self.opened_links()
        self.keys('o')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}:#{scroll_position}'), before)
        self.assertEqual(self.tm('display-message', '-p', '#{pane_mode}'), 'copy-mode')
        self.assertEqual(self.opened_links(), opened)
        self.assertEqual(self.buffer(), 'UNCHANGED')
        self.tm('set-option', '-g', '@vim-copy-highlight-ms', '1000')
        try:
            self.fixture('alpha beta', column=2)
            self.keys('yiw', 'alpha')
            self.keys('o')
            self.assertEqual(self.tm('display-message', '-p', '#{pane_mode}:#{selection_active}'), 'copy-mode:0')
            self.assertEqual(self.opened_links(), opened)
            self.assertEqual(self.buffer(), 'alpha')
        finally:
            self.tm('set-option', '-g', '@vim-copy-highlight-ms', '150')

    def test_gv_restores_character_line_block_and_reverse_selections(self):
        for sequence, row, column, expected, mode, rectangle in [
            ('vll', 0, 2, 'pha', 'char', '0'),
            ('vhh', 0, 4, 'pha', 'char', '0'),
            ('Vj', 0, 2, 'alpha beta\ngamma delta\n', 'line', '0'),
            ('Vk', 1, 2, 'alpha beta\ngamma delta\n', 'line', '0'),
            ('\x16jll', 0, 2, 'pha\nmma', 'char', '1'),
            ('v', 0, 2, 'p', 'char', '0'),
            ('viw', 0, 2, 'alpha', 'char', '0'),
        ]:
            with self.subTest(sequence=sequence):
                self.fixture('alpha beta\ngamma delta\nlast line', row=row, column=column)
                self.keys(sequence)
                # Native line mode may represent EOL as either the final
                # character or the following cell. Check its lines and mode;
                # the copied text below verifies the actual selected range.
                signature = (copy.SELECTION_SIGNATURE if mode != 'line' else
                             '#{selection_start_y}:#{selection_end_y}:#{rectangle_toggle}')
                before = self.tm('display-message', '-p', signature)
                cursor = self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}:#{scroll_position}')
                self.keys('y', expected)
                self.until(lambda: not self.tm('show-options', '-pqv', '@vim-copy-flash'))
                self.keys('jgv')
                self.assertEqual(self.tm('display-message', '-p', signature), before)
                self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}:#{scroll_position}'), cursor)
                self.assertEqual(self.tm('display-message', '-p', '#{selection_mode}:#{rectangle_toggle}:#{selection_active}'),
                                 mode + ':' + rectangle + ':1')
                self.tm('set-buffer', '--', 'UNCHANGED')
                self.keys('y', expected)

    def test_gv_after_escape_and_swap_current_visual_selection(self):
        self.fixture('alpha beta gamma', column=2)
        self.keys('viw\x1b')
        self.until(lambda: self.tm('display-message', '-p', '#{selection_active}') == '0')
        self.assertEqual(self.buffer(), 'UNCHANGED')
        self.keys('wviw')
        beta = self.tm('display-message', '-p', copy.SELECTION_SIGNATURE)
        self.keys('gv')
        self.assertEqual(self.tm('display-message', '-p', copy.SELECTION_SIGNATURE), '0:0:4:0:0')
        self.keys('gv')
        self.assertEqual(self.tm('display-message', '-p', copy.SELECTION_SIGNATURE), beta)
        self.keys('y', 'beta')

    def test_gv_block_selection_past_short_line_end(self):
        self.fixture('alpha beta\nx\nlast line', column=4)
        self.keys('\x16jll')
        signature = self.tm('display-message', '-p', copy.SELECTION_SIGNATURE)
        self.keys('y', 'a b\n\n')
        self.keys('gv')
        self.assertEqual(self.tm('display-message', '-p', copy.SELECTION_SIGNATURE), signature)
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'), '6:1')
        self.tm('set-buffer', '--', 'UNCHANGED')
        self.keys('y', 'a b\n\n')

    def test_gv_survives_old_highlight_timer_and_operator_yank(self):
        self.tm('set-option', '-g', '@vim-copy-highlight-ms', '1000')
        try:
            self.fixture('alpha beta')
            self.keys('viwy', 'alpha')
            token = self.tm('show-options', '-pqv', '@vim-copy-flash')
            self.keys('gv')
            selected = self.tm('display-message', '-p', copy.SELECTION_SIGNATURE)
            tmux = copy.Tmux('%0', self.tm('list-clients', '-F', '#{client_name}'))
            tmux.command = self.base
            tmux.finish_flash(token)
            self.assertEqual(self.tm('display-message', '-p', copy.SELECTION_SIGNATURE), selected)
            self.assertEqual(self.tm('display-message', '-p', '#{selection_active}'), '1')
            self.keys('\x1b')
            self.until(lambda: self.tm('display-message', '-p', '#{selection_active}') == '0')
            self.keys('w')
            self.keys('yiw', 'beta')
            self.keys('gv')
            self.assertEqual(self.tm('display-message', '-p', copy.SELECTION_SIGNATURE), selected)
        finally:
            self.tm('set-option', '-g', '@vim-copy-highlight-ms', '150')

    def test_gv_after_copy_mode_reentry_and_new_output(self):
        self.fixture('alpha beta', later='\n' + 'later output\n' * 30)
        self.keys('viwy', 'alpha')
        self.keys('q')
        (self.directory / 'continue').touch()
        self.until(lambda: 'later output' in self.tm('capture-pane', '-p'))
        self.tm('copy-mode')
        self.keys('gv')
        self.keys('y', 'alpha')

    def test_gv_preserves_scrolled_unicode_and_numbered_selections(self):
        self.tm('set-option', '-gw', 'copy-mode-line-numbers', 'absolute')
        try:
            text = '\n'.join(f'{i:03} 界e\u0301 alpha.beta next' for i in range(100))
            self.fixture(text, row=60, column=4)
            self.tm('send-keys', '-X', 'scroll-middle')
            self.keys('vll')
            signature = self.tm('display-message', '-p', copy.SELECTION_SIGNATURE)
            position = self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}:#{scroll_position}')
            self.keys('y', '界e\u0301 ')
            self.keys('jjgv')
            self.assertEqual(self.tm('display-message', '-p', copy.SELECTION_SIGNATURE), signature)
            self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}:#{scroll_position}'), position)
        finally:
            self.tm('set-option', '-gw', 'copy-mode-line-numbers', 'hybrid')

    def test_gv_handles_trimmed_history_and_rejects_cleared_history(self):
        self.tm('set-option', '-g', 'history-limit', '30')
        try:
            text = '\n'.join(f'{i:03} alpha beta' for i in range(35))
            self.fixture(text, row=25, column=4, later='\n' + 'new output\n' * 20)
            self.keys('viwy', 'alpha')
            frame = self.tm('show-options', '-pqv', '@vim-copy-frame')
            self.keys('q')
            (self.directory / 'continue').touch()
            self.until(lambda: 'new output' in self.tm('capture-pane', '-p'))
            self.tm('copy-mode')
            self.assertNotEqual(self.tm('show-options', '-pqv', '@vim-copy-frame'), frame)
            self.keys('gv')
            self.tm('set-buffer', '--', 'UNCHANGED')
            self.keys('y', 'alpha')
            self.keys('q')
            self.tm('clear-history')
            self.tm('copy-mode')
            self.tm('set-buffer', '--', 'UNCHANGED')
            self.keys('gv')
            self.assertEqual(self.buffer(), 'UNCHANGED')
            self.assertEqual(self.tm('display-message', '-p', '#{selection_active}'), '0')
        finally:
            self.tm('set-option', '-g', 'history-limit', '2000')

    def test_sentence_movements_counts_visual_selection_and_yanks(self):
        text = 'One sentence.  Another! Last?\nnext paragraph\n\nFinal.'
        for sequence, column in [(')', 15), ('2)', 24), (')(', 0), ('))(', 15)]:
            with self.subTest(sequence=sequence):
                self.fixture(text)
                self.keys(sequence)
                self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'), str(column) + ':0')
                self.assertEqual(self.buffer(), 'UNCHANGED')
        for sequence, expected in [('y)', 'One sentence.  '),
                                   ('y2)', 'One sentence.  Another! '),
                                   ('2y)', 'One sentence.  Another! '),
                                   ('v)y', 'One sentence.  A'),
                                   ('V)y', 'One sentence.  Another! Last?\nnext paragraph\n')]:
            with self.subTest(sequence=sequence):
                self.fixture(text)
                self.keys(sequence, expected)
        self.fixture('a' * 58 + '. Another. Tail')
        self.keys(')')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'), '0:1')
        self.keys('y)', 'Another. ')
        self.fixture(text, column=20)
        self.keys('y(', 'Anoth')
        self.fixture(text, column=24)
        self.keys('2(')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'), '0:0')
        self.fixture(text)
        self.keys('4)')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'), '0:2')

    def test_plain_visual_line_yank_and_read_preserve_selection(self):
        for text, row, column, expected in [
            ('alpha beta\ngamma delta', 0, 2, 'alpha beta\n'),
            ('alpha beta\ngamma delta', 1, 2, 'gamma delta\n'),
            ('first\n\nlast', 1, 0, '\n'),
            ('界e\u0301 alpha\nnext', 0, 1, '界e\u0301 alpha\n'),
            ('a' * 59 + ' wrapped text\nnext', 1, 3, 'a' * 59 + ' wrapped text\n'),
        ]:
            with self.subTest(row=row, text=text[:15]):
                self.fixture(text, row=row, column=column)
                self.keys('V')
                signature = copy.SELECTION_SIGNATURE + ':#{selection_mode}:#{selection_active}:#{copy_cursor_x}:#{copy_cursor_y}:#{scroll_position}'
                before = self.tm('display-message', '-p', signature)
                tmux = copy.Tmux('%0', self.tm('list-clients', '-F', '#{client_name}'))
                tmux.command = self.base
                tmux.snapshot(tmux.state())
                self.assertEqual(self.tm('display-message', '-p', signature), before)
                self.assertEqual(self.buffer(), 'UNCHANGED')
                self.keys('y', expected)
        self.fixture('one line\nnext line', column=2)
        self.keys('Vy', 'one line\n')

    def test_sentence_cache_uses_native_commands_for_rapid_repeats(self):
        fcntl.ioctl(self.master, termios.TIOCSWINSZ, struct.pack('HHHH', 55, 190, 0, 0))
        os.kill(self.client.pid, signal.SIGWINCH)
        try:
            self.until(lambda: self.tm('display-message', '-p', '#{pane_width}x#{pane_height}') == '190x55')
            self.fixture('\n'.join(f'{i:04} One sentence. Another sentence! Last sentence?'
                                   for i in range(1800)), row=1500, column=8)
            self.tm('send-keys', '-X', 'scroll-middle')
            self.until(lambda: bool(self.tm('show-options', '-pqv', '@vim-copy-sentence-frame')))
            self.tm('display-message', '-p', 'native-motion-benchmark-start')
            timings = []
            for key, column in [(')', 19), (')', 37), ('(', 19), ('(', 0)] * 4:
                started = time.monotonic()
                os.write(self.master, key.encode())
                while self.tm('display-message', '-p', '#{copy_cursor_x}') != str(column):
                    self.assertLess(time.monotonic() - started, 1)
                    self.drain()
                    time.sleep(.001)
                self.drain()
                timings.append(time.monotonic() - started)
            self.assertLess(sorted(timings)[len(timings) // 2], .025,
                            'Cached sentence movements should respond like native tmux keys')
            messages = self.tm('show-messages').split('native-motion-benchmark-start', 1)[0]
            executions = [line for line in messages.splitlines() if ' execute ' in line and
                          ('key (: run-shell ' in line or 'key ): run-shell ' in line)]
            self.assertEqual(executions, [])
            self.assertEqual(self.buffer(), 'UNCHANGED')
        finally:
            fcntl.ioctl(self.master, termios.TIOCSWINSZ, struct.pack('HHHH', 12, 60, 0, 0))
            os.kill(self.client.pid, signal.SIGWINCH)
            self.until(lambda: self.tm('display-message', '-p', '#{pane_width}x#{pane_height}') == '60x12')

    def test_sentence_cache_handles_unicode_rows_and_refresh(self):
        self.fixture('界e\u0301 One. Two!\n  Three. Four?\n\nlast')
        self.keys(')')
        self.keys(')')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'), '2:1')
        self.keys('(')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'), '9:0')
        self.keys('r')
        self.assertEqual(self.tm('show-options', '-pqv', '@vim-copy-sentence-frame'), '')
        self.keys('(')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'), '0:0')

    def test_sentence_cache_miss_reads_nearby_rows_without_selection_commands(self):
        self.fixture('\n'.join(f'{i:04} One. Two! Three?' for i in range(1800)), row=1500, column=5)
        tmux = copy.Tmux('%0', self.tm('list-clients', '-F', '#{client_name}'))
        tmux.command = self.base
        calls, call = [], tmux.call
        def record(*args, **kwargs):
            calls.append(args)
            return call(*args, **kwargs)
        tmux.call = record
        copy.perform(tmux, {'mode': 'move', 'motion': ')', 'before': 1})
        captures = [args for args in calls if args[0] == 'capture-pane']
        self.assertEqual(len(captures), 1)
        capture = captures[0]
        self.assertLessEqual(int(capture[capture.index('-E') + 1]) - int(capture[capture.index('-S') + 1]), 64)
        self.assertFalse(any('clear-selection' in args or 'begin-selection' in args or 'copy-pipe' in args for args in calls))

    def test_vim_paragraph_movements_counts_macros_and_whitespace(self):
        text = '  one\n   \ntwo\n\n\nthree\n.IP list\nitem\n.SH title\nsection'
        for row, column, sequence, target_row, target_column in [
            (0, 2, '}', 3, 0), (0, 2, '2}', 6, 0),
            (0, 2, '}}', 6, 0), (3, 0, '}', 6, 0),
            (4, 0, '}', 6, 0), (5, 3, '{', 4, 0),
            (6, 3, '{', 4, 0), (7, 2, '{', 6, 0),
            (9, 3, '2{', 6, 0), (3, 0, '{', 0, 0),
            (0, 3, '{', 0, 0),
        ]:
            with self.subTest(sequence=sequence, row=row):
                self.fixture(text, row=row, column=column)
                self.keys(sequence)
                top = int(self.tm('display-message', '-p', '#{copy_position}')) - 1
                self.assertEqual(top + int(self.tm('display-message', '-p', '#{copy_cursor_y}')), target_row)
                self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}'), str(target_column))
                self.assertEqual(self.buffer(), 'UNCHANGED')
        self.fixture('\n'.join(['first'] * 11 + ['界e\u0301']), column=2)
        self.keys('}')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'), '2:11')

    def test_paragraph_motions_preserve_visual_and_yank_rules(self):
        text = '  one\n   \ntwo\n\n\nthree\n.IP list\nitem\nnext'
        for sequence, expected in [
            ('y}', '  one\n   \ntwo\n'),
            ('y2}', '  one\n   \ntwo\n\n\nthree\n'),
            ('2y}', '  one\n   \ntwo\n\n\nthree\n'),
            ('v}y', 'one\n   \ntwo\n\n'),
            ('V}y', '  one\n   \ntwo\n\n'),
        ]:
            with self.subTest(sequence=sequence):
                self.fixture(text, column=2)
                self.keys(sequence, expected)
        self.fixture(text, row=7, column=2)
        self.keys('y{', '.IP list\nit')
        self.fixture('a' * 62 + '.IP not a macro\ntail\n\nnext')
        self.tm('send-keys', '-X', '-N', '62', 'cursor-right')
        self.keys('}')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'), '0:3')

    def test_relative_number_gutter_renders_and_never_enters_yanks(self):
        self.assertEqual(self.tm('show-options', '-gwv', 'copy-mode-line-numbers'), 'hybrid')
        self.fixture('\n'.join(f'ROW{i:03}' for i in range(30)), row=13)
        self.tm('send-keys', '-X', 'scroll-middle')
        self.drain()
        self.terminal_output.clear()
        self.tm('refresh-client')
        def screen():
            self.drain()
            return re.sub(rb'\x1b(?:\[[0-?]*[ -/]*[@-~]|\([A-Za-z0-9])', b'', bytes(self.terminal_output))
        self.until(lambda: b' 14 ROW013' in screen())
        self.assertIn(b'  1 ROW012', screen())
        self.assertIn(b'  1 ROW014', screen())
        self.assertIn(b'  3 ROW010', screen())
        self.assertEqual(self.tm('display-message', '-p', '#{copy_line_numbers}'), '1')
        self.keys('3j')
        self.keys('Vy', 'ROW016\n')
        self.keys('gv')
        self.keys('y', 'ROW016\n')
        self.keys('q')
        self.assertEqual(self.tm('display-message', '-p', '#{pane_mode}'), '')

    def test_hidden_gutter_keeps_copy_coordinates_correct(self):
        self.fixture('\n'.join(f'{i:03} alpha beta' for i in range(100)), row=60, column=4)
        self.tm('send-keys', '-X', 'line-numbers-off')
        before = self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}:#{scroll_position}')
        self.keys('yiw', 'alpha')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}:#{scroll_position}'), before)

    def test_cached_paragraph_jump_skips_intermediate_scrollback_rows(self):
        lines = [f'row {i:03}' if i not in (20, 60, 100) else '' for i in range(150)]
        self.fixture('\n'.join(lines), row=25, column=3)
        tmux = copy.Tmux('%0', self.tm('list-clients', '-F', '#{client_name}'))
        tmux.command = self.base
        state = tmux.state()
        document = tmux.snapshot(state)
        tmux.commands(tmux.sentence_cache(state, document))
        self.tm('display-message', '-p', 'native-paragraph-jump-start')
        self.keys('}')
        top = int(self.tm('display-message', '-p', '#{copy_position}')) - 1
        self.assertEqual(top + int(self.tm('display-message', '-p', '#{copy_cursor_y}')), 60)
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}'), '0')
        messages = self.tm('show-messages').split('native-paragraph-jump-start', 1)[0]
        self.assertIn('send-keys -X goto-line 61', messages)
        self.assertNotIn('key }: run-shell "python3 ', messages)
        self.assertEqual(self.buffer(), 'UNCHANGED')

    def test_cached_motion_compares_line_and_column_numbers_numerically(self):
        self.fixture('\n'.join('One. Two.' for _ in range(40)), row=9)
        self.until(lambda: bool(self.tm('show-options', '-pqv', '@vim-copy-sentence-frame')))
        self.keys('))')
        top = int(self.tm('display-message', '-p', '#{copy_position}')) - 1
        self.assertEqual(top + int(self.tm('display-message', '-p', '#{copy_cursor_y}')), 10)
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}'), '0')
        self.keys('((')
        top = int(self.tm('display-message', '-p', '#{copy_position}')) - 1
        self.assertEqual(top + int(self.tm('display-message', '-p', '#{copy_cursor_y}')), 9)
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}'), '0')

    def test_gg_remains_available(self):
        self.fixture('\n'.join('line ' + str(i) for i in range(30)), row=20)
        self.keys('gg')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_line}'), 'line 0')

    def test_yank_flash_renders_copied_range_then_expires(self):
        self.tm('set-option', '-g', '@vim-copy-highlight-ms', '500')
        try:
            self.fixture('alpha beta')
            self.keys('yiw', 'alpha')
            self.assertEqual(self.tm('display-message', '-p', copy.SELECTION_SIGNATURE), '0:0:4:0:0')
            self.assertEqual(self.tm('display-message', '-p', '#{selection_active},#{copy_cursor_x}'), '0,0')
            # Selection is a terminal overlay, so capture-pane does not
            # include its colors. Verify the actual rendered terminal output.
            self.until(lambda: b'48;2;49;49;49m' in self.terminal_output)  # Neovim's Visual background.
            self.until(lambda: not self.tm('show-options', '-pqv', '@vim-copy-flash'))
            self.assertEqual(self.tm('display-message', '-p', '#{selection_present},#{selection_active},#{copy_cursor_x}'), '0,0,0')
            self.assertEqual(self.buffer(), 'alpha')
        finally:
            self.tm('set-option', '-g', '@vim-copy-highlight-ms', '150')

    def test_padded_line_yank_highlights_only_the_copied_line(self):
        self.tm('set-option', '-g', '@vim-copy-highlight-ms', '800')
        try:
            header = "• Ran python3 - <<'PY' …"
            result = '  └ Loaded Vim paragraph bindings and relative line numbers.'
            for sequence in ['yy', 'Vy']:
                with self.subTest(sequence=sequence):
                    self.fixture(header.ljust(60) + '\n' + result)
                    self.keys(sequence, header + '\n')
                    self.assertEqual(self.tm('display-message', '-p', '#{selection_start_y}:#{selection_end_y}'), '0:0')
                    self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_y}'), '0')
        finally:
            self.tm('set-option', '-g', '@vim-copy-highlight-ms', '150')

    def test_counted_padded_line_yank_keeps_range_and_cursor(self):
        self.tm('set-option', '-g', '@vim-copy-highlight-ms', '800')
        try:
            for sequence in ['2yy', 'y2y', 'Vjy']:
                with self.subTest(sequence=sequence):
                    self.fixture('  first'.ljust(60) + '\n' + '  界e\u0301 second'.ljust(59) + '\nleave this line alone')
                    self.keys(sequence, '  first\n  界e\u0301 second\n')
                    self.assertEqual(self.tm('display-message', '-p', '#{selection_start_y}:#{selection_end_y}'), '0:1')
            self.fixture('top\n' + '  padded'.ljust(60) + '\nnext', row=1, column=4)
            self.keys('yy', '  padded\n')
            self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'), '4:1')
        finally:
            self.tm('set-option', '-g', '@vim-copy-highlight-ms', '150')

    def test_wrapped_line_yank_trims_only_the_final_padding(self):
        self.tm('set-option', '-g', '@vim-copy-highlight-ms', '800')
        try:
            line = '  ' + 'a' * 65 + ' wrapped content'
            self.fixture(line.ljust(120) + '\nnext hard line')
            self.keys('yy', line + '\n')
            self.assertEqual(self.tm('display-message', '-p', '#{selection_start_y}:#{selection_end_y}'), '0:1')
            self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}:#{copy_cursor_y}'), '0:0')
        finally:
            self.tm('set-option', '-g', '@vim-copy-highlight-ms', '150')

    def test_yank_flash_restores_cursor_in_scrolled_history(self):
        text = '\n'.join(f'{i:03} alpha "quoted text" omega' for i in range(100))
        location = '#{copy_cursor_x}:#{copy_cursor_y}:#{scroll_position}:#{copy_cursor_line}'
        for row, column, sequence, expected, target_column in [
            (10, 7, 'yiw', 'alpha', 4),
            (25, 7, 'yiw', 'alpha', 4),
            (90, 7, 'yiw', 'alpha', 4),
            (60, 18, 'yiq', 'quoted text', 11),
            (25, 4, 'yw', 'alpha ', 4),
        ]:
            with self.subTest(row=row, sequence=sequence):
                self.fixture(text, row=row, column=column)
                self.tm('send-keys', '-X', 'scroll-middle')
                before = self.tm('display-message', '-p', location).split(':', 1)[1]
                self.keys(sequence, expected)
                self.assertEqual(self.tm('display-message', '-p', location), str(target_column) + ':' + before)
                self.until(lambda: not self.tm('show-options', '-pqv', '@vim-copy-flash'))
                self.assertEqual(self.tm('display-message', '-p', location), str(target_column) + ':' + before)

    def test_flash_duration_starts_with_visible_paint_in_large_history(self):
        # Match the real terminal geometry. Traversing history used to flood
        # tmux's tty queue, delaying the flash until most of its timer elapsed.
        fcntl.ioctl(self.master, termios.TIOCSWINSZ, struct.pack('HHHH', 55, 190, 0, 0))
        os.kill(self.client.pid, signal.SIGWINCH)
        try:
            self.until(lambda: self.tm('display-message', '-p', '#{pane_width}x#{pane_height}') == '190x55')
            self.tm('set-option', '-g', '@vim-copy-highlight-style', 'bg=#123456')
            text = '\n'.join(f'{i:04} ' + ('TARGET' if i == 1500 else 'filler') +
                             ' beta gamma ' + 'abcdefgh ijklmnop ' * 8 for i in range(1800))
            self.fixture(text, row=1500, column=8)
            self.tm('send-keys', '-X', 'scroll-middle')
            self.drain()
            self.terminal_output.clear()
            for index, char in enumerate('yiw'):
                os.write(self.master, char.encode())
                if index < 2:
                    time.sleep(.025)
            deadline = time.monotonic() + 3
            while b'48;2;18;52;86m' not in self.terminal_output:
                self.drain()
                self.assertLess(time.monotonic(), deadline, 'The highlight was not painted')
                time.sleep(.001)
            painted_at = time.monotonic()
            self.until(lambda: not self.tm('show-options', '-pqv', '@vim-copy-flash'))
            self.assertGreaterEqual(time.monotonic() - painted_at, .130,
                                    'The 150 ms flash expired before it was visibly displayed')
            self.assertEqual(self.buffer(), 'TARGET')
        finally:
            self.tm('set-option', '-g', '@vim-copy-highlight-style', 'bg=#313131')
            fcntl.ioctl(self.master, termios.TIOCSWINSZ, struct.pack('HHHH', 12, 60, 0, 0))
            os.kill(self.client.pid, signal.SIGWINCH)
            self.until(lambda: self.tm('display-message', '-p', '#{pane_width}x#{pane_height}') == '60x12')

    def test_visual_yank_flashes_and_new_selection_survives_timer(self):
        self.tm('set-option', '-g', '@vim-copy-highlight-ms', '800')
        try:
            self.fixture('alpha beta')
            self.keys('viwy', 'alpha')
            self.assertTrue(self.tm('show-options', '-pqv', '@vim-copy-flash'))
            self.keys('vll')
            selection = self.tm('display-message', '-p', copy.SELECTION_SIGNATURE)
            self.assertEqual(self.tm('display-message', '-p', '#{selection_active}'), '1')
            self.until(lambda: not self.tm('show-options', '-pqv', '@vim-copy-flash'))
            self.assertEqual(self.tm('display-message', '-p', copy.SELECTION_SIGNATURE), selection)
            self.assertEqual(self.tm('display-message', '-p', '#{selection_active}'), '1')
        finally:
            self.tm('set-option', '-g', '@vim-copy-highlight-ms', '150')

    def test_repeated_yanks_ignore_old_flash_timers(self):
        self.tm('set-option', '-g', '@vim-copy-highlight-ms', '1000')
        try:
            self.fixture('alpha beta')
            self.keys('yw', 'alpha ')
            old = self.tm('show-options', '-pqv', '@vim-copy-flash')
            # Move without waiting for the first flash to expire.
            os.write(self.master, b'w')
            time.sleep(.025)
            self.keys('yw', 'beta')
            new = self.tm('show-options', '-pqv', '@vim-copy-flash')
            self.assertNotEqual(old, new)
            tmux = copy.Tmux('%0', self.tm('list-clients', '-F', '#{client_name}'))
            tmux.command = self.base
            tmux.finish_flash(old)
            self.assertEqual(self.tm('show-options', '-pqv', '@vim-copy-flash'), new)
            self.assertEqual(self.buffer(), 'beta')
        finally:
            self.tm('set-option', '-g', '@vim-copy-highlight-ms', '150')

    def test_multiline_tags_and_wrapped_text(self):
        self.fixture('<div>\n <p>first <b>bold</b> tail</p>\n</div>', row=1, column=26)
        self.keys('yit', 'first <b>bold</b> tail')
        self.fixture('a' * 59 + '(wrapped contents) tail', row=1, column=5)
        self.keys('yib', 'wrapped contents')

    def test_cancel_and_failed_motion_preserve_buffer_and_position(self):
        self.fixture('alpha beta gamma', column=3)
        before = self.tm('display-message', '-p', '#{copy_cursor_x},#{copy_cursor_y},#{scroll_position}')
        self.keys('yt!')
        self.assertEqual(self.tm('show-buffer'), 'UNCHANGED')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x},#{copy_cursor_y},#{scroll_position}'), before)
        self.keys('yf\x1b')
        time.sleep(.6)
        self.assertEqual(self.tm('show-buffer'), 'UNCHANGED')
        self.keys('yw', 'ha ')

    def test_native_counts_still_work(self):
        self.fixture('alpha beta gamma\nsecond\nthird\nfourth', column=10)
        self.keys('3h')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x}'), '7')
        self.keys('2j')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_y}'), '2')

    def test_backward_and_line_boundary_yanks(self):
        for sequence, expected in [('y0', 'one '), ('yb', 'one '), ('yh', ' '),
                                   ('yl', 't'), ('y$', 'two, three')]:
            with self.subTest(sequence=sequence):
                self.fixture('one two, three', column=4)
                self.keys(sequence, expected)
        for sequence, expected in [('yF,', ', three'), ('yT,', ' three')]:
            with self.subTest(sequence=sequence):
                self.fixture('one two, three', column=13)
                self.keys(sequence, expected)

    def test_history_yanks_and_absolute_counts(self):
        text = '\n'.join('line ' + str(i) for i in range(1, 21))
        for sequence, expected in [('ygg', 'line 1\nline 2\nline 3\n'),
                                   ('y5G', 'line 3\nline 4\nline 5\n'),
                                   ('1yG', 'line 1\nline 2\nline 3\n')]:
            with self.subTest(sequence=sequence):
                self.fixture(text, row=2)
                self.keys(sequence, expected)
        self.fixture(text, row=17)
        self.keys('yG', 'line 18\nline 19\nline 20\n')

    def test_search_yanks_and_repeat_search(self):
        self.fixture('start needle middle needle end')
        self.keys('y/needle\r', 'start ')
        self.fixture('start needle middle needle end', column=20)
        self.keys('y?needle\r', 'needle middle ')
        self.fixture('start needle middle needle end')
        self.tm('send-keys', '-X', 'search-forward', '--', 'needle')
        self.tm('send-keys', '-X', 'start-of-line')
        self.keys('yn', 'start ')
        self.fixture('start needle middle needle end')
        self.keys('y2/needle\r', 'start needle middle ')

    def test_sentence_paragraph_and_visual_line_objects(self):
        self.fixture('One sentence.  Another! Last?', column=5)
        self.keys('yas', 'One sentence.  ')
        self.fixture('first\nparagraph\n\nsecond\nlast', row=1, column=3)
        self.keys('yap', 'first\nparagraph\n\n')
        self.fixture('first\nparagraph\n\nsecond\nlast', row=1, column=3)
        self.keys('vipy', 'first\nparagraph\n')

    def test_quote_characters_are_data_not_shell_code(self):
        for target in ['"', "'", '`', '$', '#', '\\', ';', ' ']:
            with self.subTest(target=target):
                self.fixture('alpha' + target + 'omega')
                self.keys('yf' + target, 'alpha' + target)
        self.fixture('alpha "one \\" two" omega', column=12)
        self.keys('yi"', 'one \\" two')

    def test_frozen_snapshot_survives_new_pane_output(self):
        self.fixture('old copied text, rest', later='\n' + 'new live output\n' * 30)
        (self.directory / 'continue').touch()
        deadline = time.monotonic() + 2
        while 'new live output' not in self.tm('capture-pane', '-p'):
            self.assertLess(time.monotonic(), deadline)
            time.sleep(.02)
        self.keys('yt,', 'old copied text')

    def test_failed_visual_object_preserves_selection(self):
        self.fixture('alpha beta', column=2)
        self.keys('vll')
        before = self.tm('display-message', '-p', '#{selection_start_x},#{selection_start_y},#{selection_end_x},#{selection_end_y},#{copy_cursor_x},#{copy_cursor_y}')
        self.keys('i(')
        self.assertEqual(self.buffer(), 'UNCHANGED')
        self.assertEqual(self.tm('display-message', '-p', '#{selection_start_x},#{selection_start_y},#{selection_end_x},#{selection_end_y},#{copy_cursor_x},#{copy_cursor_y}'), before)

    def test_unicode_and_numbered_copy_mode(self):
        self.fixture('界e\u0301 alpha.beta next')
        self.keys('yf.', '界e\u0301 alpha.')
        self.fixture('界e\u0301 alpha.beta next')
        self.keys('viWy', '界e\u0301')
        self.fixture('界e\u0301 alpha.beta next', column=1)
        before = self.tm('display-message', '-p', '#{copy_cursor_x},#{copy_cursor_y}')
        self.keys('yf!')
        self.assertEqual(self.tm('display-message', '-p', '#{copy_cursor_x},#{copy_cursor_y}'), before)
        self.tm('set-option', '-gw', 'copy-mode-line-numbers', 'absolute')
        try:
            self.fixture('\n'.join('line ' + str(i) for i in range(1, 21)), row=10)
            self.keys('yy', 'line 11\n')
        finally:
            self.tm('set-option', '-gw', 'copy-mode-line-numbers', 'hybrid')


if __name__ == '__main__':
    unittest.main()
