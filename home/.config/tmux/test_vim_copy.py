"""Range tests and real terminal input against a separate tmux server."""

import importlib.util
import os
from pathlib import Path
import pty
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

    def test_linewise_motions_and_counts(self):
        text = 'one\ntwo\nthree\nfour'
        self.yank(text, 5, 'y', 'two\nthree\n', 2)
        self.yank(text, 5, 'j', 'two\nthree\n')
        self.yank(text, 5, 'k', 'one\ntwo\n')
        self.yank(text, 5, 'G', 'two\nthree\nfour\n')
        self.yank(text, 5, 'G', 'two\nthree\n', 3, explicit=True)
        self.yank(text, 5, 'gg', 'one\ntwo\n')
        self.yank(text, 5, '$', 'wo\nthree', 2)

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
        centering = centering.read_text().split('# ALWAYS CENTERED', 1)[1]
        config.write_text('set -g mode-keys vi\nset -g status off\nset -g escape-time 0\nset -g set-clipboard off\n'
                          'set -as terminal-features ",xterm-256color:RGB"\n'
                          'bind -T copy-mode-vi v send -X begin-selection\n' + bindings + centering)
        cls.tm('-f', str(config), 'new-session', '-d', '-x', '60', '-y', '12', '-s', 'test', 'sleep 1000')
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
        cls.client.wait(timeout=3)
        os.close(cls.master)
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

    def test_yank_flash_restores_cursor_in_scrolled_history(self):
        text = '\n'.join(f'{i:03} alpha "quoted text" omega' for i in range(100))
        location = '#{copy_cursor_x}:#{copy_cursor_y}:#{scroll_position}:#{copy_cursor_line}'
        for row, column, sequence, expected in [
            (10, 7, 'yiw', 'alpha'),
            (25, 7, 'yiw', 'alpha'),
            (90, 7, 'yiw', 'alpha'),
            (60, 18, 'yiq', 'quoted text'),
            (25, 4, 'yw', 'alpha '),
        ]:
            with self.subTest(row=row, sequence=sequence):
                self.fixture(text, row=row, column=column)
                self.tm('send-keys', '-X', 'scroll-middle')
                before = self.tm('display-message', '-p', location)
                self.keys(sequence, expected)
                self.assertEqual(self.tm('display-message', '-p', location), before)
                self.until(lambda: not self.tm('show-options', '-pqv', '@vim-copy-flash'))
                self.assertEqual(self.tm('display-message', '-p', location), before)

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
            self.keys('yiw', 'TARGET')
            self.until(lambda: b'48;2;18;52;86m' in self.terminal_output)
            painted_at = time.monotonic()
            self.until(lambda: not self.tm('show-options', '-pqv', '@vim-copy-flash'))
            self.assertGreaterEqual(time.monotonic() - painted_at, .130,
                                    'The 150 ms flash expired before it was visibly displayed')
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
            self.tm('set-option', '-gw', 'copy-mode-line-numbers', 'off')


if __name__ == '__main__':
    unittest.main()
