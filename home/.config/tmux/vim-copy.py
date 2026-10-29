#!/usr/bin/env python3
"""Vim yank motions and text objects for tmux 3.8, using only Python's stdlib.

Runtime never starts an editor. The frozen copy-mode buffer is exported through
copy-pipe -CP, so reading it changes neither paste buffers nor the clipboard.
Only a successful yank writes a buffer. Esc cancels any pending input.
The q object matches any quote type; successful yanks briefly highlight their
range. Configure @vim-copy-highlight-ms and @vim-copy-highlight-style in tmux.

Run `bindings` to emit the tmux bindings, or `python3 -m unittest discover
-s home/.config/tmux -p 'test_vim_copy.py'` from the dotfiles checkout to test.
"""

from bisect import bisect_left, bisect_right
from dataclasses import dataclass
from functools import cached_property, lru_cache
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys
import tempfile
import unicodedata
import uuid


MAX_COUNT = 100000
SELECTION_SIGNATURE = ':'.join('#{' + name + '}' for name in (
    'selection_start_x', 'selection_start_y', 'selection_end_x',
    'selection_end_y', 'rectangle_toggle'))
FLASH_SELECTION = ('#{&&:#{@vim-copy-flash},#{&&:#{==:#{selection_active},0},'
                   '#{==:' + SELECTION_SIGNATURE + ',#{@vim-copy-flash-selection}}}}')


class NoMotion(ValueError):
    """A failed motion must leave the selection and clipboard alone."""


@dataclass(frozen=True)
class Span:
    start: int
    end: int  # Exclusive; linewise spans include their last newline.
    linewise: bool = False
    cursor: int | None = None


@lru_cache(maxsize=4096)
def cell_width(char):
    if unicodedata.combining(char) or unicodedata.category(char) in ('Mn', 'Me', 'Cf'):
        return 0
    return 2 if unicodedata.east_asian_width(char) in ('W', 'F') else 1


class Document:
    def __init__(self, text):
        self.text = text if text.endswith('\n') else text + '\n'
        self.lines = self.text.split('\n')[:-1]
        self.starts = [0]
        for line in self.lines:
            self.starts.append(self.starts[-1] + len(line) + 1)

    def line(self, position):
        return min(len(self.lines) - 1, bisect_right(self.starts, position) - 1)

    def end(self, line):
        return self.starts[line] + len(self.lines[line])

    def indentation(self, line):
        return self.starts[line] + len(self.lines[line]) - len(self.lines[line].lstrip(' \t'))

    def line_span(self, first, last, cursor=None):
        first, last = sorted((first, last))
        return Span(self.starts[first], self.starts[last + 1], True, cursor)

    @staticmethod
    def kind(char, big=False):
        if char.isspace():
            return 0
        if big:
            return 1
        return 1 if char.isalnum() or char == '_' or unicodedata.combining(char) else 2

    def units(self, big=False, empty=True):
        cache = '_units_' + str((big, empty))
        if cache not in self.__dict__:
            if self.text.isascii():
                pattern = r'\S+' if big else r'[A-Za-z0-9_]+|[^A-Za-z0-9_\s]+'
                if empty:
                    pattern += r'|(?m:^\n)'
                self.__dict__[cache] = [(match.start(), match.end()) for match in re.finditer(pattern, self.text)]
                return self.__dict__[cache]
            result = []
            for row, line in enumerate(self.lines):
                if not line and empty:
                    result.append((self.starts[row], self.starts[row] + 1))
                i = 0
                while i < len(line):
                    kind = self.kind(line[i], big)
                    end = i + 1
                    while end < len(line) and self.kind(line[end], big) == kind:
                        end += 1
                    if kind:
                        result.append((self.starts[row] + i, self.starts[row] + end))
                    i = end
            self.__dict__[cache] = result
        return self.__dict__[cache]

    def exclusive(self, origin, target):
        """Vim's exclusive motion ending in column zero excludes that line."""
        start, end = sorted((origin, target))
        first, last = self.line(start), self.line(end)
        if last > first and end == self.starts[last]:
            if start <= self.indentation(first):
                return self.line_span(first, last - 1)
            end -= 1
        return Span(start, end)

    def motion(self, position, key, count=1, argument=None, explicit=False):
        text, row = self.text, self.line(position)
        start, end = self.starts[row], self.end(row)
        if key == 'y':
            return self.line_span(row, min(len(self.lines) - 1, row + count - 1), position)
        if key in ('j', 'k', 'G', 'gg'):
            if key in ('G', 'gg'):
                target = min(len(self.lines) - 1, count - 1) if explicit else (
                    len(self.lines) - 1 if key == 'G' else 0)
            else:
                target = max(0, min(len(self.lines) - 1, row + (count if key == 'j' else -count)))
            if target == row and key in ('j', 'k'):
                raise NoMotion('No more lines')
            return self.line_span(row, target)
        if key == '0':
            return Span(start, position)
        if key == '^':
            return self.exclusive(position, self.indentation(row))
        if key == '$':
            return Span(position, self.end(min(len(self.lines) - 1, row + count - 1)))
        if key == 'h':
            target = position
            for _ in range(min(count, position - start)):
                target -= 1
                while target > start and cell_width(text[target]) == 0:
                    target -= 1
                if target == start:
                    break
            return Span(target, position)
        if key == 'l':
            target = position
            for _ in range(min(count, end - position)):
                target += 1
                while target < end and cell_width(text[target]) == 0:
                    target += 1
                if target == end:
                    break
            return Span(position, target)
        if key in 'wWbBeE':
            units = self.units(key.isupper(), empty=key.lower() != 'e')
            starts = [a for a, b in units]
            ends = [b - 1 for a, b in units]
            if not units:
                raise NoMotion('No words')
            if key.lower() == 'b':
                index = max(0, bisect_left(starts, position) - count)
                return self.exclusive(position, starts[index])
            if key.lower() == 'e':
                index = min(len(units) - 1, bisect_right(ends, position) + count - 1)
                return Span(position, max(position, units[index][1]))
            index = bisect_right(starts, position) + count - 1
            target = starts[index] if index < len(starts) else len(text) - 1
            previous = position if count == 1 else starts[min(len(starts) - 1, index - 1)]
            # An operator followed by w stops at the end of its final line,
            # even when the next word is indented on a subsequent line.
            previous_row = self.line(previous)
            if self.line(target) > previous_row and not text[previous:self.end(previous_row)].isspace():
                target = self.end(previous_row)
            if index >= len(starts):
                target = max(position, self.end(len(self.lines) - 1))
            return self.exclusive(position, target)
        if key in 'fFtT':
            if not argument or len(argument) != 1:
                raise NoMotion('Expected one character')
            target = position
            for _ in range(count):
                target = text.find(argument, target + 1, end) if key in 'ft' else text.rfind(argument, start, target)
                if target < 0:
                    raise NoMotion('Character not found')
            if key == 'f':
                return Span(position, target + 1)
            if key == 't':
                return Span(position, target)
            return Span(target + (key == 'T'), position + 1)
        if key == '%':
            match = re.search(r'[()\[\]{}]', text[position:end])
            if not match:
                raise NoMotion('No bracket on this line')
            bracket = position + match.start()
            for left, right in self.brackets(text[bracket]):
                if bracket in (left, right):
                    target = right if bracket == left else left
                    return Span(min(position, target), max(position, target) + 1)
            raise NoMotion('Unmatched bracket')
        if key in '{}':
            target = row
            for _ in range(count):
                direction = 1 if key == '}' else -1
                while 0 <= target + direction < len(self.lines) and not self.lines[target].strip():
                    target += direction
                while 0 <= target + direction < len(self.lines):
                    target += direction
                    if not self.lines[target].strip():
                        break
            destination = self.starts[target]
            if key == '}' and target == len(self.lines) - 1 and self.lines[target].strip():
                return Span(position, self.end(target))
            return self.exclusive(position, destination)
        raise NoMotion('Unsupported yank motion: ' + key)

    @staticmethod
    def escaped(text, position):
        i = position - 1
        while i >= 0 and text[i] == '\\':
            i -= 1
        return (position - i - 1) % 2 == 1

    def brackets(self, key):
        aliases = {'b': '(', ')': '(', 'B': '{', '}': '{', ']': '[', '>': '<'}
        opening = aliases.get(key, key)
        closing = {'(': ')', '{': '}', '[': ']', '<': '>'}[opening]
        stack, pairs = [], []
        quote = None
        for i, char in enumerate(self.text):
            if char == '\n' and quote != '`':
                quote = None
            if self.escaped(self.text, i):
                continue
            if quote:
                if char == quote:
                    quote = None
                continue
            if char in '\'"`':
                # Apostrophes in ordinary terminal prose are not strings.
                if char != "'" or not (i and self.text[i - 1].isalnum()):
                    quote = char
            elif char == opening:
                stack.append(i)
            elif char == closing and stack:
                pairs.append((stack.pop(), i))
        return pairs

    @staticmethod
    def enclosing(pairs, position, count=1):
        candidates = sorted(((a, b) for a, b in pairs if a <= position <= b), key=lambda pair: pair[1] - pair[0])
        if not candidates:
            upcoming = [(a, b) for a, b in pairs if a > position]
            if upcoming:
                first = min(upcoming)
                candidates = sorted((p for p in pairs if p[0] <= first[0] and first[1] <= p[1]), key=lambda p: p[1] - p[0])
        if len(candidates) < count:
            raise NoMotion('No enclosing text object')
        return candidates[count - 1]

    def word_object(self, position, big, around, count):
        text, row = self.text, self.line(position)
        if not self.lines[row]:
            raise NoMotion('Empty line')
        position = min(position, self.end(row) - 1)
        kind = self.kind(text[position], big)
        start, end = position, position + 1
        while start > self.starts[row] and self.kind(text[start - 1], big) == kind:
            start -= 1
        while end < self.end(row) and self.kind(text[end], big) == kind:
            end += 1
        units = self.units(big)
        if not around:
            # Inner counts include intervening whitespace as its own object:
            # 2iw on "one two" selects "one ", while 2aw selects two words.
            for _ in range(count - 1):
                if end >= len(text) - 1:
                    raise NoMotion('Not enough words')
                kind = self.kind(text[end], big)
                end += 1
                while end < len(text) - 1 and self.kind(text[end], big) == kind:
                    end += 1
            return Span(start, end)
        remaining = count - 1
        if around and kind == 0:
            remaining += 1
        for a, b in units:
            if a >= end and remaining:
                end = b
                remaining -= 1
        if remaining:
            raise NoMotion('Not enough words')
        if around and kind:
            original = end
            while end < self.end(self.line(end - 1)) and text[end].isspace():
                end += 1
            if end == original:
                # Indentation belongs to the line, rather than the first word.
                if start > self.indentation(row):
                    while start > self.starts[row] and text[start - 1].isspace():
                        start -= 1
        return Span(start, end)

    def paragraph_object(self, position, around, count):
        first = last = self.line(position)
        blank = not self.lines[first].strip()
        while first and (not self.lines[first - 1].strip()) == blank:
            first -= 1
        while last + 1 < len(self.lines) and (not self.lines[last + 1].strip()) == blank:
            last += 1
        for _ in range(count - 1 + (around and blank)):
            if last + 1 >= len(self.lines):
                raise NoMotion('Not enough paragraphs')
            last += 1
            while last + 1 < len(self.lines) and not self.lines[last].strip():
                last += 1
            while last + 1 < len(self.lines) and self.lines[last + 1].strip():
                last += 1
        if around and not blank:
            old_last = last
            while last + 1 < len(self.lines) and not self.lines[last + 1].strip():
                last += 1
            if old_last == last:
                while first and not self.lines[first - 1].strip():
                    first -= 1
        return self.line_span(first, last)

    @cached_property
    def sentences(self):
        result = []
        for match in re.finditer(r'\S(?:.*?)(?:[.!?]["\'\])]*(?=\s|$)|(?=\n\s*\n)|\Z)', self.text, re.S):
            end = match.end()
            while end > match.start() and self.text[end - 1].isspace():
                end -= 1
            if end > match.start():
                if self.text[end:end + 2] == '\n\n':
                    end += 1
                result.append((match.start(), end))
        return result

    @cached_property
    def tags(self):
        pattern = r'<!--.*?-->|<!\[CDATA\[.*?\]\]>|<\?.*?\?>|<![^>]*>|</?([A-Za-z][\w:.-]*)\b(?:[^\'">]|"[^"]*"|\'[^\']*\')*>'
        void = set('area base br col embed hr img input link meta param source track wbr'.split())
        stack, pairs = [], []
        for match in re.finditer(pattern, self.text, re.S):
            if not match.group(1):
                continue
            name = match.group(1).lower()
            if match.group().startswith('</'):
                found = next((i for i in range(len(stack) - 1, -1, -1) if stack[i][0] == name), None)
                if found is not None:
                    opening = stack[found][1]
                    pairs.append((opening.start(), match.end() - 1, opening.end(), match.start()))
                    del stack[found:]
            elif not match.group().endswith('/>') and name not in void:
                stack.append((name, match))
        return pairs

    def text_object(self, position, key, around=False, count=1):
        if key in ('w', 'W'):
            return self.word_object(position, key == 'W', around, count)
        if key == 'p':
            return self.paragraph_object(position, around, count)
        if key == 's':
            index = next((i for i, (a, b) in enumerate(self.sentences) if position < b), None)
            if index is None:
                raise NoMotion('No sentence')
            if not around:
                segments = []
                for i, (a, b) in enumerate(self.sentences):
                    segments.append((a, b))
                    if i + 1 < len(self.sentences) and b < self.sentences[i + 1][0]:
                        segments.append((b, self.sentences[i + 1][0]))
                segment = next(i for i, (a, b) in enumerate(segments) if position < b)
                start = segments[segment][0]
                end = segments[min(len(segments) - 1, segment + count - 1)][1]
                return Span(start, end)
            start, end = self.sentences[index][0], self.sentences[min(len(self.sentences) - 1, index + count - 1)][1]
            if around:
                old_end = end
                while end < len(self.text) - 1 and self.text[end].isspace() and self.text[end - 1] != '\n':
                    end += 1
                if old_end == end:
                    while start and self.text[start - 1].isspace():
                        start -= 1
            return Span(start, end)
        if key in 'bB(){}[]<>':
            left, right = self.enclosing(self.brackets(key), position, count)
            start, end = left + 1, right
            linewise = False
            if not around and key in 'B{}' and self.text[start:start + 1] == '\n':
                start += 1
                # An inner brace block with a closing brace on its own line
                # is a linewise object, without either delimiter line.
                closing_line = self.line(right)
                if not self.text[self.starts[closing_line]:right].strip():
                    end = self.starts[closing_line]
                    linewise = True
            return Span(left, right + 1) if around else Span(start, end, linewise)
        if key in 'q\'"`':
            row = self.line(position)
            pairs = []
            for delimiter in '\'"`' if key == 'q' else key:
                quotes = []
                for i in range(self.starts[row], self.end(row)):
                    if self.text[i] != delimiter or self.escaped(self.text, i):
                        continue
                    if key == 'q' and delimiter == "'" and i and i + 1 < len(self.text):
                        if self.text[i - 1].isalnum() and self.text[i + 1].isalnum():
                            continue  # Apostrophes in don't/it's are not quote delimiters.
                    quotes.append(i)
                pairs.extend(zip(quotes[::2], quotes[1::2]))
            left, right = self.enclosing(pairs, position)
            if around:
                end = right + 1
                while end < self.end(row) and self.text[end].isspace():
                    end += 1
                if end == right + 1:
                    while left > self.starts[row] and self.text[left - 1].isspace():
                        left -= 1
                return Span(left, end)
            # Vim treats a count greater than one on a quote object as
            # including the delimiters, rather than looking for nesting.
            return Span(left, right + 1) if count > 1 else Span(left + 1, right)
        if key == 't':
            left, right = self.enclosing([(a, b) for a, b, _, _ in self.tags], position, count)
            tag = next(t for t in self.tags if t[:2] == (left, right))
            return Span(left, right + 1) if around else Span(tag[2], tag[3])
        raise NoMotion('Unsupported text object: ' + key)


class Tmux:
    def __init__(self, pane, client):
        socket = os.environ.get('TMUX', '').rsplit(',', 2)[0]
        self.command = ['tmux', '-S', socket] if socket else ['tmux']
        self.pane, self.client = pane, client

    def call(self, *args, input=None):
        return subprocess.run([*self.command, *args], input=input, text=True,
                              capture_output=True, check=True, timeout=10).stdout

    def commands(self, commands):
        args = []
        for command in commands:
            if args:
                args.append(';')
            args.extend(command)
        return self.call(*args)

    def x(self, command, *args):
        return ['send-keys', '-t', self.pane, '-X', command, *map(str, args)]

    def state(self):
        names = ('copy_cursor_x', 'copy_cursor_y', 'scroll_position', 'copy_position_limit',
                 'pane_height', 'pane_width', 'selection_present', 'selection_active',
                 'selection_start_x', 'selection_start_y', 'selection_end_x', 'selection_end_y',
                 'rectangle_toggle', 'selection_mode', 'pane_mode')
        values = self.call('display-message', '-p', '-t', self.pane, '\t'.join('#{' + k + '}' for k in names)).rstrip('\n').split('\t')
        state = dict(zip(names, values))
        if state['pane_mode'] != 'copy-mode':
            raise NoMotion('Enter tmux copy mode first')
        for name in names[:-2]:
            state[name] = int(state[name] or 0)
        # Position numbering depends on this window option, not whether the
        # gutter happens to be visible at the moment.
        numbering = self.call('show-options', '-wAqv', '-t', self.pane, 'copy-mode-line-numbers').strip()
        state['absolute_line_numbers'] = numbering in ('absolute', 'relative', 'hybrid')
        state['history'] = state['copy_position_limit'] - (state['pane_height'] if state['absolute_line_numbers'] else 0)
        state['row'] = state['history'] - state['scroll_position'] + state['copy_cursor_y']
        return state

    def move(self, row, column, state, top=None):
        if top is None:
            top = max(0, row - state['pane_height'] + 1)
        top = max(0, min(top, state['history']))
        # Jump straight to the viewport. Walking through history redraws
        # every intermediate row and can block tty updates for 100 ms,
        # consuming most of a 150 ms yank flash before it is even painted.
        line = top + 1 if state['absolute_line_numbers'] else state['history'] - top
        commands = [self.x('goto-line', line)]
        screen_row = row - top
        anchors = [(0, 'top-line'), ((state['pane_height'] - 1) // 2, 'middle-line'),
                   (state['pane_height'] - 1, 'bottom-line')]
        anchor, motion = min(anchors, key=lambda item: abs(item[0] - screen_row))
        commands.append(self.x(motion))
        delta = screen_row - anchor
        if delta:
            commands.append(['send-keys', '-t', self.pane, '-X', '-N', str(abs(delta)),
                             'cursor-down' if delta > 0 else 'cursor-up'])
        commands.append(self.x('start-of-line'))
        steps = state['layout'].steps(row, column) if 'layout' in state else column
        if steps:
            commands.append(['send-keys', '-t', self.pane, '-X', '-N', str(steps), 'cursor-right'])
        return commands

    def restore(self, state):
        commands = [self.x('clear-selection'), self.x('rectangle-on' if state['rectangle_toggle'] else 'rectangle-off')]
        if state['selection_present'] or state['selection_active']:
            commands += self.move(state['selection_start_y'], state['selection_start_x'], state)
            commands.append(self.x('begin-selection'))
            commands += self.move(state['selection_end_y'], state['selection_end_x'], state)
            commands.append(self.x('selection-mode', state['selection_mode']))
            if not state['selection_active']:
                commands.append(self.x('stop-selection'))
        commands += self.move(state['row'], state['copy_cursor_x'], state,
                              state['history'] - state['scroll_position'])
        return commands

    def snapshot(self, state):
        # Read the frozen snapshot, not the live pane, which may still be
        # receiving output. No buffer/clipboard round trip is involved.
        with tempfile.TemporaryDirectory(prefix='tmux-vim-copy-') as temporary:
            path = Path(temporary) / 'text'
            signal = 'vim-copy-' + uuid.uuid4().hex
            pipe = 'cat > ' + shlex.quote(str(path)) + '; ' + shlex.join([*self.command, 'wait-for', '-S', signal])
            commands = [self.x('clear-selection'), self.x('rectangle-off'),
                        self.x('history-top'), self.x('start-of-line'), self.x('begin-selection'),
                        self.x('history-bottom'), self.x('end-of-line'), self.x('copy-pipe', '-C', '-P', pipe)]
            commands += self.restore(state)
            self.commands(commands)
            self.call('wait-for', signal)
            document = Document(path.read_text())
            layout = Layout(document, state['pane_width'])
            state['layout'] = layout
            # Native cursor-right counts characters, rather than cells. The
            # first restoration is corrected once the snapshot supplies that
            # mapping (wide and combining characters need fewer steps).
            if not document.text.isascii():
                self.commands(self.restore(state))
            return document

    def flash(self):
        """Freeze the copied range while keeping navigation and new selections live."""
        duration = self.call('show-options', '-Aqv', '-t', self.pane,
                             '@vim-copy-highlight-ms').strip() or '150'
        milliseconds = max(0, min(5000, int(duration)))
        if not milliseconds:
            self.commands([self.x('clear-selection')])
            return
        token = uuid.uuid4().hex
        self.commands([
            self.x('stop-selection'),
            ['set-option', '-p', '-t', self.pane, '@vim-copy-flash', token],
            ['set-option', '-pF', '-t', self.pane, '@vim-copy-flash-selection', SELECTION_SIGNATURE],
            ['refresh-client', '-t', self.client],
            ['run-shell', '-b', '-d', str(milliseconds / 1000), '-t', self.pane,
             shell_command('finish-flash', self.pane, self.client, token)],
        ])

    def finish_flash(self, token):
        # Check the token inside the same tmux command queue as cleanup. A
        # separate read followed by cleanup could race with a newer yank.
        clear = ('if-shell -F -t ' + quote(self.pane) + ' ' + quote(
            '#{&&:#{==:#{pane_mode},copy-mode},' + FLASH_SELECTION + '}') +
            ' { send-keys -t ' + quote(self.pane) + ' -X clear-selection }; ')
        clear += ('set-option -pu -t ' + quote(self.pane) + ' @vim-copy-flash; '
                  'set-option -pu -t ' + quote(self.pane) + ' @vim-copy-flash-selection')
        self.call('if-shell', '-F', '-t', self.pane,
                  '#{==:#{@vim-copy-flash},' + token + '}', clear)
        # The originating client may have detached during the flash.
        subprocess.run([*self.command, 'refresh-client', '-t', self.client],
                       capture_output=True, timeout=10)


class Layout:
    """Translate character offsets to tmux rows and terminal cell columns."""

    def __init__(self, document, width):
        self.document = document
        self.rows = []
        for number, line in enumerate(document.lines):
            start, column = document.starts[number], 0
            if line.isascii():
                self.rows.extend((start + offset, start + min(len(line), offset + width))
                                 for offset in range(0, len(line) or 1, width))
                continue
            for i, char in enumerate(line):
                cells = cell_width(char)
                if cells and column + cells > width:
                    self.rows.append((start, document.starts[number] + i))
                    start, column = document.starts[number] + i, 0
                column += cells
            self.rows.append((start, document.end(number)))
        self.starts = [a for a, b in self.rows]

    def offset(self, row, column):
        start, end = self.rows[min(row, len(self.rows) - 1)]
        cell = 0
        for i in range(start, end):
            width = cell_width(self.document.text[i])
            if width and cell >= column:
                return i
            cell += width
        return end

    def point(self, offset):
        row = min(len(self.rows) - 1, bisect_right(self.starts, offset) - 1)
        column = sum(cell_width(c) for c in self.document.text[self.rows[row][0]:offset])
        return row, column

    def steps(self, row, column):
        end = self.offset(row, column)
        return sum(cell_width(c) > 0 for c in self.document.text[self.rows[row][0]:end])


def quote(value):
    """Quote a tmux argument; this is not shell quoting."""
    return '"' + str(value).replace('\\', '\\\\').replace('"', '\\"').replace('$', '\\$') + '"'


def shell_command(*args):
    # tmux expands formats in run-shell, including text in shell quotes.
    return shlex.join(['python3', str(Path(__file__).resolve()), *args]).replace('#', '##')


def select_span(tmux, state, start, end, linewise=False):
    """Build a selection for a half-open text range, including complete cells."""
    document, layout = state['layout'].document, state['layout']
    end -= 1
    while end > start and cell_width(document.text[end]) == 0:
        end -= 1
    if linewise:
        last = document.line(end)
        end = max(document.starts[last], document.end(last) - 1)
    commands = [tmux.x('clear-selection'), tmux.x('rectangle-off')]
    commands += tmux.move(*layout.point(start), state)
    commands.append(tmux.x('begin-selection'))
    commands += tmux.move(*layout.point(end), state)
    if linewise:
        commands.append(tmux.x('selection-mode', 'line'))
    return commands


def perform(tmux, context, argument=None):
    count = context['before'] * int(context.get('digits') or '1')
    if not 1 <= count <= MAX_COUNT:
        raise NoMotion('Copy count is too large')
    state = tmux.state()
    document = tmux.snapshot(state)
    layout = state['layout']
    if len(layout.rows) != state['history'] + state['pane_height']:
        raise NoMotion('Cannot map this terminal buffer to copy-mode rows')
    position = layout.offset(state['row'], state['copy_cursor_x'])
    key = context['motion']
    if key in ('/', '?', 'n', 'N'):
        native = {'/': 'search-forward', '?': 'search-backward', 'n': 'search-again', 'N': 'search-reverse'}[key]
        args = [argument or ''] if key in '/?' else []
        if args == ['']:
            args = [tmux.call('display-message', '-p', '-t', tmux.pane, '#{pane_search_string}').rstrip('\n')]
        try:
            tmux.call('send-keys', '-t', tmux.pane, '-X', '-N', str(count), native, *(['--', *args] if args else []))
            target = tmux.state()
            end = layout.offset(target['row'], target['copy_cursor_x'])
        finally:
            tmux.commands(tmux.restore(state))
        if position == end:
            raise NoMotion('No search match')
        span = document.exclusive(position, end)
    elif context.get('object'):
        span = document.text_object(position, key, context['object'] == 'a', count)
    else:
        span = document.motion(position, key, count, argument,
                               context.get('explicit', False) or context['before'] != 1 or bool(context.get('digits')))
    if span.end <= span.start:
        raise NoMotion('Text object is empty')
    # A terminal cell can hold a base character plus combining marks. Keep
    # the complete cell when an inclusive motion ends on its base character.
    end = span.end
    while end < len(document.text) and cell_width(document.text[end]) == 0:
        end += 1
    if end != span.end:
        span = Span(span.start, end, span.linewise, span.cursor)
    if context['mode'] == 'yank':
        tmux.call('load-buffer', '-w', '-t', tmux.client, '-', input=document.text[span.start:span.end])
        cursor = position if context.get('object') else (
            span.cursor if span.cursor is not None else span.start)
        row, column = layout.point(cursor)
        top = max(0, min(state['history'] - state['scroll_position'], row))
        top = max(top, row - state['pane_height'] + 1)
        tmux.commands([
            *select_span(tmux, state, span.start, span.end, span.linewise),
            tmux.x('stop-selection'),
            *tmux.move(row, column, state, top),
        ])
        tmux.flash()
    else:
        start, end = span.start, span.end
        if key in ('w', 'W', 'p', 's') and state['selection_present']:
            a = layout.offset(state['selection_start_y'], state['selection_start_x'])
            b = layout.offset(state['selection_end_y'], state['selection_end_x'])
            start, end = min(start, a, b), max(end, a + 1, b + 1)
        tmux.commands(select_span(tmux, state, start, end, span.linewise))


def bindings():
    # Pending input is handled synchronously by tmux tables/prompts. Launching
    # a helper between keys lets fast typing outrun installation of a prompt.
    # The helper runs only once a complete operation has been entered.
    command = shell_command('execute') + (
        ' #{q:pane_id} #{q:client_name} #{q:@vim-copy-mode}'
        ' #{q:@vim-copy-before} #{q:@vim-copy-after} #{q:@vim-copy-object} ')
    for name, value in [('@vim-copy-highlight-ms', '150'),
                        ('@vim-copy-highlight-style', 'bg=#313131'),
                        ('@vim-copy-selection-style', 'bg=#313131')]:
        print('set-option -goq ' + name + ' ' + quote(value))
    print('set-option -g copy-mode-selection-style ' + quote(
        '#{?' + FLASH_SELECTION + ',#{@vim-copy-highlight-style},#{@vim-copy-selection-style}}'))

    def emit(table, key, body):
        print('bind -T ' + table + ' ' + quote(key) + ' { ' + body + ' }')

    def execute(motion, argument=False):
        shell = command + shlex.quote(motion)
        run = 'run-shell ' + quote(shell)
        # Keep prompt input out of shell code altogether, including quotes,
        # empty searches and tmux format characters. The helper reads the
        # pane option through a separate tmux command.
        # %1 is the raw substitution for a structured command argument.
        # %% would add shell-style escaping to apostrophes in the value.
        return 'set-option -p @vim-copy-argument "%1"; ' + run if argument else run

    def enter(mode, before='0', object='-'):
        table = 'copyVimYank' if mode == 'yank' else 'copyVimObject'
        return ('set-option -p @vim-copy-mode ' + mode + '; '
                'set-option -pF @vim-copy-before ' + quote(before) + '; '
                'set-option -p @vim-copy-after 0; '
                'set-option -p @vim-copy-object ' + object + '; '
                'send-keys -N 1; switch-client -T ' + table)

    cancel_flash = ('if-shell -F ' + quote(FLASH_SELECTION) +
                    ' { send-keys -X clear-selection; set-option -pu @vim-copy-flash; '
                    'set-option -pu @vim-copy-flash-selection }; ')
    visual_yank = shell_command('visual-yank') + ' #{q:pane_id} #{q:client_name}'

    for table, before in [('copy-mode-vi', '0'), ('copyVimCount', '#{@vim-copy-count}')]:
        emit(table, 'y', cancel_flash + "if-shell -F '#{||:#{selection_present},#{selection_active}}' "
             '{ run-shell ' + quote(visual_yank) + ' } { ' + enter('yank', before) + ' }')
        for key in ('i', 'a'):
            emit(table, key, cancel_flash + "if-shell -F '#{||:#{selection_present},#{selection_active}}' { " + enter('visual', before, key) + ' }')
    for digit in '123456789':
        emit('copy-mode-vi', digit, 'command-prompt -NP -I ' + digit +
             ' -p "(repeat)" { send-keys -N "%%"; set-option -p @vim-copy-count "%%"; switch-client -T copyVimCount }')
        emit('copyVimYank', digit, 'command-prompt -NP -I ' + digit +
             ' -p "(y count)" { set-option -p @vim-copy-after "%%"; switch-client -T copyVimYank }')
    emit('copyVimCount', 'Any', 'switch-client -T root; send-keys -K')
    for key in 'yeEbBwW^0$hl%jkG{}nN':
        emit('copyVimYank', key, execute(key))
    for key in ('i', 'a'):
        emit('copyVimYank', key, 'set-option -p @vim-copy-object ' + key + '; switch-client -T copyVimObject')
    for key in 'fFtT/?':
        flags = '-P -T search' if key in '/?' else '-1P'
        emit('copyVimYank', key, 'command-prompt ' + flags + ' -p ' + quote('(y' + key + ')') + ' { ' + execute(key, True) + ' }')
    emit('copyVimYank', 'g', 'switch-client -T copyVimG')
    emit('copyVimG', 'g', execute('gg'))
    for key in 'wWp sbB(){}[]<>tq\'"`'.replace(' ', ''):
        emit('copyVimObject', key, execute(key))
    for table in ('copyVimYank', 'copyVimG', 'copyVimObject'):
        emit(table, 'Any', 'send-keys -N 1')


def main():
    if sys.argv[1:] == ['bindings']:
        bindings()
        return
    action, pane, client, *args = sys.argv[1:]
    tmux = Tmux(pane, client)
    try:
        if action == 'finish-flash':
            tmux.finish_flash(args[0])
        elif action == 'visual-yank':
            tmux.call('send-keys', '-t', pane, '-X', 'copy-selection-no-clear')
            tmux.flash()
        elif action == 'execute':
            mode, before, after, object, motion, *argument = args
            context = {'mode': mode, 'before': max(1, int(before)), 'digits': after if after != '0' else '',
                       'motion': motion, 'explicit': before != '0' or after != '0'}
            if object != '-':
                context['object'] = object
            value = None
            if object == '-' and motion in ('f', 'F', 't', 'T', '/', '?'):
                value = tmux.call('show-options', '-pqv', '-t', pane, '@vim-copy-argument').removesuffix('\n')
            perform(tmux, context, value)
    except (NoMotion, subprocess.SubprocessError, OSError, ValueError) as error:
        if action == 'finish-flash':
            return  # Closing a pane/client during its timer is harmless.
        message = str(error) if isinstance(error, NoMotion) else 'Copy command failed'
        tmux.call('display-message', '-c', client, '--', message)


if __name__ == '__main__':
    main()
