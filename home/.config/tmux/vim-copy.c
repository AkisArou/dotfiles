/*
 * Vim yank motions and text objects for tmux 3.8, using only libc.
 *
 * Runtime never starts an editor. capture-pane -M reads the frozen copy-mode
 * buffer without moving the cursor or changing selections, buffers or
 * clipboard. Only a successful yank writes a buffer. Esc cancels any pending
 * input. The q object matches any quote type; successful yanks briefly
 * highlight their range. gv restores the last visual selection; ( and ) move
 * by sentences. ge and gE move backwards to word ends, including in visual
 * mode and yanks. gx opens the hyperlink or URL under the copy cursor with
 * the system opener. Configure @vim-copy-highlight-ms and
 * @vim-copy-highlight-style in tmux.
 *
 * Text positions are code point offsets. Character classes and cell widths
 * come from vim-copy-unicode.h rather than the C library, so results never
 * depend on the locale tmux happens to run the helper with.
 *
 * A binding runs the helper once per complete operation; pending keys stay
 * in tmux key tables. The helper keeps no state between runs and talks to
 * tmux through two short-lived clients: one reads the state and the rows
 * around the cursor, reading further only until a result is proven, and
 * one applies the result. Highlights expire through a delayed tmux command,
 * and plain ( ) { } moves use cached tmux formats without the helper.
 *
 * Run `make` to build, `vim-copy bindings` to emit the tmux bindings, or
 * `python3 -m unittest discover -s home/.config/tmux -p 'test_vim_copy.py'`
 * from the dotfiles checkout to test.
 */

#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <signal.h>
#include <spawn.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/random.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#include "vim-copy-unicode.h"

extern char **environ;

#define MAX_COUNT 100000
#define NONE LONG_MIN  /* An omitted optional number. */
#define ALL LONG_MAX   /* Prefetch every row rather than nearby ones. */
#define LENGTH(array) ((long)(sizeof(array) / sizeof *(array)))

static long lmin(long a, long b) { return a < b ? a : b; }
static long lmax(long a, long b) { return a > b ? a : b; }
static bool is(const char *a, const char *b) { return !strcmp(a, b); }

/* A single character key that occurs in set. */
static bool one_of(const char *key, const char *set) { return key[0] && !key[1] && strchr(set, key[0]); }
static bool in_ascii(uint32_t c, const char *set) { return c && c < 128 && strchr(set, (int)c); }

/* ---------------------------------------------------------------------- */
/* Memory: the helper exits within milliseconds, so allocations come from an
 * arena released all at once. Only the test probe ever releases it. */

typedef struct Chunk {
    struct Chunk *next;
    size_t used, size;
    max_align_t data[];
} Chunk;

static Chunk *arena;

/* Uninitialized memory, for buffers that are written before they are read. */
static void *allocate(size_t size) {
    enum { CHUNK = 1 << 18 };
    size = (size + sizeof(max_align_t) - 1) / sizeof(max_align_t) * sizeof(max_align_t);
    if (!arena || arena->size - arena->used < size) {
        size_t capacity = size > CHUNK ? size : CHUNK;
        Chunk *chunk = malloc(sizeof *chunk + capacity);
        if (!chunk) {
            fputs("vim-copy: out of memory\n", stderr);
            exit(1);
        }
        chunk->used = 0;
        chunk->size = capacity;
        if (arena && size > CHUNK) {
            // Keep filling the current chunk after a large one-off block.
            chunk->next = arena->next;
            arena->next = chunk;
        } else {
            chunk->next = arena;
            arena = chunk;
        }
        if (chunk != arena) {
            chunk->used = size;
            return chunk->data;
        }
    }
    void *block = (char *)arena->data + arena->used;
    arena->used += size;
    return block;
}

static void *alloc(size_t size) { return memset(allocate(size), 0, size); }

#ifdef VIM_COPY_PROBE
static void release(void) {
    while (arena) {
        Chunk *next = arena->next;
        free(arena);
        arena = next;
    }
}
#endif

static void *reserve(void *items, long *capacity, long need, size_t size) {
    if (need <= *capacity)
        return items;
    long grown = *capacity ? *capacity : 8;
    while (grown < need)
        grown *= 2;
    void *copy = allocate(grown * size);
    if (items && *capacity)
        memcpy(copy, items, *capacity * size);
    *capacity = grown;
    return copy;
}

#define push(list, ...) \
    ((list)->v = reserve((list)->v, &(list)->cap, (list)->n + 1, sizeof *(list)->v), \
     (list)->v[(list)->n++] = (__VA_ARGS__))

typedef struct { long start, end; } Pair;
typedef struct { Pair *v; long n, cap; } Pairs;
typedef struct { long *v; long n, cap; } Longs;
typedef struct { char *v; long n, cap; } Buf;

static void putn(Buf *buffer, const char *text, long length) {
    if (!length && buffer->v)
        return;
    buffer->v = reserve(buffer->v, &buffer->cap, buffer->n + length + 1, 1);
    memcpy(buffer->v + buffer->n, text, length);
    buffer->n += length;
    buffer->v[buffer->n] = 0;
}

static void put(Buf *buffer, const char *text) { putn(buffer, text, strlen(text)); }
static void putch(Buf *buffer, char c) { putn(buffer, &c, 1); }

static char *text_of(Buf *buffer) {
    if (!buffer->v)
        putn(buffer, "", 0);
    return buffer->v;
}

static char *format(const char *pattern, ...) {
    va_list arguments;
    va_start(arguments, pattern);
    int length = vsnprintf(NULL, 0, pattern, arguments);
    va_end(arguments);
    char *text = alloc(length + 1);
    va_start(arguments, pattern);
    vsnprintf(text, length + 1, pattern, arguments);
    va_end(arguments);
    return text;
}

/* Concatenate strings up to a terminating NULL. */
static char *cat(const char *first, ...) {
    Buf buffer = {0};
    va_list arguments;
    va_start(arguments, first);
    for (const char *part = first; part; part = va_arg(arguments, const char *))
        put(&buffer, part);
    va_end(arguments);
    return text_of(&buffer);
}
#define CAT(...) cat(__VA_ARGS__, (const char *)NULL)

static const char *num(long value) { return format("%ld", value); }
static char *letter(char c) { return format("%c", c); }

/* A decimal integer, allowing a sign and surrounding whitespace. */
static bool parse_long(const char *text, long *value) {
    char *end;
    errno = 0;
    *value = strtol(text, &end, 10);
    if (end == text || errno)
        return false;
    while (*end == ' ' || *end == '\t' || *end == '\n')
        end++;
    return !*end;
}

static char *strip(char *text) {
    while (*text == ' ' || *text == '\t' || *text == '\n')
        text++;
    long length = strlen(text);
    while (length && (text[length - 1] == ' ' || text[length - 1] == '\t' || text[length - 1] == '\n'))
        text[--length] = 0;
    return text;
}

/* ---------------------------------------------------------------------- */
/* Unicode */

static unsigned properties(uint32_t c) {
    if (c < 128)
        return ascii_properties[c];
    long low = 0, high = LENGTH(unicode_ranges);
    while (high - low > 1) {
        long middle = (low + high) / 2;
        if (unicode_ranges[middle].start <= c)
            low = middle;
        else
            high = middle;
    }
    return unicode_ranges[low].properties;
}

static int cell_width(uint32_t c) {
    unsigned value = properties(c);
    return value & ZERO_WIDTH ? 0 : value & WIDE ? 2 : 1;
}

static bool is_space(uint32_t c) { return properties(c) & SPACE; }
static bool is_alnum(uint32_t c) { return properties(c) & ALNUM; }
static bool is_word(uint32_t c) { return c == '_' || is_alnum(c); }
static bool is_ascii_alpha(uint32_t c) { return (c | 32) >= 'a' && (c | 32) <= 'z'; }
static uint32_t ascii_lower(uint32_t c) { return c >= 'A' && c <= 'Z' ? c | 32 : c; }

/* Decode UTF-8, replacing each invalid byte with U+FFFD. */
static long decode(const char *bytes, long length, uint32_t *out) {
    long count = 0;
    for (long i = 0; i < length;) {
        unsigned char c = bytes[i];
        uint32_t code = c;
        int size = c < 0x80 ? 1 : c >= 0xC2 && c < 0xE0 ? 2 : c >= 0xE0 && c < 0xF0 ? 3 : c >= 0xF0 && c < 0xF5 ? 4 : 0;
        bool valid = size && i + size <= length;
        if (size > 1 && valid) {
            code = c & (0x7F >> size);
            for (int k = 1; k < size && valid; k++) {
                unsigned char next = bytes[i + k];
                valid = (next & 0xC0) == 0x80;
                code = code << 6 | (next & 0x3F);
            }
            if ((size == 3 && (code < 0x800 || (code >= 0xD800 && code <= 0xDFFF))) ||
                (size == 4 && (code < 0x10000 || code > 0x10FFFF)))
                valid = false;
        }
        out[count++] = valid ? code : 0xFFFD;
        i += valid ? size : 1;
    }
    return count;
}

static void put_utf8(Buf *buffer, uint32_t c) {
    char bytes[4];
    int size = 0;
    if (c < 0x80) {
        bytes[size++] = c;
    } else if (c < 0x800) {
        bytes[size++] = 0xC0 | c >> 6;
        bytes[size++] = 0x80 | (c & 0x3F);
    } else if (c < 0x10000) {
        bytes[size++] = 0xE0 | c >> 12;
        bytes[size++] = 0x80 | (c >> 6 & 0x3F);
        bytes[size++] = 0x80 | (c & 0x3F);
    } else {
        bytes[size++] = 0xF0 | c >> 18;
        bytes[size++] = 0x80 | (c >> 12 & 0x3F);
        bytes[size++] = 0x80 | (c >> 6 & 0x3F);
        bytes[size++] = 0x80 | (c & 0x3F);
    }
    putn(buffer, bytes, size);
}

/* ---------------------------------------------------------------------- */
/* Failures: a failed motion must leave the selection and clipboard alone. */

enum { OK, NO_MOTION, INCOMPLETE, FAILED };

/* INCOMPLETE means the motion needs text outside a captured fragment. */
static const char *reason;

static int fail(int status, const char *why) {
    reason = why;
    return status;
}

#define TRY(expression) \
    do { \
        int status_ = (expression); \
        if (status_) \
            return status_; \
    } while (0)

/* ---------------------------------------------------------------------- */
/* Documents */

/* Half-open range; linewise ranges include the final newline. */
typedef struct {
    long start, end;
    bool linewise;
    long cursor;   /* -1 when the cursor goes to the start. */
    bool anywhere; /* Text anywhere in the document could change it. */
} Span;

typedef struct { long left, right, inner_start, inner_end; } Tag;
typedef struct { Tag *v; long n, cap; } Tags;

typedef struct {
    uint32_t *text;
    long length, lines;
    long *starts; /* lines + 1 entries; the text always ends with a newline. */
    bool complete_start, complete_end;
    Pairs units[4];
    Longs sentence_starts, backward_starts;
    Pairs sentences;
    Tags tags;
    bool have_units[4], have_sentence_starts, have_backward_starts, have_sentences, have_tags;
} Document;

static Span span_of(long start, long end) { return (Span){start, end, false, -1, false}; }

/* Take over text, which must have room for one more code point. */
static Document *document(uint32_t *text, long length) {
    Document *d = alloc(sizeof *d);
    if (!length || text[length - 1] != '\n')
        text[length++] = '\n';
    d->text = text;
    d->length = length;
    for (long i = 0; i < length; i++)
        d->lines += text[i] == '\n';
    d->starts = allocate((d->lines + 1) * sizeof *d->starts);
    d->starts[0] = 0;
    for (long i = 0, row = 1; i < length; i++)
        if (text[i] == '\n')
            d->starts[row++] = i + 1;
    d->complete_start = d->complete_end = true;
    return d;
}

static Document *document_from_bytes(const char *bytes, long length) {
    uint32_t *text = allocate((length + 1) * sizeof *text);
    return document(text, decode(bytes, length, text));
}

static long bisect(const long *items, long count, long value, bool right) {
    long low = 0, high = count;
    while (low < high) {
        long middle = low + (high - low) / 2;
        if (right ? items[middle] <= value : items[middle] < value)
            low = middle + 1;
        else
            high = middle;
    }
    return low;
}

/* Bisect unit starts, or their last characters when ends is set. */
static long bisect_units(const Pairs *units, long value, bool right, bool ends) {
    long low = 0, high = units->n;
    while (low < high) {
        long middle = low + (high - low) / 2;
        long item = ends ? units->v[middle].end - 1 : units->v[middle].start;
        if (right ? item <= value : item < value)
            low = middle + 1;
        else
            high = middle;
    }
    return low;
}

static long line_of(const Document *d, long position) {
    return lmin(d->lines - 1, bisect(d->starts, d->lines + 1, position, true) - 1);
}

static long line_end(const Document *d, long row) { return d->starts[row + 1] - 1; }
static long line_length(const Document *d, long row) { return line_end(d, row) - d->starts[row]; }

static long indentation(const Document *d, long row) {
    long i = d->starts[row], end = line_end(d, row);
    while (i < end && (d->text[i] == ' ' || d->text[i] == '\t'))
        i++;
    return i;
}

static Span line_span(const Document *d, long first, long last, long cursor) {
    if (first > last) {
        long swap = first;
        first = last;
        last = swap;
    }
    return (Span){d->starts[first], d->starts[last + 1], true, cursor, false};
}

/* Whether a nonempty range is all whitespace. */
static bool all_space(const Document *d, long start, long end) {
    if (start >= end)
        return false;
    for (long i = start; i < end; i++)
        if (!is_space(d->text[i]))
            return false;
    return true;
}

/* Whether a range has no visible text, as an empty range has none. */
static bool blank(const Document *d, long start, long end) { return start >= end || all_space(d, start, end); }
static bool blank_line(const Document *d, long row) { return blank(d, d->starts[row], line_end(d, row)); }

static char *copy_text(const Document *d, Span span) {
    Buf buffer = {0};
    buffer.v = reserve(buffer.v, &buffer.cap, 4 * (span.end - span.start) + 1, 1);
    buffer.v[0] = 0;
    for (long i = span.start; i < span.end; i++) {
        uint32_t c = d->text[i];
        // Terminal UIs paint trailing spaces to erase the rest of a row.
        // Keep indentation and whitespace-only lines, but don't put that
        // right padding after visible text into a line yank.
        if (span.linewise && (c == ' ' || c == '\t') && i > span.start && !is_space(d->text[i - 1])) {
            long end = i;
            while (end < span.end && (d->text[end] == ' ' || d->text[end] == '\t'))
                end++;
            if (end < span.end && d->text[end] == '\n') {
                i = end - 1;
                continue;
            }
        }
        put_utf8(&buffer, c);
    }
    return text_of(&buffer);
}

static int kind(uint32_t c, bool big) {
    unsigned value = properties(c);
    if (value & SPACE)
        return 0;
    if (big)
        return 1;
    return value & (ALNUM | COMBINING) || c == '_' ? 1 : 2;
}

static Pairs *units(Document *d, bool big, bool empty) {
    int slot = big * 2 + empty;
    Pairs *result = &d->units[slot];
    if (d->have_units[slot])
        return result;
    d->have_units[slot] = true;
    for (long row = 0; row < d->lines; row++) {
        long start = d->starts[row], end = line_end(d, row);
        if (start == end && empty)
            push(result, (Pair){start, start + 1});
        for (long i = start; i < end;) {
            int group = kind(d->text[i], big);
            long next = i + 1;
            while (next < end && kind(d->text[next], big) == group)
                next++;
            if (group)
                push(result, (Pair){i, next});
            i = next;
        }
    }
    return result;
}

/* Vim's exclusive motion ending in column zero excludes that line. */
static Span exclusive(const Document *d, long origin, long target) {
    long start = lmin(origin, target), end = lmax(origin, target);
    long first = line_of(d, start), last = line_of(d, end);
    if (last > first && end == d->starts[last]) {
        if (start <= indentation(d, first))
            return line_span(d, first, last - 1, -1);
        end -= 1;
    }
    return span_of(start, end);
}

static int previous_word_end(Document *d, long position, long count, bool big, long *target) {
    Pairs *all = units(d, big, true);
    long *ends = alloc((all->n + 1) * sizeof *ends);
    for (long i = 0; i < all->n; i++) {
        long end = all->v[i].end - 1;
        while (end > all->v[i].start && cell_width(d->text[end]) == 0)
            end--;
        ends[i] = end;
    }
    long index = bisect(ends, all->n, position, false) - count;
    if (index < 0 && !d->complete_start)
        return fail(INCOMPLETE, "Need earlier words");
    *target = index >= 0 ? ends[index] : 0;
    return OK;
}

static bool escaped(const Document *d, long position) {
    long i = position - 1;
    while (i >= 0 && d->text[i] == '\\')
        i--;
    return (position - i - 1) % 2 == 1;
}

static uint32_t opening_of(uint32_t key) {
    switch (key) {
    case 'b': case ')': return '(';
    case 'B': case '}': return '{';
    case ']': return '[';
    case '>': return '<';
    default: return key;
    }
}

static uint32_t closing_of(uint32_t opening) {
    switch (opening) {
    case '(': return ')';
    case '{': return '}';
    case '[': return ']';
    default: return '>';
    }
}

/* Pairs of the bracket key. Without a position, every pair in the text;
 * with one, the pairs that can enclose it or follow it. Quotes end with
 * their line, as in Vim, so pairs that start after the first line do not
 * depend on any earlier text. A Markdown fence or a stray backtick must
 * not turn the rest of the scrollback into one string. */
static Pairs brackets(const Document *d, uint32_t key, bool located, long position, long count) {
    uint32_t opening = opening_of(key), closing = closing_of(opening), quote = 0;
    Longs stack = {0};
    Pairs pairs = {0};
    long upcoming = -1, enclosed = 0;
    for (long i = 0; i < d->length; i++) {
        uint32_t c = d->text[i];
        if (c != '\n' && c != '\'' && c != '"' && c != '`' && c != opening && c != closing)
            continue;
        if (c == '\n')
            quote = 0;
        if (escaped(d, i))
            continue;
        if (quote) {
            if (c == quote)
                quote = 0;
            continue;
        }
        if (c == '\'' || c == '"' || c == '`') {
            // Apostrophes in ordinary terminal prose are not strings.
            if (c != '\'' || !(i && is_alnum(d->text[i - 1])))
                quote = c;
        } else if (c == opening) {
            push(&stack, i);
            if (located && i > position && upcoming < 0)
                upcoming = i;
        } else if (c == closing && stack.n) {
            long left = stack.v[--stack.n];
            bool around = left <= position && position <= i;
            if (!located || left > position || around)
                push(&pairs, (Pair){left, i});
            if (located) {
                if (around) {
                    enclosed++;
                    if (enclosed >= count || !stack.n)
                        break;
                } else if (left == upcoming && !stack.n && !enclosed) {
                    break;
                }
            }
        }
    }
    return pairs;
}

typedef struct { Pair pair; long index; } Candidate;
typedef struct { Candidate *v; long n, cap; } Candidates;

/* Shortest first, keeping equal lengths in their original order. */
static int by_length(const void *a, const void *b) {
    const Candidate *x = a, *y = b;
    long first = x->pair.end - x->pair.start, second = y->pair.end - y->pair.start;
    if (first != second)
        return first < second ? -1 : 1;
    return (x->index > y->index) - (x->index < y->index);
}

/* The count-th pair around position, or else the first that follows it,
 * which only the whole document can show is not inside another pair. */
static int enclosing(const Pairs *pairs, long position, long count, Pair *result, bool *following) {
    Candidates candidates = {0};
    if (following)
        *following = false;
    for (long i = 0; i < pairs->n; i++)
        if (pairs->v[i].start <= position && position <= pairs->v[i].end)
            push(&candidates, (Candidate){pairs->v[i], i});
    if (!candidates.n) {
        Pair first = {-1, -1};
        for (long i = 0; i < pairs->n; i++) {
            Pair p = pairs->v[i];
            if (p.start > position && (first.start < 0 || p.start < first.start ||
                                       (p.start == first.start && p.end < first.end)))
                first = p;
        }
        if (following)
            *following = true;
        if (first.start >= 0)
            for (long i = 0; i < pairs->n; i++)
                if (pairs->v[i].start <= first.start && first.end <= pairs->v[i].end)
                    push(&candidates, (Candidate){pairs->v[i], i});
    }
    if (candidates.n < count)
        return fail(NO_MOTION, "No enclosing text object");
    qsort(candidates.v, candidates.n, sizeof *candidates.v, by_length);
    *result = candidates.v[count - 1].pair;
    return OK;
}

static bool paragraph_boundary(const Document *d, long row) {
    static const char macros[] = "IPLPPPQPP TPHPLIPpLpItpplpipbpSHNHH HUnhsh";
    const uint32_t *line = d->text + d->starts[row];
    long length = line_length(d, row);
    // :help paragraph: whitespace-only lines are not empty lines. Match
    // the default 'paragraphs' and 'sections' nroff macros in column zero.
    if (!length || line[0] == '\f')
        return true;
    if (line[0] != '.')
        return false;
    uint32_t a = length > 1 ? line[1] : ' ', b = length > 2 ? line[2] : ' ';
    for (const char *macro = macros; *macro; macro += 2)
        if (a == (uint32_t)macro[0] && b == (uint32_t)macro[1])
            return true;
    return false;
}

static int paragraph_target(Document *d, long position, char key, long count, long *result) {
    long row = line_of(d, position), direction = key == '}' ? 1 : -1;
    // Vim's findpar(): ignore the starting line, skip a run of empty
    // lines, then stop at a boundary after encountering nonempty text.
    for (long remaining = count; remaining > 0; remaining--) {
        bool skipped = false, first = true;
        for (;;) {
            skipped = skipped || line_length(d, row);
            if (!first && skipped && paragraph_boundary(d, row))
                break;
            if (row + direction < 0 || row + direction >= d->lines) {
                if (!(direction > 0 ? d->complete_end : d->complete_start))
                    return fail(INCOMPLETE, "Need more paragraphs");
                if (remaining > 1)
                    return fail(NO_MOTION, "Not enough paragraphs");
                break;
            }
            row += direction;
            first = false;
        }
    }
    bool last = direction > 0 && row == d->lines - 1;
    if (last && !d->complete_end)
        return fail(INCOMPLETE, "Need the complete paragraph boundary");
    if (last && line_length(d, row)) {
        long target = line_end(d, row) - 1;
        while (target > d->starts[row] && cell_width(d->text[target]) == 0)
            target--;
        *result = target;
        return OK;
    }
    *result = d->starts[row];
    return OK;
}

static int compare_longs(const void *a, const void *b) {
    long x = *(const long *)a, y = *(const long *)b;
    return (x > y) - (x < y);
}

static void sort_unique(Longs *items) {
    qsort(items->v, items->n, sizeof *items->v, compare_longs);
    long count = 0;
    for (long i = 0; i < items->n; i++)
        if (!count || items->v[count - 1] != items->v[i])
            items->v[count++] = items->v[i];
    items->n = count;
}

/* An nroff macro name after its period, ignoring trailing whitespace. */
static bool sentence_macro(const uint32_t *name, long length) {
    static const char *const macros[] = {"IP", "LP", "PP", "QP", "P", "TP", "HP", "LI", "Pp", "Lp", "It",
                                         "pp", "lp", "ip", "bp", "SH", "NH", "H", "HU", "nh", "sh"};
    while (length && is_space(name[length - 1]))
        length--;
    for (long i = 0; i < LENGTH(macros); i++) {
        long size = strlen(macros[i]);
        if (size == length && name[0] == (uint32_t)macros[i][0] && (size < 2 || name[1] == (uint32_t)macros[i][1]))
            return true;
    }
    return false;
}

static Longs *sentence_starts(Document *d) {
    Longs *starts = &d->sentence_starts;
    if (d->have_sentence_starts)
        return starts;
    d->have_sentence_starts = true;
    // Vim's default sentence rules (:help sentence). Empty lines and
    // nroff paragraph/section macros are boundaries too; a line containing
    // only spaces is not. Wrapped terminal rows are already joined here.
    push(starts, 0);
    for (long row = 0; row < d->lines; row++) {
        const uint32_t *line = d->text + d->starts[row];
        long length = line_length(d, row);
        bool previous = row && line_length(d, row - 1);
        bool boundary = !length || line[0] == '\f' || (line[0] == '.' && sentence_macro(line + 1, lmin(2, length - 1)));
        if (boundary && (row == 0 || previous || length))
            push(starts, d->starts[row]);
        if (length && (row == 0 || !previous))
            push(starts, indentation(d, row));
    }
    const uint32_t *text = d->text;
    for (long i = 0; i < d->length; i++) {
        if (text[i] != '.' && text[i] != '!' && text[i] != '?')
            continue;
        long end = i + 1;
        while (end < d->length && in_ascii(text[end], ")]\"'"))
            end++;
        if (end >= d->length || !in_ascii(text[end], " \t\n"))
            continue;
        i = end - 1; // Matches resume after this one.
        while (end < d->length && in_ascii(text[end], " \t\n")) {
            // Don't skip the paragraph boundary in front of a sentence.
            if (text[end] == '\n' && (end == 0 || text[end - 1] == '\n')) {
                push(starts, end);
                break;
            }
            end++;
        }
        if (end < d->length)
            push(starts, end);
    }
    sort_unique(starts);
    return starts;
}

static Longs *backward_sentence_starts(Document *d) {
    Longs *starts = &d->backward_starts;
    if (d->have_backward_starts)
        return starts;
    d->have_backward_starts = true;
    // Forward movement stops on the first empty line in a run; backward
    // movement reaches the last. From inside the run, either skips it.
    Longs *forward = sentence_starts(d);
    for (long i = 0; i < forward->n; i++) {
        long row = line_of(d, forward->v[i]);
        if (forward->v[i] != d->starts[row] || line_length(d, row))
            push(starts, forward->v[i]);
    }
    for (long row = 0; row < d->lines; row++)
        if (!line_length(d, row) && (row + 1 == d->lines || line_length(d, row + 1)))
            push(starts, d->starts[row]);
    push(starts, 0);
    sort_unique(starts);
    return starts;
}

static int sentence_target(Document *d, long position, char key, long count, long *target) {
    if (key == '(') {
        Longs *starts = backward_sentence_starts(d);
        long index = bisect(starts->v, starts->n, position, false) - count;
        if (index < 0 && !d->complete_start)
            return fail(INCOMPLETE, "Need earlier sentences");
        *target = starts->v[lmax(0, index)];
        return OK;
    }
    Longs *starts = sentence_starts(d);
    long index = bisect(starts->v, starts->n, position, true) + count - 1;
    if (index >= starts->n && !d->complete_end)
        return fail(INCOMPLETE, "Need more sentences");
    *target = index < starts->n ? starts->v[index] : d->length;
    return OK;
}

static int motion(Document *d, long position, const char *key, long count, const uint32_t *argument,
                  long argument_length, bool explicit, Span *result) {
    const uint32_t *text = d->text;
    long row = line_of(d, position), start = d->starts[row], end = line_end(d, row), last = d->lines - 1;
    if (is(key, "y")) {
        *result = line_span(d, row, lmin(last, row + count - 1), position);
        return OK;
    }
    if (one_of(key, "jkG") || is(key, "gg")) {
        long target;
        if (!one_of(key, "jk"))
            target = explicit ? lmin(last, count - 1) : is(key, "G") ? last : 0;
        else
            target = lmax(0, lmin(last, row + (is(key, "j") ? count : -count)));
        if (target == row && one_of(key, "jk"))
            return fail(NO_MOTION, "No more lines");
        *result = line_span(d, row, target, -1);
        return OK;
    }
    if (is(key, "0")) {
        *result = span_of(start, position);
        return OK;
    }
    if (is(key, "^")) {
        *result = exclusive(d, position, indentation(d, row));
        return OK;
    }
    if (is(key, "$")) {
        *result = span_of(position, line_end(d, lmin(last, row + count - 1)));
        return OK;
    }
    if (is(key, "h")) {
        long target = position;
        for (long n = lmin(count, position - start); n > 0; n--) {
            target--;
            while (target > start && cell_width(text[target]) == 0)
                target--;
            if (target == start)
                break;
        }
        *result = span_of(target, position);
        return OK;
    }
    if (is(key, "l")) {
        long target = position;
        for (long n = lmin(count, end - position); n > 0; n--) {
            target++;
            while (target < end && cell_width(text[target]) == 0)
                target++;
            if (target == end)
                break;
        }
        *result = span_of(position, target);
        return OK;
    }
    if (is(key, "ge") || is(key, "gE")) {
        long target;
        TRY(previous_word_end(d, position, count, is(key, "gE"), &target));
        if (target == position)
            return fail(NO_MOTION, "No previous word end");
        *result = span_of(target, position + 1);
        return OK;
    }
    if (one_of(key, "wWbBeE")) {
        bool big = key[0] >= 'A' && key[0] <= 'Z';
        char lower = key[0] | 32;
        Pairs *all = units(d, big, lower != 'e');
        if (!all->n)
            return fail(NO_MOTION, "No words");
        if (lower == 'b') {
            long index = bisect_units(all, position, false, false) - count;
            if (index < 0 && !d->complete_start)
                return fail(INCOMPLETE, "Need earlier words");
            *result = exclusive(d, position, all->v[lmax(0, index)].start);
            return OK;
        }
        if (lower == 'e') {
            long index = bisect_units(all, position, true, true) + count - 1;
            if (index >= all->n && !d->complete_end)
                return fail(INCOMPLETE, "Need more words");
            *result = span_of(position, lmax(position, all->v[lmin(all->n - 1, index)].end));
            return OK;
        }
        long index = bisect_units(all, position, true, false) + count - 1;
        long target = index < all->n ? all->v[index].start : d->length - 1;
        long previous = count == 1 ? position : all->v[lmin(all->n - 1, index - 1)].start;
        if (index >= all->n && !d->complete_end && (index > all->n || line_of(d, previous) == last))
            return fail(INCOMPLETE, "Need more words");
        // An operator followed by w stops at the end of its final line,
        // even when the next word is indented on a subsequent line.
        long previous_row = line_of(d, previous);
        if (line_of(d, target) > previous_row && !all_space(d, previous, line_end(d, previous_row)))
            target = line_end(d, previous_row);
        if (index >= all->n)
            target = lmax(position, line_end(d, last));
        *result = exclusive(d, position, target);
        return OK;
    }
    if (one_of(key, "fFtT")) {
        if (!argument || argument_length != 1)
            return fail(NO_MOTION, "Expected one character");
        long target = position;
        for (long n = 0; n < count; n++) {
            long found = -1;
            if (one_of(key, "ft")) {
                for (long i = target + 1; i < end && found < 0; i++)
                    if (text[i] == argument[0])
                        found = i;
            } else {
                for (long i = target - 1; i >= start && found < 0; i--)
                    if (text[i] == argument[0])
                        found = i;
            }
            if (found < 0)
                return fail(NO_MOTION, "Character not found");
            target = found;
        }
        if (is(key, "f"))
            *result = span_of(position, target + 1);
        else if (is(key, "t"))
            *result = span_of(position, target);
        else
            *result = span_of(target + is(key, "T"), position + 1);
        return OK;
    }
    if (is(key, "%")) {
        long bracket = -1;
        for (long i = position; i < end && bracket < 0; i++)
            if (in_ascii(text[i], "()[]{}"))
                bracket = i;
        if (bracket < 0)
            return fail(NO_MOTION, "No bracket on this line");
        Pairs pairs = brackets(d, text[bracket], false, 0, 1);
        for (long i = 0; i < pairs.n; i++) {
            Pair p = pairs.v[i];
            if (bracket == p.start || bracket == p.end) {
                long target = bracket == p.start ? p.end : p.start;
                *result = span_of(lmin(position, target), lmax(position, target) + 1);
                return OK;
            }
        }
        // Its partner may lie outside a fragment of the document.
        return fail(d->complete_start && d->complete_end ? NO_MOTION : INCOMPLETE, "Unmatched bracket");
    }
    if (one_of(key, "()")) {
        long target;
        TRY(sentence_target(d, position, key[0], count, &target));
        if (target == d->length)
            target = line_end(d, last);
        *result = exclusive(d, position, target);
        return OK;
    }
    if (one_of(key, "{}")) {
        long target;
        TRY(paragraph_target(d, position, key[0], count, &target));
        if (key[0] == '}' && line_of(d, target) == last && line_length(d, last))
            *result = span_of(position, line_end(d, last));
        else
            *result = exclusive(d, position, target);
        return OK;
    }
    return fail(NO_MOTION, CAT("Unsupported yank motion: ", key));
}

static int word_object(Document *d, long position, bool big, bool around, long count, Span *result) {
    const uint32_t *text = d->text;
    long row = line_of(d, position);
    if (!line_length(d, row))
        return fail(NO_MOTION, "Empty line");
    position = lmin(position, line_end(d, row) - 1);
    int group = kind(text[position], big);
    long start = position, end = position + 1;
    while (start > d->starts[row] && kind(text[start - 1], big) == group)
        start--;
    while (end < line_end(d, row) && kind(text[end], big) == group)
        end++;
    if (!around) {
        // Inner counts include intervening whitespace as its own object:
        // 2iw on "one two" selects "one ", while 2aw selects two words.
        for (long n = count - 1; n > 0; n--) {
            if (end >= d->length - 1)
                return fail(NO_MOTION, "Not enough words");
            int next = kind(text[end], big);
            end++;
            while (end < d->length - 1 && kind(text[end], big) == next)
                end++;
        }
        *result = span_of(start, end);
        return OK;
    }
    long remaining = count - 1 + (group == 0);
    if (remaining) {
        Pairs *all = units(d, big, true);
        for (long i = 0; i < all->n && remaining; i++) {
            if (all->v[i].start >= end) {
                end = all->v[i].end;
                remaining--;
            }
        }
    }
    if (remaining)
        return fail(NO_MOTION, "Not enough words");
    if (group) {
        long original = end;
        while (end < line_end(d, line_of(d, end - 1)) && is_space(text[end]))
            end++;
        // Indentation belongs to the line, rather than the first word.
        if (end == original && start > indentation(d, row))
            while (start > d->starts[row] && is_space(text[start - 1]))
                start--;
    }
    *result = span_of(start, end);
    return OK;
}

static int paragraph_object(Document *d, long position, bool around, long count, Span *result) {
    long first = line_of(d, position), last = first;
    bool empty = blank_line(d, first);
    while (first && blank_line(d, first - 1) == empty)
        first--;
    while (last + 1 < d->lines && blank_line(d, last + 1) == empty)
        last++;
    for (long n = count - 1 + (around && empty); n > 0; n--) {
        if (last + 1 >= d->lines)
            return fail(NO_MOTION, "Not enough paragraphs");
        last++;
        while (last + 1 < d->lines && blank_line(d, last))
            last++;
        while (last + 1 < d->lines && !blank_line(d, last + 1))
            last++;
    }
    if (around && !empty) {
        long old_last = last;
        while (last + 1 < d->lines && blank_line(d, last + 1))
            last++;
        if (old_last == last)
            while (first && blank_line(d, first - 1))
                first--;
    }
    *result = line_span(d, first, last, -1);
    return OK;
}

/* Sentences end at [.!?] and closing punctuation before whitespace, before
 * an empty line, or at the end of the text. */
static Pairs *sentences(Document *d) {
    Pairs *result = &d->sentences;
    if (d->have_sentences)
        return result;
    d->have_sentences = true;
    const uint32_t *text = d->text;
    long length = d->length;
    for (long search = 0;;) {
        while (search < length && is_space(text[search]))
            search++;
        if (search >= length)
            break;
        long start = search, end = length;
        for (long q = start + 1; q < length; q++) {
            if (in_ascii(text[q], ".!?")) {
                long after = q + 1;
                while (after < length && in_ascii(text[after], "\"')]"))
                    after++;
                if (after == length || is_space(text[after])) {
                    end = after;
                    break;
                }
            }
            if (text[q] == '\n') {
                long after = q + 1;
                while (after < length && is_space(text[after]) && text[after] != '\n')
                    after++;
                if (after < length && text[after] == '\n') {
                    end = q;
                    break;
                }
            }
        }
        search = end;
        while (end > start && is_space(text[end - 1]))
            end--;
        if (end + 1 < length && text[end] == '\n' && text[end + 1] == '\n')
            end++;
        push(result, (Pair){start, end});
    }
    return result;
}

static bool ascii_at(const Document *d, long position, const char *literal) {
    for (long i = 0; literal[i]; i++)
        if (position + i >= d->length || d->text[position + i] != (uint32_t)(unsigned char)literal[i])
            return false;
    return true;
}

static long find_ascii(const Document *d, long from, const char *literal) {
    for (long i = from; i < d->length; i++)
        if (ascii_at(d, i, literal))
            return i;
    return -1;
}

static bool names_match(const Document *d, Pair a, Pair b) {
    if (a.end - a.start != b.end - b.start)
        return false;
    for (long i = 0; i < a.end - a.start; i++)
        if (ascii_lower(d->text[a.start + i]) != ascii_lower(d->text[b.start + i]))
            return false;
    return true;
}

static bool void_element(const Document *d, Pair name) {
    static const char *const names[] = {"area", "base", "br", "col", "embed", "hr", "img", "input",
                                        "link", "meta", "param", "source", "track", "wbr"};
    for (long i = 0; i < LENGTH(names); i++) {
        long length = strlen(names[i]), k = 0;
        while (k < length && k < name.end - name.start && ascii_lower(d->text[name.start + k]) == (uint32_t)names[i][k])
            k++;
        if (k == length && k == name.end - name.start)
            return true;
    }
    return false;
}

/* Match an element tag at a '<', returning its end, or -1. Comments, CDATA,
 * processing instructions and declarations match with an empty name. */
static long markup_at(const Document *d, long i, Pair *name, bool *closing) {
    const uint32_t *text = d->text;
    long length = d->length, found;
    *name = (Pair){0, 0};
    if (ascii_at(d, i, "<!--") && (found = find_ascii(d, i + 4, "-->")) >= 0)
        return found + 3;
    if (ascii_at(d, i, "<![CDATA[") && (found = find_ascii(d, i + 9, "]]>")) >= 0)
        return found + 3;
    if (ascii_at(d, i, "<?") && (found = find_ascii(d, i + 2, "?>")) >= 0)
        return found + 2;
    if (ascii_at(d, i, "<!") && (found = find_ascii(d, i + 2, ">")) >= 0)
        return found + 1;
    long start = i + 1;
    *closing = start < length && text[start] == '/';
    start += *closing;
    if (start >= length || !is_ascii_alpha(text[start]))
        return -1;
    long end = start + 1;
    while (end < length && (is_word(text[end]) || in_ascii(text[end], ":.-")))
        end++;
    // The name needs a word boundary after it; backtrack to the last one.
    while (end > start && is_word(text[end - 1]) == (end < length && is_word(text[end])))
        end--;
    if (end == start)
        return -1;
    *name = (Pair){start, end};
    for (long k = end; k < length; k++) {
        if (text[k] == '>')
            return k + 1;
        if (text[k] == '"' || text[k] == '\'') {
            long close = k + 1;
            while (close < length && text[close] != text[k])
                close++;
            if (close >= length)
                return -1;
            k = close;
        }
    }
    return -1;
}

static Tags *tags(Document *d) {
    Tags *pairs = &d->tags;
    if (d->have_tags)
        return pairs;
    d->have_tags = true;
    typedef struct { Pair name; long start, end; } Open;
    struct { Open *v; long n, cap; } stack = {0};
    for (long i = 0; i < d->length;) {
        Pair name;
        bool closing = false;
        long end = d->text[i] == '<' ? markup_at(d, i, &name, &closing) : -1;
        if (end < 0) {
            i++;
            continue;
        }
        if (name.end > name.start) {
            if (closing) {
                long found = stack.n - 1;
                while (found >= 0 && !names_match(d, stack.v[found].name, name))
                    found--;
                if (found >= 0) {
                    Open opening = stack.v[found];
                    push(pairs, (Tag){opening.start, end - 1, opening.end, i});
                    stack.n = found;
                }
            } else if (d->text[end - 2] != '/' && !void_element(d, name)) {
                push(&stack, (Open){name, i, end});
            }
        }
        i = end;
    }
    return pairs;
}

static int text_object(Document *d, long position, const char *key, bool around, long count, Span *result) {
    const uint32_t *text = d->text;
    if (one_of(key, "wW"))
        return word_object(d, position, is(key, "W"), around, count, result);
    if (is(key, "p"))
        return paragraph_object(d, position, around, count, result);
    if (is(key, "s")) {
        Pairs *all = sentences(d);
        long index = 0;
        while (index < all->n && !(position < all->v[index].end))
            index++;
        if (index == all->n)
            return fail(NO_MOTION, "No sentence");
        if (!around) {
            Pairs segments = {0};
            for (long i = 0; i < all->n; i++) {
                push(&segments, all->v[i]);
                if (i + 1 < all->n && all->v[i].end < all->v[i + 1].start)
                    push(&segments, (Pair){all->v[i].end, all->v[i + 1].start});
            }
            long segment = 0;
            while (!(position < segments.v[segment].end))
                segment++;
            if (segment + count > segments.n && !d->complete_end)
                return fail(INCOMPLETE, "Need more sentences");
            *result = span_of(segments.v[segment].start, segments.v[lmin(segments.n - 1, segment + count - 1)].end);
            return OK;
        }
        if (index + count > all->n && !d->complete_end)
            return fail(INCOMPLETE, "Need more sentences");
        long start = all->v[index].start, end = all->v[lmin(all->n - 1, index + count - 1)].end, old_end = end;
        while (end < d->length - 1 && is_space(text[end]) && text[end - 1] != '\n')
            end++;
        if (old_end == end)
            while (start && is_space(text[start - 1]))
                start--;
        *result = span_of(start, end);
        return OK;
    }
    if (one_of(key, "bB(){}[]<>")) {
        Pairs pairs = brackets(d, key[0], true, position, count);
        Pair p;
        bool following;
        TRY(enclosing(&pairs, position, count, &p, &following));
        // The first line of a fragment may continue a line whose earlier
        // quote hides this bracket, so it cannot open a proven pair.
        if (!d->complete_start && line_of(d, p.start) == 0)
            return fail(INCOMPLETE, "Need the text before the bracket");
        *result = around ? span_of(p.start, p.end + 1) : span_of(p.start + 1, p.end);
        result->anywhere = following;
        if (around)
            return OK;
        if (one_of(key, "B{}") && text[p.start + 1] == '\n') {
            result->start++;
            // An inner brace block with a closing brace on its own line
            // is a linewise object, without either delimiter line.
            long closing_line = line_of(d, p.end);
            if (blank(d, d->starts[closing_line], p.end)) {
                result->end = d->starts[closing_line];
                result->linewise = true;
            }
        }
        return OK;
    }
    if (one_of(key, "q'\"`")) {
        long row = line_of(d, position), start = d->starts[row], end = line_end(d, row);
        const char *delimiters = is(key, "q") ? "'\"`" : key;
        Pairs pairs = {0};
        for (const char *delimiter = delimiters; *delimiter; delimiter++) {
            Longs quotes = {0};
            for (long i = start; i < end; i++) {
                if (text[i] != (uint32_t)*delimiter || escaped(d, i))
                    continue;
                // Apostrophes in don't/it's are not quote delimiters.
                if (is(key, "q") && *delimiter == '\'' && i && i + 1 < d->length && is_alnum(text[i - 1]) &&
                    is_alnum(text[i + 1]))
                    continue;
                push(&quotes, i);
            }
            for (long i = 0; i + 1 < quotes.n; i += 2)
                push(&pairs, (Pair){quotes.v[i], quotes.v[i + 1]});
        }
        Pair p;
        TRY(enclosing(&pairs, position, 1, &p, NULL));
        if (around) {
            long last = p.end + 1, first = p.start;
            while (last < end && is_space(text[last]))
                last++;
            if (last == p.end + 1)
                while (first > start && is_space(text[first - 1]))
                    first--;
            *result = span_of(first, last);
            return OK;
        }
        // Vim treats a count greater than one on a quote object as
        // including the delimiters, rather than looking for nesting.
        *result = count > 1 ? span_of(p.start, p.end + 1) : span_of(p.start + 1, p.end);
        return OK;
    }
    if (is(key, "t")) {
        Tags *all = tags(d);
        Pairs pairs = {0};
        for (long i = 0; i < all->n; i++)
            push(&pairs, (Pair){all->v[i].left, all->v[i].right});
        Pair p;
        TRY(enclosing(&pairs, position, count, &p, NULL));
        long i = 0;
        while (all->v[i].left != p.start || all->v[i].right != p.end)
            i++;
        *result = around ? span_of(p.start, p.end + 1) : span_of(all->v[i].inner_start, all->v[i].inner_end);
        return OK;
    }
    return fail(NO_MOTION, CAT("Unsupported text object: ", key));
}

/* ---------------------------------------------------------------------- */
/* Links */

static bool ascii_prefix(const uint32_t *text, long length, long i, const char *prefix) {
    for (long k = 0; prefix[k]; k++)
        if (i + k >= length || ascii_lower(text[i + k]) != (uint32_t)prefix[k])
            return false;
    return true;
}

/* The end of a URL that starts at i, or -1. */
static long link_match(const uint32_t *text, long length, long i) {
    if (!is_ascii_alpha(text[i]) || (i && is_word(text[i - 1])))
        return -1;
    long scheme = -1, k = i + 1;
    while (k < length && (is_ascii_alpha(text[k]) || (text[k] >= '0' && text[k] <= '9') || in_ascii(text[k], "+.-")))
        k++;
    if (ascii_prefix(text, length, k, "://"))
        scheme = k + 3;
    long prefixes[] = {scheme, ascii_prefix(text, length, i, "mailto:") ? i + 7 : -1,
                       ascii_prefix(text, length, i, "www.") ? i + 4 : -1};
    for (int n = 0; n < 3; n++) {
        long end = prefixes[n];
        if (end < 0)
            continue;
        while (end < length && !is_space(text[end]) && !in_ascii(text[end], "<>\"`"))
            end++;
        if (end > prefixes[n])
            return end;
    }
    return -1;
}

static long occurrences(const uint32_t *text, long length, uint32_t c) {
    long count = 0;
    for (long i = 0; i < length; i++)
        count += text[i] == c;
    return count;
}

/* Find the URL containing the cursor, excluding surrounding prose markup. */
static int link_at(const uint32_t *text, long length, long position, char **link) {
    static const char brackets[][2] = {{'(', ')'}, {'[', ']'}, {'{', '}'}};
    for (long i = 0; i < length;) {
        long end = link_match(text, length, i);
        if (end < 0) {
            i++;
            continue;
        }
        const uint32_t *found = text + i;
        long size = end - i;
        while (size) {
            long previous = size;
            while (size && in_ascii(found[size - 1], ".,;:!?"))
                size--;
            if (i && text[i - 1] == '\'' && size && found[size - 1] == '\'')
                size--;
            for (int k = 0; k < 3; k++)
                if (size && found[size - 1] == (uint32_t)brackets[k][1] &&
                    occurrences(found, size, brackets[k][1]) > occurrences(found, size, brackets[k][0]))
                    size--;
            if (size == previous)
                break;
        }
        if (i <= position && position < i + size) {
            Buf buffer = {0};
            if (ascii_prefix(found, size, 0, "www."))
                put(&buffer, "https://");
            for (long k = 0; k < size; k++)
                put_utf8(&buffer, found[k]);
            *link = text_of(&buffer);
            return OK;
        }
        i = end;
    }
    return fail(NO_MOTION, "No link under cursor");
}

/* ---------------------------------------------------------------------- */
/* Layout: translate character offsets to tmux rows and terminal cell columns. */

typedef struct Layout {
    Document *document;
    long first_row, width;
    Pairs rows;
} Layout;

/* Cells a character covers at a column. tmux keeps a tab as one character
 * reaching the next stop, every eight cells, short of the last column;
 * cursor-right crosses it in one step. */
static int cells_at(uint32_t c, long column, long width) {
    if (c != '\t')
        return cell_width(c);
    return (int)lmax(1, lmin((column / 8 + 1) * 8, width - 1) - column);
}

static Layout *layout_of(Document *d, long width, long first_row) {
    Layout *layout = alloc(sizeof *layout);
    layout->document = d;
    layout->first_row = first_row;
    layout->width = width = lmax(1, width);
    for (long number = 0; number < d->lines; number++) {
        long start = d->starts[number], end = line_end(d, number), cells = 0, column = 0;
        for (long i = start; i < end && cells <= width; i++)
            cells += cells_at(d->text[i], cells, width);
        if (cells <= width) {
            push(&layout->rows, (Pair){start, end});
            continue;
        }
        for (long i = start; i < end; i++) {
            int size = cells_at(d->text[i], column, width);
            if (size && column + size > width) {
                push(&layout->rows, (Pair){start, i});
                start = i;
                column = 0;
            }
            column += size;
        }
        push(&layout->rows, (Pair){start, end});
    }
    return layout;
}

static long layout_offset(const Layout *layout, long row, long column) {
    Pair r = layout->rows.v[lmax(0, lmin(row - layout->first_row, layout->rows.n - 1))];
    long cell = 0;
    for (long i = r.start; i < r.end; i++) {
        int size = cells_at(layout->document->text[i], cell, layout->width);
        if (size && cell >= column)
            return i;
        cell += size;
    }
    return r.end;
}

static void layout_point(const Layout *layout, long offset, long *row, long *column) {
    long low = 0, high = layout->rows.n;
    while (low < high) {
        long middle = low + (high - low) / 2;
        if (layout->rows.v[middle].start <= offset)
            low = middle + 1;
        else
            high = middle;
    }
    long index = lmin(layout->rows.n - 1, low - 1);
    if (index < 0)
        index = layout->rows.n - 1;
    *column = 0;
    for (long i = layout->rows.v[index].start; i < offset; i++)
        *column += cells_at(layout->document->text[i], *column, layout->width);
    *row = index + layout->first_row;
}

static long layout_steps(const Layout *layout, long row, long column, bool virtual) {
    long end = layout_offset(layout, row, column), steps = 0, cells = 0;
    Pair r = layout->rows.v[lmax(0, lmin(row - layout->first_row, layout->rows.n - 1))];
    for (long i = r.start; i < end; i++) {
        int size = cells_at(layout->document->text[i], cells, layout->width);
        steps += size > 0;
        cells += size;
    }
    if (virtual)
        steps += lmax(0, column - cells);
    return steps;
}

/* ---------------------------------------------------------------------- */
/* tmux */

#define SELECTION_SIGNATURE \
    "#{selection_start_x}:#{selection_start_y}:#{selection_end_x}:#{selection_end_y}:#{rectangle_toggle}"
#define FLASH_SELECTION \
    "#{&&:#{@vim-copy-flash},#{&&:#{==:#{selection_active},0},#{==:" SELECTION_SIGNATURE \
    ",#{@vim-copy-flash-selection}}}}"
#define VISUAL_SELECTION "#{&&:#{||:#{selection_present},#{selection_active}},#{!:" FLASH_SELECTION "}}"
#define SAVED(name) "\"" name "\":\"#{" name "}\","
#define SAVED_SELECTION \
    "{" SAVED("selection_start_x") SAVED("selection_start_y") SAVED("selection_end_x") \
    SAVED("selection_end_y") SAVED("rectangle_toggle") SAVED("selection_mode") SAVED("copy_cursor_x") \
    SAVED("copy_cursor_y") SAVED("scroll_position") SAVED("copy_position_limit") SAVED("pane_height") \
    SAVED("pane_width") SAVED("copy_line_numbers") \
    "\"numbering\":\"#{copy-mode-line-numbers}\",\"frame\":\"#{@vim-copy-frame}\"}"
#define COPY_FRAME "#{history_generation}:#{history_collected}"
#define SENTENCE_FRAME "#{@vim-copy-frame}:#{@vim-copy-epoch}:#{copy_position_limit}:#{pane_width}:#{pane_height}"
#define STATE_FORMAT \
    "#{copy_cursor_x}\t#{copy_cursor_y}\t#{scroll_position}\t#{copy_position_limit}\t#{pane_height}\t" \
    "#{pane_width}\t#{selection_present}\t#{selection_active}\t#{selection_start_x}\t#{selection_start_y}\t" \
    "#{selection_end_x}\t#{selection_end_y}\t#{rectangle_toggle}\t#{copy_line_numbers}\t#{selection_mode}\t" \
    "#{pane_mode}\t#{copy-mode-line-numbers}\t#{@vim-copy-highlight-ms}"
/* The state's history and cursor row, as tmux formats. */
#define NUMBERED_FORMAT "#{&&:#{copy_line_numbers},#{m/r:^(absolute|relative|hybrid)$,#{copy-mode-line-numbers}}}"
#define HISTORY_FORMAT "#{e|-|:#{copy_position_limit},#{?" NUMBERED_FORMAT ",#{pane_height},0}}"
#define CURSOR_FORMAT "#{e|-|:#{copy_cursor_y},#{scroll_position}}"

static const char *const SAVED_FIELDS[] = {
    "selection_start_x", "selection_start_y", "selection_end_x", "selection_end_y", "rectangle_toggle",
    "selection_mode", "copy_cursor_x", "copy_cursor_y", "scroll_position", "copy_position_limit",
    "pane_height", "pane_width", "copy_line_numbers"};

typedef struct {
    long copy_cursor_x, copy_cursor_y, scroll_position, copy_position_limit, pane_height, pane_width,
        selection_present, selection_active, selection_start_x, selection_start_y, selection_end_x,
        selection_end_y, rectangle_toggle, copy_line_numbers;
    const char *selection_mode, *pane_mode, *numbering, *highlight_ms;
    bool absolute_line_numbers;
    long history, row;
    Layout *layout;
} State;

typedef struct { const char **v; long n, cap; } Command;

/* Where a batch of commands leaves the copy cursor: on a character of row,
 * that many cursor-right steps from the row's start; and the first row the
 * viewport shows. */
typedef struct {
    long row, steps;
    bool known;
    long top;
    bool shown;
} Cursor;

typedef struct {
    Command *v;
    long n, cap;
    Cursor at;
} Commands;

/* Rows read along with the state, so most operations start one tmux client
 * fewer: starting a client costs about as much as the rest of a yank. */
typedef struct {
    Document *document;
    Layout *layout;
    long first, last;
} Prefetch;

typedef struct {
    const char *socket, *pane, *client;
    Prefetch prefetch;
    long first_offset; /* Offset of the latest snapshot inside the prefetch. */
} Tmux;

static Command command(const char *first, ...) {
    Command c = {0};
    va_list arguments;
    va_start(arguments, first);
    for (const char *argument = first; argument; argument = va_arg(arguments, const char *))
        push(&c, argument);
    va_end(arguments);
    return c;
}
#define CMD(...) command(__VA_ARGS__, (const char *)NULL)

static Command x(const Tmux *t, const char *name) { return CMD("send-keys", "-t", t->pane, "-X", name); }

/* Quote a tmux argument; this is not shell quoting. */
static char *quote(const char *value) {
    Buf buffer = {0};
    putch(&buffer, '"');
    for (const char *c = value; *c; c++) {
        if (*c == '\\' || *c == '"' || *c == '$')
            putch(&buffer, '\\');
        putch(&buffer, *c);
    }
    putch(&buffer, '"');
    return text_of(&buffer);
}

static char *shell_quote(const char *value) {
    bool safe = *value;
    for (const char *c = value; *c && safe; c++)
        safe = (*c >= 'a' && *c <= 'z') || (*c >= 'A' && *c <= 'Z') || (*c >= '0' && *c <= '9') ||
               strchr("_@%+=:,./-", *c);
    if (safe)
        return (char *)value;
    Buf buffer = {0};
    putch(&buffer, '\'');
    for (const char *c = value; *c; c++) {
        if (*c == '\'')
            put(&buffer, "'\"'\"'");
        else
            putch(&buffer, *c);
    }
    putch(&buffer, '\'');
    return text_of(&buffer);
}

static char *shell_join(const Command *c) {
    Buf buffer = {0};
    for (long i = 0; i < c->n; i++) {
        if (i)
            putch(&buffer, ' ');
        put(&buffer, shell_quote(c->v[i]));
    }
    return text_of(&buffer);
}

/* tmux expands formats in run-shell, including text in shell quotes. */
static char *escape_formats(const char *text) {
    Buf buffer = {0};
    for (const char *c = text; *c; c++) {
        if (*c == '#')
            putch(&buffer, '#');
        putch(&buffer, *c);
    }
    return text_of(&buffer);
}

/* The arguments of a command as one tmux command string. */
static char *command_string(const Command *c) {
    Buf buffer = {0};
    for (long i = 0; i < c->n; i++) {
        if (i)
            putch(&buffer, ' ');
        put(&buffer, quote(c->v[i]));
    }
    return text_of(&buffer);
}

static char *commands_string(const Commands *list) {
    Buf buffer = {0};
    for (long i = 0; i < list->n; i++) {
        if (i)
            put(&buffer, "; ");
        put(&buffer, command_string(&list->v[i]));
    }
    return text_of(&buffer);
}

static void append(Commands *list, const Commands *more) {
    for (long i = 0; i < more->n; i++)
        push(list, more->v[i]);
}

static void close_pipe(int *fd) {
    if (*fd >= 0)
        close(*fd);
    *fd = -1;
}

static long milliseconds(void) {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    return now.tv_sec * 1000L + now.tv_nsec / 1000000;
}

/* Run a program with input on stdin. Its stdout goes to *output if requested,
 * as a malloc'd string the caller may free, and otherwise to /dev/null, as
 * does stderr. Fails on a nonzero status. */
static int spawn(const char *const *argv, const char *input, char **output, long *output_length) {
    int in[2] = {-1, -1}, out[2] = {-1, -1};
    if ((input && pipe(in)) || (output && pipe(out))) {
        close_pipe(&in[0]);
        close_pipe(&in[1]);
        return fail(FAILED, "pipe");
    }
    posix_spawn_file_actions_t actions;
    posix_spawn_file_actions_init(&actions);
    if (input) {
        fcntl(in[1], F_SETFD, FD_CLOEXEC);
        posix_spawn_file_actions_adddup2(&actions, in[0], 0);
        if (in[0] != 0)
            posix_spawn_file_actions_addclose(&actions, in[0]);
    } else {
        posix_spawn_file_actions_addopen(&actions, 0, "/dev/null", O_RDONLY, 0);
    }
    if (output) {
        fcntl(out[0], F_SETFD, FD_CLOEXEC);
        posix_spawn_file_actions_adddup2(&actions, out[1], 1);
        if (out[1] != 1)
            posix_spawn_file_actions_addclose(&actions, out[1]);
    } else {
        posix_spawn_file_actions_addopen(&actions, 1, "/dev/null", O_WRONLY, 0);
    }
    posix_spawn_file_actions_addopen(&actions, 2, "/dev/null", O_WRONLY, 0);
    // This process ignores SIGPIPE; the child gets the default action.
    posix_spawnattr_t attributes;
    posix_spawnattr_init(&attributes);
    sigset_t defaults;
    sigemptyset(&defaults);
    sigaddset(&defaults, SIGPIPE);
    posix_spawnattr_setsigdefault(&attributes, &defaults);
    posix_spawnattr_setflags(&attributes, POSIX_SPAWN_SETSIGDEF);
    pid_t pid;
    int error = posix_spawnp(&pid, argv[0], &actions, &attributes, (char *const *)argv, environ);
    posix_spawn_file_actions_destroy(&actions);
    posix_spawnattr_destroy(&attributes);
    close_pipe(&in[0]);
    close_pipe(&out[1]);
    if (error) {
        close_pipe(&in[1]);
        close_pipe(&out[0]);
        return fail(FAILED, "spawn");
    }
    char *buffer = NULL;
    size_t used = 0, capacity = 0;
    long written = 0, input_length = input ? (long)strlen(input) : 0, deadline = milliseconds() + 10000;
    bool timed_out = false;
    if (input && !input_length)
        close_pipe(&in[1]);
    while (in[1] >= 0 || out[0] >= 0) {
        struct pollfd fds[2];
        int count = 0;
        if (in[1] >= 0)
            fds[count++] = (struct pollfd){.fd = in[1], .events = POLLOUT};
        if (out[0] >= 0)
            fds[count++] = (struct pollfd){.fd = out[0], .events = POLLIN};
        long remaining = deadline - milliseconds();
        if (remaining <= 0) {
            timed_out = true;
            kill(pid, SIGKILL);
            break;
        }
        if (poll(fds, count, remaining) < 0) {
            if (errno == EINTR)
                continue;
            timed_out = true;
            kill(pid, SIGKILL);
            break;
        }
        for (int i = 0; i < count; i++) {
            if (!fds[i].revents)
                continue;
            if (fds[i].fd == in[1]) {
                ssize_t n = write(in[1], input + written, input_length - written);
                if (n > 0)
                    written += n;
                if ((n < 0 && errno != EINTR && errno != EAGAIN) || written == input_length)
                    close_pipe(&in[1]);
            } else {
                // realloc grows large blocks in place rather than copying.
                if (capacity - used < 65537) {
                    capacity = capacity ? capacity * 2 : 65536 * 2;
                    buffer = realloc(buffer, capacity);
                    if (!buffer) {
                        fputs("vim-copy: out of memory\n", stderr);
                        exit(1);
                    }
                }
                ssize_t n = read(out[0], buffer + used, capacity - used - 1);
                if (n > 0)
                    used += n;
                else if (n == 0 || (errno != EINTR && errno != EAGAIN))
                    close_pipe(&out[0]);
            }
        }
    }
    close_pipe(&in[1]);
    close_pipe(&out[0]);
    int status;
    while (waitpid(pid, &status, 0) < 0 && errno == EINTR)
        ;
    if (output) {
        *output = buffer ? buffer : calloc(1, 1);
        (*output)[used] = 0;
        *output_length = used;
    }
    if (timed_out || !WIFEXITED(status) || WEXITSTATUS(status))
        return fail(FAILED, "tmux failed");
    return OK;
}

/* Run tmux commands through one client, separated as tmux expects. tmux
 * also splits an argument that ends in a semicolon, unless one backslash
 * precedes it, so data like a search for "a;" keeps its last character. */
static int run(const Tmux *t, const Commands *list, const char *input, char **output, long *output_length) {
    Command argv = {0};
    push(&argv, "tmux");
    if (t->socket) {
        push(&argv, "-S");
        push(&argv, t->socket);
    }
    for (long i = 0; i < list->n; i++) {
        if (i)
            push(&argv, ";");
        for (long k = 0; k < list->v[i].n; k++) {
            const char *argument = list->v[i].v[k];
            long length = strlen(argument);
            if (length && argument[length - 1] == ';')
                argument = format("%.*s\\;", (int)(length - 1), argument);
            push(&argv, argument);
        }
    }
    push(&argv, (const char *)NULL);
    return spawn(argv.v, input, output, output_length);
}

static int call(const Tmux *t, Command c, char **output) {
    Commands list = {0};
    long length;
    push(&list, c);
    return run(t, &list, NULL, output, &length);
}

static char *next_line(char **cursor) {
    char *line = *cursor, *newline = strchr(line, '\n');
    if (newline) {
        *newline = 0;
        *cursor = newline + 1;
    } else {
        *cursor = line + strlen(line);
    }
    return line;
}

static int parse_state(char *line, State *s) {
    char *fields[18];
    int count = 0;
    for (char *cursor = line; count < 18;) {
        fields[count++] = cursor;
        char *tab = strchr(cursor, '\t');
        if (!tab)
            break;
        *tab = 0;
        cursor = tab + 1;
    }
    while (count < 18)
        fields[count++] = "";
    *s = (State){0};
    s->selection_mode = fields[14];
    s->pane_mode = fields[15];
    s->numbering = fields[16];
    if (!is(s->pane_mode, "copy-mode"))
        return fail(NO_MOTION, "Enter tmux copy mode first");
    long *numbers[] = {&s->copy_cursor_x, &s->copy_cursor_y, &s->scroll_position, &s->copy_position_limit,
                       &s->pane_height, &s->pane_width, &s->selection_present, &s->selection_active,
                       &s->selection_start_x, &s->selection_start_y, &s->selection_end_x,
                       &s->selection_end_y, &s->rectangle_toggle, &s->copy_line_numbers};
    for (int i = 0; i < LENGTH(numbers); i++)
        if (*fields[i] && !parse_long(fields[i], numbers[i]))
            return fail(FAILED, "Unexpected tmux state");
    s->highlight_ms = *fields[17] ? fields[17] : "150";
    // A mouse entry or line-numbers-off can hide the gutter even when the
    // window option requests numbers. That also changes goto-line units.
    s->absolute_line_numbers = s->copy_line_numbers && (is(s->numbering, "absolute") ||
                                                        is(s->numbering, "relative") || is(s->numbering, "hybrid"));
    s->history = s->copy_position_limit - (s->absolute_line_numbers ? s->pane_height : 0);
    s->row = s->history - s->scroll_position + s->copy_cursor_y;
    return OK;
}

/* Read the state and extra formats, plus rows from before the cursor row to
 * after it (ALL for every row; before NONE for none), with one client. */
static int query(Tmux *t, State *s, long before, long after, int extras, const char *const *formats,
                 char **values) {
    Commands list = {0};
    push(&list, CMD("display-message", "-p", "-t", t->pane, STATE_FORMAT));
    for (int i = 0; i < extras; i++)
        push(&list, CMD("display-message", "-p", "-t", t->pane, formats[i]));
    if (before != NONE) {
        // capture-pane expands formats in -S and -E. Rows count from the
        // first screen row, so the cursor is at copy_cursor_y - scroll.
        const char *first = before == ALL ? "#{e|-|:0," HISTORY_FORMAT "}"
                                          : format("#{e|-|:" CURSOR_FORMAT ",%ld}", before);
        const char *last = before == ALL ? "#{e|-|:#{pane_height},1}"
                                         : format("#{e|+|:" CURSOR_FORMAT ",%ld}", after);
        push(&list, CMD("capture-pane", "-pMJ", "-t", t->pane, "-S", first, "-E", last));
    }
    char *output = NULL;
    long length = 0;
    int status = run(t, &list, NULL, &output, &length);
    if (!output)
        return status;
    char *cursor = output, *line = CAT(next_line(&cursor));
    if (status && !*line) {
        free(output);
        return status;
    }
    for (int i = 0; i < extras; i++)
        values[i] = status ? NULL : CAT(next_line(&cursor));
    int parsed = parse_state(line, s);
    if (!parsed && !status && before != NONE) {
        long limit = s->history + s->pane_height - 1;
        long first = before == ALL ? 0 : lmax(0, s->row - before);
        long last = before == ALL ? limit : lmin(limit, s->row + after);
        Document *d = document_from_bytes(cursor, output + length - cursor);
        Layout *layout = layout_of(d, s->pane_width, first);
        // Without a row per copy-mode row, the slower explicit capture
        // reports the problem when an operation actually needs the text.
        if (layout->rows.n == last - first + 1)
            t->prefetch = (Prefetch){d, layout, first, last};
    }
    free(output);
    return parsed ? parsed : status;
}

/* Whether a row and column are on one of that row's characters, which
 * cursor-right and cursor-left reach one step at a time without wrapping. */
static bool on_character(const State *s, long row, long column) {
    Layout *layout = s->layout;
    long index = layout ? row - layout->first_row : -1;
    return index >= 0 && index < layout->rows.n && layout_offset(layout, row, column) < layout->rows.v[index].end;
}

/* The state's cursor and viewport are where a new batch starts. */
static void locate(const State *s, Commands *list) {
    long steps = s->layout ? layout_steps(s->layout, s->row, s->copy_cursor_x, false) : 0;
    list->at = (Cursor){s->row, steps, on_character(s, s->row, s->copy_cursor_x), s->history - s->scroll_position, true};
}

/* The cursor steps a move takes from where a batch left the cursor, or -1
 * when it is unknown or too far and the move rebuilds the viewport. */
static long nearby_steps(const State *s, const Commands *list, long row, long column) {
    if (!list->at.known)
        return -1;
    long steps = s->layout ? layout_steps(s->layout, row, column, s->rectangle_toggle) : column;
    if (row == list->at.row)
        return labs(steps - list->at.steps);
    long rows = labs(row - list->at.row);
    return rows <= s->pane_height ? rows + 1 + steps : -1;
}

static void move(const Tmux *t, const State *s, Commands *out, long row, long column, long top, bool relative) {
    long steps = s->layout ? layout_steps(s->layout, row, column, s->rectangle_toggle) : column;
    if (!relative && top == NONE && nearby_steps(s, out, row, column) >= 0) {
        // Step from where the cursor already is. While selecting, tmux
        // redraws the selection after each step, so walking from the start
        // of a wide row would cost more than the rest of a yank.
        if (row == out->at.row) {
            long delta = steps - out->at.steps;
            if (delta)
                push(out, CMD("send-keys", "-t", t->pane, "-X", "-N", num(labs(delta)),
                              delta > 0 ? "cursor-right" : "cursor-left"));
        } else {
            // Like a jump, cursor-down and cursor-up scroll just enough to
            // keep the cursor visible.
            long rows = row - out->at.row;
            if (row < out->at.top)
                out->at.top = row;
            else if (row >= out->at.top + s->pane_height)
                out->at.top = row - s->pane_height + 1;
            push(out, CMD("send-keys", "-t", t->pane, "-X", "-N", num(labs(rows)), rows > 0 ? "cursor-down" : "cursor-up"));
            Command start = x(t, "start-of-line");
            push(out, CMD("if-shell", "-F", "-t", t->pane, "#{!=:#{copy_cursor_x},0}", command_string(&start)));
            if (steps)
                push(out, CMD("send-keys", "-t", t->pane, "-X", "-N", num(steps), "cursor-right"));
        }
        out->at.row = row;
        out->at.steps = steps;
        out->at.known = on_character(s, row, column);
        return;
    }
    Commands list = {0};
    long delta;
    if (relative) {
        // Move monotonically while extending a selection. Visiting a
        // screen anchor past the target can reverse tmux's line drag
        // direction and lose its original anchor on the next redraw.
        delta = row - s->row;
    } else {
        // Keep visible targets in their current viewport. Only scroll
        // enough to reveal a target that has moved beyond either edge.
        if (top == NONE)
            top = lmax(row - s->pane_height + 1, lmin(row, s->history - s->scroll_position));
        top = lmax(0, lmin(top, s->history));
        // Jump straight to the viewport. Walking through history redraws
        // every intermediate row can consume most of a 150 ms yank flash.
        long line = s->absolute_line_numbers ? top + 1 : s->history - top, screen_row = row - top;
        Command jump = x(t, "goto-line");
        push(&jump, num(line));
        push(&list, jump);
        long anchors[] = {0, (s->pane_height - 1) / 2, s->pane_height - 1};
        const char *motions[] = {"top-line", "middle-line", "bottom-line"};
        int nearest = 0;
        for (int i = 1; i < 3; i++)
            if (labs(anchors[i] - screen_row) < labs(anchors[nearest] - screen_row))
                nearest = i;
        push(&list, x(t, motions[nearest]));
        delta = screen_row - anchors[nearest];
    }
    if (delta)
        push(&list, CMD("send-keys", "-t", t->pane, "-X", "-N", num(labs(delta)), delta > 0 ? "cursor-down" : "cursor-up"));
    bool keep_row = false;
    if (s->layout) {
        Layout *layout = s->layout;
        Document *d = layout->document;
        long index = row - layout->first_row;
        if (index >= 0 && index < layout->rows.n) {
            Pair r = layout->rows.v[index];
            long logical = line_of(d, r.start);
            // start-of-line follows a soft wrap to its logical start.
            // Skip the row positioning only for a complete unwrapped line.
            keep_row = (layout->first_row == 0 || logical > 0) && r.start == d->starts[logical] &&
                       r.end == line_end(d, logical);
        }
    }
    if (!relative && keep_row) {
        // Most local yanks only change the column. Check the actual tmux
        // position when this batch executes: rebuilding the same viewport
        // for every selection endpoint needlessly redraws the whole pane.
        const char *changed = format("#{||:#{!=:#{scroll_position},%ld},#{!=:#{copy_cursor_y},%ld}}",
                                     s->history - top, row - top);
        Command wrapped = CMD("if-shell", "-F", "-t", t->pane, changed, commands_string(&list));
        list.n = 0;
        push(&list, wrapped);
    }
    // At column zero, tmux's start-of-line walks to the previous wrapped
    // row. Only reset the column when it is actually nonzero.
    Command start = x(t, "start-of-line");
    push(&list, CMD("if-shell", "-F", "-t", t->pane, "#{!=:#{copy_cursor_x},0}", command_string(&start)));
    if (steps)
        push(&list, CMD("send-keys", "-t", t->pane, "-X", "-N", num(steps), "cursor-right"));
    append(out, &list);
    out->at = (Cursor){row, steps, on_character(s, row, column), top, !relative};
}

static void restore(const Tmux *t, const State *s, Commands *list) {
    push(list, x(t, "clear-selection"));
    push(list, x(t, s->rectangle_toggle ? "rectangle-on" : "rectangle-off"));
    if (s->selection_present || s->selection_active) {
        bool linewise = is(s->selection_mode, "line");
        move(t, s, list, s->selection_start_y, linewise ? 0 : s->selection_start_x, NONE, false);
        push(list, x(t, "begin-selection"));
        // Line endpoints can be one cell beyond EOL. Moving cursor-right
        // to that column wraps into the next line; let selection-mode
        // expand the actual last line instead.
        move(t, s, list, s->selection_end_y, linewise ? 0 : s->selection_end_x, NONE, false);
        // other-end can leave the cursor dragging the original anchor.
        // Recreate that orientation before restoring its exact column.
        bool at_start = s->row == s->selection_start_y && (s->copy_cursor_x == s->selection_start_x || linewise);
        bool at_end = s->row == s->selection_end_y && (s->copy_cursor_x == s->selection_end_x || linewise);
        if (s->selection_active && at_start && !at_end)
            push(list, x(t, "other-end"));
        Command mode = x(t, "selection-mode");
        push(&mode, s->selection_mode);
        push(list, mode);
        list->at.known = false;
        if (!s->selection_active)
            push(list, x(t, "stop-selection"));
    }
    move(t, s, list, s->row, s->copy_cursor_x, s->history - s->scroll_position, false);
}

/* In tmux 3.8, -M exposes copy mode's frozen backing screen, including its
 * history. Exporting it through a temporary selection was both slow (full
 * redraws) and capable of changing a line selection. */
static int snapshot(Tmux *t, State *s, long first, long last, Document **result) {
    if (last == NONE)
        last = s->history + s->pane_height - 1;
    Prefetch *p = &t->prefetch;
    Document *d;
    Layout *layout = NULL;
    if (p->document && p->first <= first && last <= p->last) {
        Pair from = p->layout->rows.v[first - p->first], to = p->layout->rows.v[last - p->first];
        t->first_offset = from.start;
        if (first == p->first && last == p->last) {
            d = p->document;
            layout = p->layout;
        } else {
            // capture-pane emits a newline for the last physical row,
            // including an empty row or a truncated wrapped line.
            long length = to.end - from.start;
            uint32_t *text = alloc((length + 2) * sizeof *text);
            memcpy(text, p->document->text + from.start, length * sizeof *text);
            text[length] = '\n';
            d = document(text, length + 1);
        }
    } else {
        char *output;
        long length;
        Commands list = {0};
        push(&list, CMD("capture-pane", "-pMJ", "-t", t->pane, "-S", num(first - s->history), "-E",
                        num(last - s->history)));
        TRY(run(t, &list, NULL, &output, &length));
        d = document_from_bytes(output, length);
        free(output);
    }
    d->complete_start = d->complete_end = true;
    if (!layout)
        layout = layout_of(d, s->pane_width, first);
    if (layout->rows.n != last - first + 1)
        return fail(NO_MOTION, "Cannot map this terminal buffer to copy-mode rows");
    s->layout = layout;
    *result = d;
    return OK;
}

typedef struct {
    const char *mode, *motion, *object, *digits;
    long before;
    bool explicit;
} Context;

/* Read enough text to prove the result independent of the slice edges. */
static int operation_snapshot(Tmux *t, State *s, const Context *c, long count, const uint32_t *argument,
                              long argument_length, Document **document, Span *result) {
    const char *key = c->motion;
    bool object = c->object != NULL;
    // Tags and absolute line numbers can depend on context anywhere in the
    // document. Brackets grow their read until the pair is provably whole.
    bool global = object ? is(key, "t") : is(key, "G") || is(key, "gg");
    bool line_limited = object ? one_of(key, "q'\"`") : one_of(key, "fFtT0^hl");
    long first_row = s->row, last_row = s->row;
    if (object && one_of(key, "wWps") && s->selection_present) {
        first_row = lmin(first_row, lmin(s->selection_start_y, s->selection_end_y));
        last_row = lmax(last_row, lmax(s->selection_start_y, s->selection_end_y));
    }
    long limit = s->history + s->pane_height - 1;
    bool explicit = c->explicit || c->before != 1 || *c->digits;
    for (long distance = 4;; distance *= 2) {
        long first = global ? 0 : lmax(0, first_row - distance);
        long last = global ? limit : lmin(limit, last_row + distance);
        Document *d;
        TRY(snapshot(t, s, first, last, &d));
        d->complete_start = first == 0;
        d->complete_end = last == limit;
        long position = layout_offset(s->layout, s->row, s->copy_cursor_x), row = line_of(d, position);
        bool complete_line = (first == 0 || row > 0) && (last == limit || row < d->lines - 1);
        if (object && one_of(key, "wW") && !line_length(d, row) && complete_line)
            return fail(NO_MOTION, "Empty line");
        Span span;
        int status = object ? text_object(d, position, key, is(c->object, "a"), count, &span)
                            : motion(d, position, key, count, argument, argument_length, explicit, &span);
        bool whole = first == 0 && last == limit;
        if (status == NO_MOTION) {
            // % fails early only when the cursor's line has no bracket.
            if (whole || ((line_limited || is(key, "%")) && complete_line))
                return status;
        } else if (status == OK && (whole || !span.anywhere)) {
            // An exclusive motion may omit its destination line. Keep
            // two following lines as evidence that it did not merely
            // stop at the artificial end of the captured fragment. The
            // first line may continue from before the read, so a range
            // starting there could have parsed its quotes wrongly.
            if ((line_limited && complete_line) ||
                ((first == 0 || line_of(d, span.start) > 0) &&
                 (last == limit || line_of(d, lmax(span.start, span.end - 1)) < d->lines - 2))) {
                *document = d;
                *result = span;
                return OK;
            }
        } else if (status == FAILED) {
            return status;
        }
        // Only the whole document can confirm this kind of result, so read
        // it next rather than growing the read a few rows at a time.
        if (status == OK && span.anywhere)
            global = true;
    }
}

static int sentence_snapshot(Tmux *t, State *s, char key, long count, Document **document, long *target) {
    // Most jumps need only the neighbouring lines. Grow the read only
    // when the requested boundary lies outside that window, so ordinary
    // movement costs the same with 50 or 50,000 lines of scrollback.
    long limit = s->history + s->pane_height - 1;
    for (long distance = 32;; distance *= 2) {
        long first = lmax(0, s->row - distance), last = lmin(limit, s->row + distance);
        Document *d;
        TRY(snapshot(t, s, first, last, &d));
        long position = layout_offset(s->layout, s->row, s->copy_cursor_x);
        int status = key == '{' || key == '}' ? paragraph_target(d, position, key, count, target)
                                              : sentence_target(d, position, key, count, target);
        if (status) {
            if ((key == '}' && last < limit) || (key == '{' && first))
                continue;
            return status;
        }
        bool complete;
        if (key == '}')
            complete = line_of(d, *target) < d->lines - 1 || last == limit;
        else if (key == ')')
            complete = *target < d->length || last == limit;
        else
            // The first logical line may continue from before the read;
            // its apparent sentence start is not a confirmed boundary.
            complete = line_of(d, *target) > 0 || first == 0;
        if (complete) {
            *document = d;
            return OK;
        }
    }
}

static int word_end_snapshot(Tmux *t, State *s, long count, bool big, Document **document, long *target) {
    // A backward motion needs only the current row and enough preceding
    // rows to find its target, even with a large scrollback buffer.
    for (long distance = 4;; distance *= 2) {
        long first = lmax(0, s->row - distance);
        TRY(snapshot(t, s, first, s->row, document));
        long position = layout_offset(s->layout, s->row, s->copy_cursor_x);
        TRY(previous_word_end(*document, position, count, big, target));
        if (first == 0 || *target > 0)
            return OK;
    }
}

static void boundary_expression(Buf *out, const long *limits, const long *values, long left, long right,
                                const char *comparison) {
    if (left == right) {
        put(out, num(values[left]));
        return;
    }
    long middle = (left + right) / 2;
    put(out, format("#{?#{e|%s|:#{@vim-copy-sentence-position},%ld},", comparison, limits[middle]));
    boundary_expression(out, limits, values, left, middle, comparison);
    putch(out, ',');
    boundary_expression(out, limits, values, middle + 1, right, comparison);
    putch(out, '}');
}

static Command set_option(const Tmux *t, const char *name, const char *value) {
    return CMD("set-option", "-p", "-t", t->pane, name, value);
}

static Command set_expression(const Tmux *t, const char *name, const long *limits, const long *values,
                              long count, const char *comparison) {
    Buf buffer = {0};
    boundary_expression(&buffer, limits, values, 0, count, comparison);
    return set_option(t, name, text_of(&buffer));
}

static long code(const State *s, long offset) {
    long row, column;
    layout_point(s->layout, offset, &row, &column);
    return row * (s->pane_width + 1) + column;
}

static void sentence_expressions(const Tmux *t, const State *s, Longs groups[2], long lower, long upper,
                                 Commands *list) {
    static const char *const keys[] = {"next", "previous"};
    for (int k = 0; k < 2; k++) {
        long n = groups[k].n, *points = groups[k].v;
        long *limits = alloc(n * sizeof *limits), *destinations = alloc((n + 1) * sizeof *destinations);
        for (long i = 0; i < n; i++) {
            limits[i] = code(s, points[i]);
            destinations[i + (k == 1)] = points[i];
        }
        destinations[k == 0 ? n : 0] = points[k == 0 ? n - 1 : 0];
        long *rows = alloc((n + 1) * sizeof *rows), *steps = alloc((n + 1) * sizeof *steps);
        for (long i = 0; i <= n; i++) {
            long column;
            layout_point(s->layout, destinations[i], &rows[i], &column);
            steps[i] = layout_steps(s->layout, rows[i], column, false);
        }
        const char *comparison = k == 0 ? "<" : "<=";
        push(list, set_expression(t, CAT("@vim-copy-sentence-", keys[k], "-row"), limits, rows, n, comparison));
        push(list, set_expression(t, CAT("@vim-copy-sentence-", keys[k], "-steps"), limits, steps, n, comparison));
    }
    push(list, set_option(t, "@vim-copy-sentence-lower", num(code(s, lower))));
    push(list, set_option(t, "@vim-copy-sentence-upper", num(code(s, upper))));
}

static void paragraph_cache(const Tmux *t, const State *s, Document *d, Commands *list) {
    Layout *layout = s->layout;
    long first = lmax(layout->first_row, s->row - 16);
    long last = lmin(layout->first_row + layout->rows.n - 1, s->row + 16), width = s->pane_width + 1;
    bool at_end = layout->first_row + layout->rows.n == s->history + s->pane_height;
    static const char *const directions[] = {"paragraph-next", "paragraph-previous"};
    for (int k = 0; k < 2; k++) {
        char key = k ? '{' : '}';
        Longs rows = {0}, targets = {0}, steps = {0};
        long cached_line = -1, target = 0;
        for (long row = first; row <= last; row++) {
            long origin = layout_offset(layout, row, 0), logical = line_of(d, origin);
            if (logical != cached_line) {
                cached_line = logical;
                if (paragraph_target(d, origin, key, 1, &target)) {
                    cached_line = -1;
                    continue;
                }
            }
            long line = line_of(d, target), target_row, column;
            if ((key == '}' && line == d->lines - 1 && !at_end) || (key == '{' && line == 0 && layout->first_row))
                continue;
            layout_point(layout, target, &target_row, &column);
            push(&rows, row);
            push(&targets, target_row);
            push(&steps, layout_steps(layout, target_row, column, false));
        }
        long lower = rows.n ? rows.v[0] * width : 0, upper = rows.n ? (rows.v[rows.n - 1] + 1) * width : 0;
        push(list, set_option(t, CAT("@vim-copy-sentence-", directions[k], "-lower"), num(lower)));
        push(list, set_option(t, CAT("@vim-copy-sentence-", directions[k], "-upper"), num(upper)));
        if (rows.n) {
            long *limits = alloc(rows.n * sizeof *limits);
            for (long i = 1; i < rows.n; i++)
                limits[i - 1] = rows.v[i] * width;
            push(list, set_expression(t, CAT("@vim-copy-sentence-", directions[k], "-row"), limits, targets.v,
                                      rows.n - 1, "<"));
            push(list, set_expression(t, CAT("@vim-copy-sentence-", directions[k], "-steps"), limits, steps.v,
                                      rows.n - 1, "<"));
        }
    }
}

/* Cache nearby boundaries as balanced tmux format expressions. Plain ( and )
 * then use only native commands, without starting the helper, exporting a
 * buffer or changing the search register. Counts retain the helper. */
static void sentence_cache(const Tmux *t, const State *s, Document *d, Commands *list) {
    Layout *layout = s->layout;
    long position = layout_offset(layout, s->row, s->copy_cursor_x);
    Longs *sources[] = {sentence_starts(d), backward_sentence_starts(d)}, groups[2];
    for (int k = 0; k < 2; k++) {
        long middle = bisect(sources[k]->v, sources[k]->n, position, true);
        long from = lmax(0, middle - 17), to = lmin(sources[k]->n, middle + 17);
        groups[k] = (Longs){sources[k]->v + from, to - from, to - from};
    }
    long first = layout->first_row && d->lines > 1 ? d->starts[1] : 0;
    long last = layout->first_row + layout->rows.n == s->history + s->pane_height ? d->length : d->starts[d->lines - 1];
    // Cached answers hold from the first start of the forward group, and
    // only after the first start of the backward group: ( there needs a
    // start before the cache, so the helper must answer it.
    long lower = lmax(first, lmax(groups[0].v[0], groups[1].v[0] + 1));
    long upper = lmin(last, lmin(groups[0].v[groups[0].n - 1], groups[1].v[groups[1].n - 1]));
    if (lower >= upper) {
        push(list, set_option(t, "@vim-copy-sentence-lower", "0"));
        push(list, set_option(t, "@vim-copy-sentence-upper", "0"));
    } else {
        sentence_expressions(t, s, groups, lower, upper, list);
    }
    paragraph_cache(t, s, d, list);
    push(list, set_option(t, "@vim-copy-sentence-history", num(s->history)));
    push(list, CMD("set-option", "-pF", "-t", t->pane, "@vim-copy-sentence-frame", SENTENCE_FRAME));
}

static Command flash_cleanup(const Tmux *t, const char *token) {
    const char *pane = quote(t->pane);
    const char *clear = CAT("if-shell -F -t ", pane, " ",
                            quote("#{&&:#{==:#{pane_mode},copy-mode}," FLASH_SELECTION "}"),
                            " { send-keys -t ", pane, " -X clear-selection }; set-option -pu -t ", pane,
                            " @vim-copy-flash; set-option -pu -t ", pane, " @vim-copy-flash-selection; set-option -pu -t ",
                            pane, " copy-mode-selection-style; refresh-client -t ", quote(t->client));
    return CMD("if-shell", "-F", "-t", t->pane, CAT("#{==:#{@vim-copy-flash},", token, "}"), clear);
}

/* A yank highlight keeps the copied range selected, in its own style, until
 * a delayed command clears it. Navigation and new selections stay live. */
typedef struct {
    const char *token, *seconds; /* No token when highlights are off. */
} Flash;

static int prepare_flash(const char *duration, Flash *flash) {
    long ms;
    unsigned char random[16];
    *flash = (Flash){NULL, NULL};
    if (!parse_long(duration, &ms))
        return fail(FAILED, "Bad highlight duration");
    ms = lmax(0, lmin(5000, ms));
    if (!ms)
        return OK;
    if (getentropy(random, sizeof random))
        return fail(FAILED, "No random token");
    Buf token = {0};
    for (int i = 0; i < LENGTH(random); i++)
        put(&token, format("%02x", random[i]));
    // Seconds for run-shell -d, without trailing zeros.
    char *seconds = format("%ld.%03ld", ms / 1000, ms % 1000);
    for (long end = strlen(seconds) - 1; seconds[end] == '0' && seconds[end - 1] != '.'; end--)
        seconds[end] = 0;
    *flash = (Flash){text_of(&token), seconds};
    return OK;
}

/* Select in the highlight style from here on. A pane option, unlike a style
 * format, costs tmux nothing on each redraw of copy mode. Schedule the end
 * first, so nothing that fails later in the batch can leave the style set. */
static void start_flash(const Tmux *t, const Flash *flash, Commands *list) {
    push(list, CMD("set-option", "-pF", "-t", t->pane, "copy-mode-selection-style", "#{@vim-copy-highlight-style}"));
    push(list, set_option(t, "@vim-copy-flash", flash->token));
    // run-shell -C runs a tmux command, so no process starts when the timer
    // fires. Formats stay literal until then, when the token is checked. A
    // pane or client that has gone by then only logs a tmux message.
    Command cleanup = flash_cleanup(t, flash->token);
    push(list, CMD("run-shell", "-b", "-d", flash->seconds, "-C", "-t", t->pane,
                   escape_formats(command_string(&cleanup))));
}

/* Set the buffer and clipboard. load-buffer reads stdin through the client,
 * one more round trip, so small text goes as an argument; tmux limits how
 * long a command can be. *input becomes NULL unless the text needs stdin. */
static Command write_buffer(const Tmux *t, const char **input) {
    const char *text = *input;
    if (strlen(text) > 8192)
        return CMD("load-buffer", "-w", "-t", t->client, "-");
    *input = NULL;
    return CMD("set-buffer", "-w", "-t", t->client, "--", text);
}

/* Remember which stopped selection the highlight is. */
static void freeze_flash(const Tmux *t, Commands *list) {
    push(list, CMD("set-option", "-pF", "-t", t->pane, "@vim-copy-flash-selection", SELECTION_SIGNATURE));
}

static int finish_flash(const Tmux *t, const char *token) {
    // For a timer scheduled before a config reload. The token check and
    // cleanup still execute atomically in tmux's queue.
    return call(t, flash_cleanup(t, token), NULL);
}

/* The last cell a selection of a half-open range ends on. A linewise range
 * ends on the row of its last visible character: rows after it hold only
 * the padding that terminal UIs paint to erase a line. */
static long selection_end(const Document *d, long start, long end, bool linewise) {
    end -= 1;
    while (end > start && cell_width(d->text[end]) == 0)
        end--;
    if (linewise) {
        long last = line_of(d, end), length = line_length(d, last);
        while (length && (d->text[d->starts[last] + length - 1] == ' ' || d->text[d->starts[last] + length - 1] == '\t'))
            length--;
        end = lmax(d->starts[last], d->starts[last] + length - 1);
        while (end > start && cell_width(d->text[end]) == 0)
            end--;
    }
    return end;
}

/* Build a selection for a half-open text range, including complete cells. */
static void select_span(const Tmux *t, const State *s, long start, long end, bool linewise, Commands *list) {
    Layout *layout = s->layout;
    end = selection_end(layout->document, start, end, linewise);
    long row, column;
    // Each of these redraws the whole pane, even when nothing changes.
    if (s->selection_present || s->selection_active)
        push(list, x(t, "clear-selection"));
    if (s->rectangle_toggle)
        push(list, x(t, "rectangle-off"));
    locate(s, list);
    layout_point(layout, start, &row, &column);
    move(t, s, list, row, column, NONE, false);
    layout_point(layout, end, &row, &column);
    // Line mode selects whole rows and puts the cursor at the end of the
    // last one, whichever of its columns the cursor reached.
    if (linewise)
        column = 0;
    // While selecting with the line number gutter shown, tmux redraws the
    // whole pane after every cursor step: ten times a step without it.
    // Hide the gutter for longer walks. Coordinates do not change, and
    // tmux paints only the final state of a batch.
    bool hide = s->copy_line_numbers && nearby_steps(s, list, row, column) >= 3;
    if (hide)
        push(list, x(t, "line-numbers-off"));
    push(list, x(t, "begin-selection"));
    move(t, s, list, row, column, NONE, false);
    if (hide)
        push(list, x(t, "line-numbers-on"));
    if (linewise) {
        Command mode = x(t, "selection-mode");
        push(&mode, "line");
        push(list, mode);
        list->at.known = false;
    }
}

/* Select a yanked range in the highlight style and leave the cursor at row
 * and column, in view top. While any selection exists, tmux redraws the
 * pane after every cursor step, so build the range from its start and let
 * other-end return the cursor there, where Vim leaves it for most yanks. */
static void yank_selection(const Tmux *t, const State *s, Span span, long row, long column, long top, Commands *list) {
    Layout *layout = s->layout;
    long end = selection_end(layout->document, span.start, span.end, span.linewise);
    long start_row, start_column, end_row, end_column;
    layout_point(layout, span.start, &start_row, &start_column);
    layout_point(layout, end, &end_row, &end_column);
    // Line mode selects whole rows, whichever of their columns the cursor
    // is on. It also moves the anchor to the first row's start.
    if (span.linewise)
        start_column = end_column = 0;
    bool back = span.linewise || (row == start_row && column == start_column);
    if (s->selection_present || s->selection_active)
        push(list, x(t, "clear-selection"));
    if (s->rectangle_toggle)
        push(list, x(t, "rectangle-off"));
    locate(s, list);
    move(t, s, list, start_row, start_column, NONE, false);
    Cursor origin = list->at;
    long walk = nearby_steps(s, list, end_row, end_column);
    long after = back ? (span.linewise ? layout_steps(layout, row, column, false) : 0) : 3;
    // Every step costs a full redraw with the line number gutter shown.
    bool hide = s->copy_line_numbers && walk >= 0 && walk + after >= 3;
    if (hide)
        push(list, x(t, "line-numbers-off"));
    push(list, x(t, "begin-selection"));
    move(t, s, list, end_row, end_column, NONE, false);
    if (span.linewise) {
        Command mode = x(t, "selection-mode");
        push(&mode, "line");
        push(list, mode);
    }
    if (back) {
        push(list, x(t, "other-end"));
        Cursor view = list->at;
        list->at = origin;
        list->at.top = view.top;
        list->at.shown = view.shown;
    }
    push(list, x(t, "stop-selection"));
    if (!(list->at.known && list->at.row == row && list->at.steps == layout_steps(layout, row, column, false))) {
        bool nearby = nearby_steps(s, list, row, column) >= 0 && list->at.shown && list->at.top == top;
        if (hide && !nearby) {
            // A jump counts goto-line in the gutter's units.
            push(list, x(t, "line-numbers-on"));
            hide = false;
        }
        move(t, s, list, row, column, nearby ? NONE : top, false);
    }
    if (hide)
        push(list, x(t, "line-numbers-on"));
}

static int visual_yank(Tmux *t) {
    State s;
    const char *text = NULL;
    Commands list = {0};
    TRY(query(t, &s, 4, 4, 0, NULL, NULL));
    bool line = is(s.selection_mode, "line");
    if (line || (is(s.selection_mode, "char") && !s.rectangle_toggle)) {
        // Native tmux can omit the final newline depending on which end
        // of a line selection is being dragged. Vim line yanks include it.
        long first_row = lmin(s.selection_start_y, s.selection_end_y);
        long last_row = lmax(s.selection_start_y, s.selection_end_y);
        Document *d;
        TRY(snapshot(t, &s, first_row, last_row, &d));
        Layout *layout = s.layout;
        Span span;
        if (line) {
            long first = line_of(d, layout_offset(layout, s.selection_start_y, 0));
            long last = line_of(d, layout_offset(layout, s.selection_end_y, 0));
            span = line_span(d, first, last, -1);
        } else {
            // Native character copies trim trailing spaces, including a
            // whitespace-only line that Vim correctly kept in a paragraph.
            long a = layout_offset(layout, s.selection_start_y, s.selection_start_x);
            long b = layout_offset(layout, s.selection_end_y, s.selection_end_x);
            long start = lmin(a, b), end = lmax(a, b) + 1;
            while (end < d->length && cell_width(d->text[end]) == 0)
                end++;
            span = span_of(start, end);
        }
        text = copy_text(d, span);
        push(&list, write_buffer(t, &text));
    } else {
        push(&list, CMD("send-keys", "-t", t->pane, "-X", "copy-selection-no-clear"));
    }
    Flash flash;
    int prepared = prepare_flash(s.highlight_ms, &flash);
    if (flash.token) {
        start_flash(t, &flash, &list);
        push(&list, x(t, "stop-selection"));
        freeze_flash(t, &list);
        // This selection was drawn before its style changed.
        push(&list, CMD("refresh-client", "-t", t->client));
    } else {
        push(&list, x(t, "clear-selection"));
    }
    TRY(run(t, &list, text, NULL, NULL));
    return prepared;
}

static int open_link(Tmux *t) {
    State s;
    const char *formats[] = {"#{copy_cursor_hyperlink}"};
    char *link = NULL;
    TRY(query(t, &s, 4, 4, 1, formats, &link));
    if (!*link) {
        // Read only nearby rows, extending until the logical line is
        // complete on both sides. A URL can wrap across several rows.
        long limit = s.history + s.pane_height - 1;
        for (long distance = 4;; distance *= 2) {
            long first = lmax(0, s.row - distance), last = lmin(limit, s.row + distance);
            Document *d;
            TRY(snapshot(t, &s, first, last, &d));
            long position = layout_offset(s.layout, s.row, s.copy_cursor_x), row = line_of(d, position);
            if ((first == 0 || row > 0) && (last == limit || row < d->lines - 1)) {
                TRY(link_at(d->text + d->starts[row], line_length(d, row), position - d->starts[row], &link));
                break;
            }
        }
    }
    // Pass the URI as data, never through a shell or tmux format expansion.
    // Detach the opener so starting a browser cannot block copy mode.
#ifdef __APPLE__
    const char *argv[] = {"open", link, NULL};
#else
    const char *argv[] = {"xdg-open", link, NULL};
#endif
    posix_spawn_file_actions_t actions;
    posix_spawn_file_actions_init(&actions);
    posix_spawn_file_actions_addopen(&actions, 0, "/dev/null", O_RDONLY, 0);
    posix_spawn_file_actions_addopen(&actions, 1, "/dev/null", O_WRONLY, 0);
    posix_spawn_file_actions_addopen(&actions, 2, "/dev/null", O_WRONLY, 0);
    posix_spawnattr_t attributes;
    posix_spawnattr_init(&attributes);
    sigset_t defaults;
    sigemptyset(&defaults);
    sigaddset(&defaults, SIGPIPE);
    posix_spawnattr_setsigdefault(&attributes, &defaults);
#ifdef POSIX_SPAWN_SETSID
    posix_spawnattr_setflags(&attributes, POSIX_SPAWN_SETSIGDEF | POSIX_SPAWN_SETSID);
#else
    posix_spawnattr_setflags(&attributes, POSIX_SPAWN_SETSIGDEF | POSIX_SPAWN_SETPGROUP);
#endif
    pid_t pid;
    int error = posix_spawnp(&pid, argv[0], &actions, &attributes, (char *const *)argv, environ);
    posix_spawn_file_actions_destroy(&actions);
    posix_spawnattr_destroy(&attributes);
    return error ? fail(FAILED, "Could not open link") : OK;
}

/* A string field of the flat JSON object that SAVED_SELECTION produces. */
static char *json_field(const char *json, const char *name) {
    const char *value = strstr(json, CAT("\"", name, "\":\""));
    if (!value)
        return NULL;
    Buf buffer = {0};
    for (value += strlen(name) + 4; *value && *value != '"'; value++) {
        if (*value == '\\' && value[1])
            value++;
        putch(&buffer, *value);
    }
    return *value == '"' ? text_of(&buffer) : NULL;
}

static bool parse_frame(const char *frame, long *generation, long *collected) {
    const char *colon = strchr(frame, ':');
    if (!colon)
        return false;
    return parse_long(format("%.*s", (int)(colon - frame), frame), generation) && parse_long(colon + 1, collected);
}

static char *remember_selection(const char *pane) {
    const char *target = pane ? CAT(" -t ", quote(pane)) : "";
    return CAT("if-shell -F", target, " ", quote(VISUAL_SELECTION), " { set-option -pF", target,
               " @vim-copy-last-selection ", quote(SAVED_SELECTION), " }");
}

static int reselect(Tmux *t) {
    State s;
    const char *formats[] = {"#{@vim-copy-last-selection}", "#{@vim-copy-frame}"};
    char *values[2] = {NULL, NULL};
    int status = query(t, &s, NONE, NONE, 2, formats, values);
    if (!values[0])
        return status ? status : fail(FAILED, "No reply");
    char *value = strip(values[0]);
    if (!*value)
        return fail(NO_MOTION, "No previous visual selection");
    TRY(status);
    long saved[LENGTH(SAVED_FIELDS)];
    const char *numbering = json_field(value, "numbering"), *frame = json_field(value, "frame");
    const char *mode = json_field(value, "selection_mode");
    if (!numbering || !frame || !mode)
        return fail(FAILED, "Bad saved selection");
    bool numbered = is(numbering, "absolute") || is(numbering, "relative") || is(numbering, "hybrid");
    for (long i = 0; i < LENGTH(SAVED_FIELDS); i++) {
        if (i == 5)
            continue; // selection_mode
        const char *field = json_field(value, SAVED_FIELDS[i]);
        // Selections saved before the gutter was enabled have no
        // visibility field. Keep gv usable across a config reload.
        if (!field && is(SAVED_FIELDS[i], "copy_line_numbers"))
            field = numbered ? "1" : "0";
        if (!field || !parse_long(field, &saved[i]))
            return fail(FAILED, "Bad saved selection");
    }
    enum { START_X, START_Y, END_X, END_Y, RECTANGLE, MODE, CURSOR_X, CURSOR_Y, SCROLL, LIMIT, HEIGHT, WIDTH, LINES };
    long generation, collected, old_generation, old_collected;
    if (!parse_frame(strip(values[1]), &generation, &collected) || !parse_frame(frame, &old_generation, &old_collected))
        return fail(FAILED, "Bad copy-mode frame");
    if (generation != old_generation || s.pane_width != saved[WIDTH])
        return fail(NO_MOTION, "Previous selection no longer matches this scrollback");
    // Collected history shifts physical rows on a later copy-mode entry.
    // While still in the same frozen snapshot, the frame stays unchanged.
    long removed = collected - old_collected;
    long old_history = saved[LIMIT] - (saved[LINES] && numbered ? saved[HEIGHT] : 0);
    long top = old_history - saved[SCROLL] - removed, row = top + saved[CURSOR_Y];
    long start_y = saved[START_Y] - removed, end_y = saved[END_Y] - removed;
    long first = lmin(row, lmin(start_y, end_y)), last = lmax(row, lmax(start_y, end_y));
    if (removed < 0 || first < 0 || last >= s.history + s.pane_height)
        return fail(NO_MOTION, "Previous selection has left the scrollback");
    // Only the saved selection and cursor need character/cell mapping.
    Document *d;
    TRY(snapshot(t, &s, first, last, &d));
    State restored = s;
    restored.selection_start_x = saved[START_X];
    restored.selection_start_y = start_y;
    restored.selection_end_x = saved[END_X];
    restored.selection_end_y = end_y;
    restored.selection_mode = mode;
    restored.rectangle_toggle = saved[RECTANGLE];
    restored.selection_present = restored.selection_active = 1;
    restored.row = row;
    restored.copy_cursor_x = saved[CURSOR_X];
    restored.scroll_position = s.history - lmax(0, lmin(top, s.history));
    // Remember the current visual selection first, so gv swaps with it.
    Commands list = {0};
    push(&list, CMD("if-shell", "-F", "-t", t->pane, VISUAL_SELECTION,
                    CAT("set-option -pF -t ", quote(t->pane), " @vim-copy-last-selection ", quote(SAVED_SELECTION))));
    push(&list, CMD("set-option", "-pu", "-t", t->pane, "@vim-copy-flash"));
    push(&list, CMD("set-option", "-pu", "-t", t->pane, "@vim-copy-flash-selection"));
    push(&list, CMD("set-option", "-pu", "-t", t->pane, "copy-mode-selection-style"));
    restore(t, &restored, &list);
    return run(t, &list, NULL, NULL, NULL);
}

static int prime_sentences(Tmux *t) {
    State s;
    const char *formats[] = {SENTENCE_FRAME};
    char *frame = NULL;
    TRY(query(t, &s, 32, 32, 1, formats, &frame));
    Document *d;
    TRY(snapshot(t, &s, lmax(0, s.row - 32), lmin(s.history + s.pane_height - 1, s.row + 32), &d));
    Commands cache = {0};
    sentence_cache(t, &s, d, &cache);
    // A background warmup must not overwrite a foreground command's
    // cache, or one belonging to a newer copy-mode entry or resize.
    const char *condition = CAT("#{&&:#{==:" SENTENCE_FRAME ",", strip(frame), "},#{!:#{@vim-copy-sentence-frame}}}");
    return call(t, CMD("if-shell", "-F", "-t", t->pane, condition, commands_string(&cache)), NULL);
}

/* Read the rows from the cursor to where a search stopped, with the complete
 * lines of both: an exclusive motion reads the first line's indentation and
 * where the last line starts. */
static int search_snapshot(Tmux *t, State *s, const State *target, Document **document, Span *span) {
    long low = lmin(s->row, target->row), high = lmax(s->row, target->row), limit = s->history + s->pane_height - 1;
    for (long distance = 4;; distance *= 2) {
        long first = lmax(0, low - distance), last = lmin(limit, high + distance);
        Document *d;
        TRY(snapshot(t, s, first, last, &d));
        long position = layout_offset(s->layout, s->row, s->copy_cursor_x);
        long end = layout_offset(s->layout, target->row, target->copy_cursor_x);
        if ((first == 0 || line_of(d, lmin(position, end)) > 0) &&
            (last == limit || line_of(d, lmax(position, end)) < d->lines - 1)) {
            *document = d;
            *span = exclusive(d, position, end);
            return OK;
        }
    }
}

static int perform(Tmux *t, const Context *c, const char *argument) {
    long before = c->before, digits = 1;
    if (*c->digits && !parse_long(c->digits, &digits))
        return fail(FAILED, "Bad count");
    if (before > MAX_COUNT || digits > MAX_COUNT || digits < 1 || before * digits > MAX_COUNT)
        return fail(NO_MOTION, "Copy count is too large");
    long count = before * digits;
    const char *key = c->motion;
    bool object = c->object != NULL;
    State s;
    if (is(c->mode, "move")) {
        bool word_end = is(key, "ge") || is(key, "gE");
        Document *d;
        long target;
        TRY(query(t, &s, word_end ? 4 : 32, word_end ? 0 : 32, 0, NULL, NULL));
        TRY(word_end ? word_end_snapshot(t, &s, count, is(key, "gE"), &d, &target)
                     : sentence_snapshot(t, &s, key[0], count, &d, &target));
        target = lmin(target, d->length - 1);
        // A trailing newline is not a cursor cell on a nonempty final line.
        long line = line_of(d, target), row, column;
        target = lmax(d->starts[line], lmin(target, line_end(d, line) - 1));
        layout_point(s.layout, target, &row, &column);
        long top = lmax(row - s.pane_height + 1, lmin(row, s.history - s.scroll_position));
        Commands list = {0};
        if (!word_end)
            sentence_cache(t, &s, d, &list);
        move(t, &s, &list, row, column, top, word_end && s.selection_active);
        if (!word_end && !s.selection_active && !s.selection_present)
            push(&list, x(t, "scroll-middle"));
        return run(t, &list, NULL, NULL, NULL);
    }
    Document *d;
    Span span;
    State from; /* Where the batch finds the cursor. */
    if (one_of(key, "/?nN")) {
        const char *formats[] = {"#{pane_search_string}"};
        char *previous = NULL;
        TRY(query(t, &s, 4, 4, 1, formats, &previous));
        const char *native = is(key, "/") ? "search-forward" : is(key, "?") ? "search-backward"
                           : is(key, "n") ? "search-again" : "search-reverse";
        Command search = CMD("send-keys", "-t", t->pane, "-X", "-N", num(count), native);
        if (one_of(key, "/?")) {
            push(&search, "--");
            push(&search, argument && *argument ? argument : previous);
        }
        // Search natively first, then read only the rows up to the match.
        Commands list = {0};
        push(&list, search);
        push(&list, CMD("display-message", "-p", "-t", t->pane, STATE_FORMAT));
        char *output;
        long length;
        State target;
        TRY(run(t, &list, NULL, &output, &length));
        TRY(parse_state(CAT(next_line(&output)), &target));
        if (target.row == s.row && target.copy_cursor_x == s.copy_cursor_x)
            return fail(NO_MOTION, "No search match");
        int status = search_snapshot(t, &s, &target, &d, &span);
        if (status || span.end <= span.start) {
            Commands back = {0};
            restore(t, &s, &back);
            run(t, &back, NULL, NULL, NULL);
            TRY(status);
        }
        from = s;
        from.row = target.row;
        from.copy_cursor_x = target.copy_cursor_x;
        from.scroll_position = target.scroll_position;
    } else {
        bool global = object ? is(key, "t") : is(key, "G") || is(key, "gg");
        long codes_length = 0;
        uint32_t *codes = NULL;
        if (argument) {
            codes = alloc((strlen(argument) + 1) * sizeof *codes);
            codes_length = decode(argument, strlen(argument), codes);
        }
        TRY(query(t, &s, global ? ALL : 4, global ? ALL : 4, 0, NULL, NULL));
        TRY(operation_snapshot(t, &s, c, count, codes, codes_length, &d, &span));
        from = s;
    }
    Layout *layout = s.layout;
    if (span.end <= span.start)
        return fail(NO_MOTION, "Text object is empty");
    // A terminal cell can hold a base character plus combining marks. Keep
    // the complete cell when an inclusive motion ends on its base character.
    while (span.end < d->length && cell_width(d->text[span.end]) == 0)
        span.end++;
    Commands list = {0};
    if (is(c->mode, "yank")) {
        long cursor = span.cursor >= 0 ? span.cursor : span.start;
        if (object) {
            long line = line_of(d, cursor);
            // Inner blocks that start on a newline land on the next line.
            // Other characterwise objects (notably tags) clamp to the last
            // character, since Vim's normal cursor cannot sit on a newline.
            if (is(c->object, "i") && one_of(key, "bB(){}[]<>") && cursor == line_end(d, line) && line_length(d, line)) {
                cursor++;
                line = line_of(d, cursor);
            }
            cursor = lmax(d->starts[line], lmin(cursor, line_end(d, line) - 1));
        }
        long row, column;
        layout_point(layout, cursor, &row, &column);
        long top = lmax(0, lmin(s.history - s.scroll_position, row));
        top = lmax(top, row - s.pane_height + 1);
        // Write the buffer, then select and highlight its range, in one
        // client. The final move restores the viewport the yank began in.
        Flash flash;
        int prepared = prepare_flash(s.highlight_ms, &flash);
        const char *text = copy_text(d, span);
        push(&list, write_buffer(t, &text));
        from.layout = layout;
        if (flash.token) {
            start_flash(t, &flash, &list);
            yank_selection(t, &from, span, row, column, top, &list);
            freeze_flash(t, &list);
        } else {
            if (from.selection_present || from.selection_active)
                push(&list, x(t, "clear-selection"));
            move(t, &s, &list, row, column, top, false);
        }
        TRY(run(t, &list, text, NULL, NULL));
        return prepared;
    }
    long start = span.start, end = span.end;
    if ((one_of(key, "wWps")) && s.selection_present) {
        long a = layout_offset(layout, s.selection_start_y, s.selection_start_x);
        long b = layout_offset(layout, s.selection_end_y, s.selection_end_x);
        start = lmin(start, lmin(a, b));
        end = lmax(end, lmax(a + 1, b + 1));
    }
    from.layout = layout;
    select_span(t, &from, start, end, span.linewise, &list);
    return run(t, &list, NULL, NULL, NULL);
}

/* ---------------------------------------------------------------------- */
/* Bindings */

static const char *helper;

static char *shell_command(const char *action) {
    Command c = CMD(helper, action);
    return escape_formats(shell_join(&c));
}

static void emit(const char *table, const char *key, const char *body) {
    printf("bind -T %s %s { %s }\n", table, quote(key), body);
}

static const char *execute_command;

static char *execute(const char *motion, bool argument) {
    const char *run = CAT("run-shell ", quote(CAT(execute_command, shell_quote(motion))));
    // Keep prompt input out of shell code altogether, including quotes,
    // empty searches and tmux format characters. The helper reads the
    // pane option through a separate tmux command.
    // %1 is the raw substitution for a structured command argument.
    // %% would add shell-style escaping to apostrophes in the value.
    return argument ? CAT("set-option -p @vim-copy-argument \"%1\"; ", run) : (char *)run;
}

static char *enter(const char *mode, const char *before, const char *object) {
    const char *table = is(mode, "yank") ? "copyVimYank" : is(mode, "visual") ? "copyVimObject" : "copyVimNormalG";
    return CAT("set-option -p @vim-copy-mode ", mode, "; set-option -pF @vim-copy-before ", quote(before),
               "; set-option -p @vim-copy-after 0; set-option -p @vim-copy-object ", object,
               "; send-keys -N 1; switch-client -T ", table);
}

/* End a pending highlight, clearing its selection if it is still shown. */
#define CANCEL_FLASH \
    "if-shell -F \"#{@vim-copy-flash}\" { if-shell -F \"" FLASH_SELECTION "\" { send-keys -X clear-selection }; " \
    "set-option -pu @vim-copy-flash; set-option -pu @vim-copy-flash-selection; " \
    "set-option -pu copy-mode-selection-style }; "
#define SENTENCE_ROW "#{e|+|:#{e|-|:#{@vim-copy-sentence-history},#{scroll_position}},#{copy_cursor_y}}"
#define SENTENCE_POSITION "#{e|+|:#{e|*|:" SENTENCE_ROW ",#{e|+|:#{pane_width},1}},#{copy_cursor_x}}"

static char *sentence(char key, const char *before) {
    const char *direction = key == '(' ? "previous" : key == ')' ? "next" : key == '{' ? "paragraph-previous"
                                                                                   : "paragraph-next";
    const char *bound = CAT("@vim-copy-sentence-", key == '{' || key == '}' ? CAT(direction, "-") : "");
    const char *valid = CAT("#{&&:#{==:#{@vim-copy-sentence-frame}," SENTENCE_FRAME "},#{&&:#{e|<=|:", before,
                            ",1},#{&&:#{==:#{refresh_active},0},#{&&:#{e|>=|:#{@vim-copy-sentence-position},#{",
                            bound, "lower}},#{e|<|:#{@vim-copy-sentence-position},#{", bound, "upper}}}}}}");
    const char *relative =
        "if-shell -F \"#{e|>|:#{@vim-copy-sentence-target},#{@vim-copy-sentence-row}}\" "
        "{ send-keys -FX -N \"#{e|-|:#{@vim-copy-sentence-target},#{@vim-copy-sentence-row}}\" cursor-down }; "
        "if-shell -F \"#{e|<|:#{@vim-copy-sentence-target},#{@vim-copy-sentence-row}}\" "
        "{ send-keys -FX -N \"#{e|-|:#{@vim-copy-sentence-row},#{@vim-copy-sentence-target}}\" cursor-up }";
    // Jump directly for a large gap rather than redrawing every row on
    // the way. This also keeps long paragraph movement responsive.
    const char *top = "#{?#{e|<|:#{@vim-copy-sentence-target},#{@vim-copy-sentence-history}},"
                      "#{@vim-copy-sentence-target},#{@vim-copy-sentence-history}}";
    const char *line = "#{?" NUMBERED_FORMAT ",#{e|+|:#{@vim-copy-sentence-top},1},"
                       "#{e|-|:#{@vim-copy-sentence-history},#{@vim-copy-sentence-top}}}";
    // -F does not expand goto-line's argument. run-shell -C expands and
    // executes a tmux command directly, without starting a shell process.
    const char *jump = CAT("set-option -pF @vim-copy-sentence-top ", quote(top), "; run-shell -C ",
                           quote(CAT("send-keys -X goto-line ", line, "; send-keys -X top-line; "
                                     "if-shell -F \"#{e|>|:#{@vim-copy-sentence-target},#{@vim-copy-sentence-top}}\" "
                                     "{ send-keys -X -N \"#{e|-|:#{@vim-copy-sentence-target},"
                                     "#{@vim-copy-sentence-top}}\" cursor-down }")));
    const char *far = "#{||:#{e|>|:#{e|-|:#{@vim-copy-sentence-target},#{@vim-copy-sentence-row}},#{pane_height}},"
                      "#{e|>|:#{e|-|:#{@vim-copy-sentence-row},#{@vim-copy-sentence-target}},#{pane_height}}}";
    const char *native = CAT("set-option -pF @vim-copy-sentence-row ", quote(SENTENCE_ROW),
                             "; set-option -pF @vim-copy-sentence-target \"#{E:@vim-copy-sentence-", direction,
                             "-row}\"; set-option -pF @vim-copy-sentence-steps \"#{E:@vim-copy-sentence-", direction,
                             "-steps}\"; if-shell -F ", quote(far), " { ", jump, " } { ", relative,
                             " }; if-shell -F \"#{!=:#{copy_cursor_x},0}\" { send-keys -X start-of-line }; "
                             "if-shell -F \"#{e|>|:#{@vim-copy-sentence-steps},0}\" "
                             "{ send-keys -FX -N \"#{@vim-copy-sentence-steps}\" cursor-right }; "
                             "if-shell -F \"#{!:#{||:#{selection_present},#{selection_active}}}\" "
                             "{ send-keys -X scroll-middle }");
    const char *fallback = CAT("set-option -p @vim-copy-mode move; set-option -pF @vim-copy-before ", quote(before),
                               "; set-option -p @vim-copy-after 0; set-option -p @vim-copy-object -; ",
                               execute(letter(key), false));
    return CAT(CANCEL_FLASH "send-keys -N 1; set-option -pF @vim-copy-sentence-position ", quote(SENTENCE_POSITION),
               "; if-shell -F ", quote(valid), " { ", native, " } { ", fallback, " }");
}

static void bindings(void) {
    // Pending input is handled synchronously by tmux tables/prompts.
    // Launching a helper between keys lets fast typing outrun installation
    // of a prompt. The helper runs only once a complete operation has been
    // entered.
    execute_command = CAT(shell_command("execute"), " #{q:pane_id} #{q:client_name} #{q:@vim-copy-mode}"
                                                    " #{q:@vim-copy-before} #{q:@vim-copy-after} #{q:@vim-copy-object} ");
    static const char *const defaults[][2] = {{"@vim-copy-highlight-ms", "150"},
                                              {"@vim-copy-highlight-style", "bg=#313131"}};
    for (int i = 0; i < LENGTH(defaults); i++)
        printf("set-option -goq %s %s\n", defaults[i][0], quote(defaults[i][1]));
    // A frame records the live grid's origin when tmux freezes it. Later
    // pane output must not change that origin. An indexed hook makes
    // reloads idempotent and leaves unrelated user hooks in place.
    printf("set-hook -g pane-mode-entered[17391] { if-shell -F \"#{==:#{pane_mode},copy-mode}\" "
           "{ send-keys -X line-numbers-on; set-option -pF @vim-copy-frame %s; set-option -pF @vim-copy-epoch "
           "\"#{e|+|:#{?@vim-copy-epoch,#{@vim-copy-epoch},0},1}\"; set-option -pu @vim-copy-sentence-frame; "
           "run-shell -b %s } }\n",
           quote(COPY_FRAME), quote(CAT(shell_command("prime-sentences"), " #{q:pane_id} -")));
    printf("set-hook -g pane-resized[17391] { set-option -pu @vim-copy-last-selection; "
           "set-option -pu @vim-copy-sentence-frame }\n");
    printf("if-shell -F \"#{&&:#{==:#{pane_mode},copy-mode},#{!:#{@vim-copy-frame}}}\" "
           "{ set-option -pF @vim-copy-frame %s }\n",
           quote(COPY_FRAME));

    const char *visual_yank = CAT(shell_command("visual-yank"), " #{q:pane_id} #{q:client_name}");
    const char *remember = CAT(remember_selection(NULL), "; ");
    emit("copy-mode-vi", "r", "set-option -pu @vim-copy-sentence-frame; send-keys -X refresh-now");
    static const char *const plain[][2] = {{"Escape", "clear-selection"}, {"C-[", "clear-selection"},
                                           {"q", "cancel"}, {"C-c", "cancel"},
                                           {"Enter", "copy-pipe-and-cancel"}, {"A", "append-selection-and-cancel"}};
    for (int i = 0; i < LENGTH(plain); i++)
        emit("copy-mode-vi", plain[i][0], CAT(remember, "send-keys -X ", plain[i][1]));
    static const char *const visual[][2] = {{"v", "begin-selection"}, {"Space", "begin-selection"},
                                            {"V", "select-line"}, {"C-v", "begin-selection; send-keys -X rectangle-toggle"}};
    for (int i = 0; i < LENGTH(visual); i++)
        emit("copy-mode-vi", visual[i][0], CAT(CANCEL_FLASH, remember, "send-keys -X ", visual[i][1]));
    // Override tmux-open's `o`, which copies the selection and exits copy
    // mode. Opening links belongs to gx; visual o changes the active end.
    emit("copy-mode-vi", "o", CAT(CANCEL_FLASH "if-shell -F ", quote(VISUAL_SELECTION),
                                  " { send-keys -N 1; if-shell -F \"#{==:#{selection_mode},line}\" "
                                  "{ send-keys -X other-end; send-keys -X selection-mode line } "
                                  "{ send-keys -X other-end } }"));

    static const char *const counted[][2] = {{"copy-mode-vi", "0"}, {"copyVimCount", "#{@vim-copy-count}"}};
    for (int i = 0; i < LENGTH(counted); i++) {
        const char *table = counted[i][0], *before = counted[i][1];
        emit(table, "y", CAT(CANCEL_FLASH "if-shell -F '#{||:#{selection_present},#{selection_active}}' { ", remember,
                             "run-shell ", quote(visual_yank), " } { ", enter("yank", before, "-"), " }"));
        for (const char *key = "ia"; *key; key++)
            emit(table, letter(*key), CAT(CANCEL_FLASH "if-shell -F '#{||:#{selection_present},#{selection_active}}' { ",
                                          enter("visual", before, letter(*key)), " }"));
        for (const char *key = "(){}"; *key; key++)
            emit(table, letter(*key), sentence(*key, before));
        emit(table, "g", CAT(CANCEL_FLASH, enter("move", before, "-")));
    }
    emit("copyVimNormalG", "g", "send-keys -X history-top");
    emit("copyVimNormalG", "e", execute("ge", false));
    emit("copyVimNormalG", "E", execute("gE", false));
    emit("copyVimNormalG", "v", CAT("send-keys -N 1; run-shell ",
                                    quote(CAT(shell_command("reselect"), " #{q:pane_id} #{q:client_name}"))));
    emit("copyVimNormalG", "x", CAT("send-keys -N 1; run-shell ",
                                    quote(CAT(shell_command("open-link"), " #{q:pane_id} #{q:client_name}"))));
    emit("copyVimNormalG", "Any", "send-keys -N 1");
    for (char digit = '1'; digit <= '9'; digit++) {
        const char *d = letter(digit);
        emit("copy-mode-vi", d, CAT("command-prompt -NP -I ", d, " -p \"(repeat)\" { send-keys -N \"%%\"; "
                                    "set-option -p @vim-copy-count \"%%\"; switch-client -T copyVimCount }"));
        emit("copyVimYank", d, CAT("command-prompt -NP -I ", d, " -p \"(y count)\" { set-option -p @vim-copy-after "
                                   "\"%%\"; switch-client -T copyVimYank }"));
    }
    emit("copyVimCount", "Any", "switch-client -T root; send-keys -K");
    for (const char *key = "yeEbBwW^0$hl%jkG{}()nN"; *key; key++)
        emit("copyVimYank", letter(*key), execute(letter(*key), false));
    for (const char *key = "ia"; *key; key++)
        emit("copyVimYank", letter(*key), CAT("set-option -p @vim-copy-object ", letter(*key),
                                              "; switch-client -T copyVimObject"));
    for (const char *key = "fFtT/?"; *key; key++) {
        const char *flags = strchr("/?", *key) ? "-P -T search" : "-1P";
        emit("copyVimYank", letter(*key), CAT("command-prompt ", flags, " -p ", quote(CAT("(y", letter(*key), ")")),
                                              " { ", execute(letter(*key), true), " }"));
    }
    emit("copyVimYank", "g", "switch-client -T copyVimG");
    emit("copyVimG", "g", execute("gg", false));
    emit("copyVimG", "e", execute("ge", false));
    emit("copyVimG", "E", execute("gE", false));
    for (const char *key = "wWpsbB(){}[]<>tq'\"`"; *key; key++)
        emit("copyVimObject", letter(*key), execute(letter(*key), false));
    static const char *const pending[] = {"copyVimYank", "copyVimG", "copyVimObject"};
    for (int i = 0; i < LENGTH(pending); i++)
        emit(pending[i], "Any", "send-keys -N 1");
}

/* ---------------------------------------------------------------------- */
/* Test probe: the text engine over a line protocol, for test_vim_copy.py. */

#ifdef VIM_COPY_PROBE
static int nibble(char c) { return c <= '9' ? c - '0' : (c | 32) - 'a' + 10; }

static char *unhex(const char *hex, long *length) {
    long n = strlen(hex) / 2;
    char *bytes = alloc(n + 1);
    for (long i = 0; i < n; i++)
        bytes[i] = nibble(hex[2 * i]) << 4 | nibble(hex[2 * i + 1]);
    if (length)
        *length = n;
    return bytes;
}

static Document *probe_document(const char *hex) {
    long length;
    char *bytes = unhex(hex, &length);
    return document_from_bytes(bytes, length);
}

static void print_hex(const char *text) {
    printf("ok ");
    for (const unsigned char *c = (const unsigned char *)text; *c; c++)
        printf("%02x", *c);
    putchar('\n');
}

static void print_span(Span span, long offset) {
    printf("ok %ld %ld %d %ld\n", span.start + offset, span.end + offset, span.linewise,
           span.cursor >= 0 ? span.cursor + offset : -1);
}

static void probe(void) {
    char *line = NULL;
    size_t capacity = 0;
    ssize_t length;
    setvbuf(stdout, NULL, _IOFBF, 1 << 16);
    while ((length = getline(&line, &capacity, stdin)) > 0) {
        if (line[length - 1] == '\n')
            line[length - 1] = 0;
        char *f[16] = {0};
        int n = 0;
        for (char *cursor = line; n < 16 && cursor;) {
            f[n++] = cursor;
            cursor = strchr(cursor, '\t');
            if (cursor)
                *cursor++ = 0;
        }
        const char *op = f[0];
        int status = OK;
        Span span;
        long value;
        reason = "";
        if (is(op, "motion") || is(op, "object")) {
            Document *d = probe_document(f[1]);
            const char *key = unhex(f[3], NULL);
            if (is(op, "motion")) {
                long argument_length = 0;
                uint32_t *argument = NULL;
                if (!is(f[5], "-")) {
                    char *bytes = unhex(f[5], &argument_length);
                    argument = alloc((argument_length + 1) * sizeof *argument);
                    argument_length = decode(bytes, argument_length, argument);
                }
                status = motion(d, atol(f[2]), key, atol(f[4]), argument, argument_length, atoi(f[6]), &span);
            } else {
                status = text_object(d, atol(f[2]), key, atoi(f[4]), atol(f[5]), &span);
            }
            if (!status)
                print_span(span, 0);
        } else if (is(op, "wordend") || is(op, "sentence") || is(op, "paragraph")) {
            Document *d = probe_document(f[1]);
            if (is(op, "wordend"))
                status = previous_word_end(d, atol(f[2]), atol(f[3]), atoi(f[4]), &value);
            else if (is(op, "sentence"))
                status = sentence_target(d, atol(f[2]), *unhex(f[3], NULL), atol(f[4]), &value);
            else
                status = paragraph_target(d, atol(f[2]), *unhex(f[3], NULL), atol(f[4]), &value);
            if (!status)
                printf("ok %ld\n", value);
        } else if (is(op, "boundary")) {
            printf("ok %d\n", paragraph_boundary(probe_document(f[1]), atol(f[2])));
        } else if (is(op, "starts")) {
            Longs *starts = sentence_starts(probe_document(f[1]));
            printf("ok");
            for (long i = 0; i < starts->n; i++)
                printf(" %ld", starts->v[i]);
            putchar('\n');
        } else if (is(op, "copy")) {
            Span copied = {atol(f[2]), atol(f[3]), atoi(f[4]), -1, false};
            print_hex(copy_text(probe_document(f[1]), copied));
        } else if (is(op, "enclose")) {
            Document *d = probe_document(f[1]);
            long position = atol(f[3]), count = atol(f[4]);
            Pairs pairs = brackets(d, *unhex(f[2], NULL), !atoi(f[5]), position, count);
            Pair p;
            if (!(status = enclosing(&pairs, position, count, &p, NULL)))
                printf("ok %ld %ld\n", p.start, p.end);
        } else if (is(op, "layout") || is(op, "point") || is(op, "offset")) {
            Layout *layout = layout_of(probe_document(f[1]), atol(f[2]), 0);
            if (is(op, "layout")) {
                printf("ok");
                for (long i = 0; i < layout->rows.n; i++)
                    printf(" %ld %ld", layout->rows.v[i].start, layout->rows.v[i].end);
                putchar('\n');
            } else if (is(op, "point")) {
                long row, column;
                layout_point(layout, atol(f[3]), &row, &column);
                printf("ok %ld %ld\n", row, column);
            } else {
                printf("ok %ld\n", layout_offset(layout, atol(f[3]), atol(f[4])));
            }
        } else if (is(op, "link")) {
            long bytes_length;
            char *bytes = unhex(f[1], &bytes_length), *link;
            uint32_t *text = alloc((bytes_length + 1) * sizeof *text);
            if (!(status = link_at(text, decode(bytes, bytes_length, text), atol(f[2]), &link)))
                print_hex(link);
        } else if (is(op, "width")) {
            printf("ok %d\n", cell_width(strtoul(f[1], NULL, 10)));
        } else if (is(op, "operation")) {
            // A yank against slices of one document, as if each slice
            // were a capture of those rows, with offsets into the whole.
            Document *d = probe_document(f[1]);
            Tmux t = {0};
            State s = {0};
            Layout *layout = layout_of(d, atol(f[2]), 0);
            t.prefetch = (Prefetch){d, layout, 0, layout->rows.n - 1};
            s.row = atol(f[3]);
            s.copy_cursor_x = atol(f[4]);
            s.pane_height = 12;
            s.history = layout->rows.n - s.pane_height;
            s.pane_width = atol(f[2]);
            long count = atol(f[7]);
            Context c = {"yank", unhex(f[5], NULL), is(f[6], "-") ? NULL : unhex(f[6], NULL),
                         count > 1 ? num(count) : "", 1, false};
            Document *slice;
            if (!(status = operation_snapshot(&t, &s, &c, count, NULL, 0, &slice, &span)))
                print_span(span, t.first_offset);
        } else {
            status = fail(FAILED, "Unknown probe operation");
        }
        if (status)
            printf("%s %s\n", status == INCOMPLETE ? "incomplete" : status == NO_MOTION ? "nomotion" : "failed", reason);
        fflush(stdout);
        release();
    }
}
#endif

/* ---------------------------------------------------------------------- */

static const char *socket_path(void) {
    const char *value = getenv("TMUX");
    if (!value || !*value)
        return NULL;
    // $TMUX is socket,pid,session; the socket path may contain commas.
    char *socket = CAT(value);
    for (int fields = 0; fields < 2; fields++) {
        char *comma = strrchr(socket, ',');
        if (!comma)
            break;
        *comma = 0;
    }
    return socket;
}

static const char *executable(const char *argv0) {
#ifdef __linux__
    char *path = realpath("/proc/self/exe", NULL);
#else
    char *path = strchr(argv0, '/') ? realpath(argv0, NULL) : NULL;
#endif
    return path ? path : argv0;
}

int main(int argc, char **argv) {
    signal(SIGPIPE, SIG_IGN);
    if (argc == 2 && is(argv[1], "bindings")) {
        helper = executable(argv[0]);
        bindings();
        return 0;
    }
#ifdef VIM_COPY_PROBE
    if (argc == 2 && is(argv[1], "probe")) {
        probe();
        return 0;
    }
#endif
    if (argc < 4) {
        fprintf(stderr, "usage: %s bindings | ACTION PANE CLIENT [ARGUMENTS...]\n", argv[0]);
        return 2;
    }
    const char *action = argv[1];
    Tmux t = {.socket = socket_path(), .pane = argv[2], .client = argv[3]};
    int status = OK;
    if (is(action, "finish-flash")) {
        status = argc > 4 ? finish_flash(&t, argv[4]) : fail(FAILED, "Missing token");
    } else if (is(action, "visual-yank")) {
        status = visual_yank(&t);
    } else if (is(action, "reselect")) {
        status = reselect(&t);
    } else if (is(action, "open-link")) {
        status = open_link(&t);
    } else if (is(action, "prime-sentences")) {
        status = prime_sentences(&t);
    } else if (is(action, "execute")) {
        if (argc < 9) {
            status = fail(FAILED, "Missing arguments");
        } else {
            const char *before = argv[5], *after = argv[6], *object = argv[7], *motion = argv[8];
            long count;
            Context c = {.mode = argv[4], .motion = motion, .object = is(object, "-") ? NULL : object,
                         .digits = is(after, "0") ? "" : after, .explicit = !is(before, "0") || !is(after, "0")};
            if (!parse_long(before, &count)) {
                status = fail(FAILED, "Bad count");
            } else {
                c.before = lmax(1, count);
                char *argument = NULL;
                if (!c.object && one_of(motion, "fFtT/?")) {
                    status = call(&t, CMD("show-options", "-pqv", "-t", t.pane, "@vim-copy-argument"), &argument);
                    long length = argument ? strlen(argument) : 0;
                    if (length && argument[length - 1] == '\n')
                        argument[length - 1] = 0;
                }
                if (!status)
                    status = perform(&t, &c, argument);
            }
        }
    }
    // Closing a pane/client during background work is harmless.
    if (status && !is(action, "finish-flash") && !is(action, "prime-sentences")) {
        const char *message = status == NO_MOTION || status == INCOMPLETE ? reason
                              : is(action, "open-link")                    ? "Could not open link"
                                                                           : "Copy command failed";
        call(&t, CMD("display-message", "-c", t.client, "--", message), NULL);
    }
    return 0;
}
