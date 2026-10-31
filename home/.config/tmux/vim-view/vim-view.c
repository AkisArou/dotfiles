/* The C side of vim-view.lua, loaded with LuaJIT's FFI.
 *
 * Neovim keeps what needs Neovim: the buffer, highlights, windows and keys.
 * This library turns lines captured from a tmux pane into the view's plain
 * text with highlight spans, and does tmux's row arithmetic on them.
 *
 * Build: make vim-view.so */

#define _DEFAULT_SOURCE /* mkfifo and O_CLOEXEC under -std=c11. */

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "unicode.h"

#define LENGTH(a) ((long)(sizeof(a) / sizeof *(a)))
#define FILL UINT32_MAX /* A span's stop: highlight to the window's edge. */
#define TRUE_COLOUR 0x1000000 /* Colours: -1 default, 0-255 palette, or this | rgb. */

enum { BOLD = 1, ITALIC = 2, REVERSE = 4, STRIKETHROUGH = 8, UNDERLINE_SHIFT = 4 };

typedef struct { uint32_t start, stop; int32_t attr; } vv_span;
typedef struct { int32_t fg, bg, sp; uint32_t flags; } vv_attr;

typedef struct { char *data; long length, size; } Buf;
typedef struct { char *text; long length, cells; vv_span *spans; long count; } Line;

static void *grow(void *items, long *size, long needed, size_t item) {
    if (needed <= *size)
        return items;
    long size2 = *size ? *size : 16;
    while (size2 < needed)
        size2 *= 2;
    if (!(items = realloc(items, size2 * item)))
        abort();
    *size = size2;
    return items;
}

static void append(Buf *buffer, const char *data, long length) {
    buffer->data = grow(buffer->data, &buffer->size, buffer->length + length + 1, 1);
    memcpy(buffer->data + buffer->length, data, length);
    buffer->length += length;
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

/* The character starting text and its size; an invalid byte is U+FFFD. */
static int decode(const char *text, long length, uint32_t *out) {
    unsigned char c = text[0];
    int size = c < 0x80 ? 1 : c >= 0xC2 && c < 0xE0 ? 2 : c >= 0xE0 && c < 0xF0 ? 3 : c >= 0xF0 && c < 0xF5 ? 4 : 0;
    bool valid = size && size <= length;
    uint32_t code = size > 1 ? c & (0x7F >> size) : c;
    for (int k = 1; k < size && valid; k++) {
        unsigned char next = text[k];
        valid = (next & 0xC0) == 0x80;
        code = code << 6 | (next & 0x3F);
    }
    if ((size == 3 && (code < 0x800 || (code >= 0xD800 && code <= 0xDFFF))) ||
        (size == 4 && (code < 0x10000 || code > 0x10FFFF)))
        valid = false;
    *out = valid ? code : 0xFFFD;
    return valid ? size : 1;
}

/* Cells of text, with tab stops every 8 cells. */
static long cells_of(const char *text, long length) {
    long cells = 0;
    for (long i = 0; i < length;) {
        uint32_t c;
        i += decode(text + i, length - i, &c);
        cells += c == '\t' ? (cells / 8 + 1) * 8 - cells : cell_width(c);
    }
    return cells;
}

/* ---------------------------------------------------------------------- */
/* Highlight attributes */

static vv_attr *attrs; /* attrs[0] is the default, which needs no span. */
static long attr_count, attr_size;
static int32_t *table; /* Open addressing: attribute index + 1, or 0. */
static long table_size;

static uint32_t hash(const vv_attr *a) {
    uint32_t h = 2166136261u;
    int32_t fields[] = { a->fg, a->bg, a->sp, (int32_t)a->flags };
    for (int i = 0; i < LENGTH(fields); i++)
        h = (h ^ (uint32_t)fields[i]) * 16777619u;
    return h;
}

static bool same(const vv_attr *a, const vv_attr *b) {
    return a->fg == b->fg && a->bg == b->bg && a->sp == b->sp && a->flags == b->flags;
}

static int32_t byte(long value) { return value < 0 ? 0 : value & 255; }

static int32_t attr_id(const vv_attr *a) {
    if (!attr_count) {
        attrs = grow(attrs, &attr_size, 1, sizeof *attrs);
        attrs[attr_count++] = (vv_attr){ -1, -1, -1, 0 };
    }
    if (same(a, &attrs[0]))
        return 0;
    if (attr_count * 2 >= table_size) {
        free(table);
        table_size = table_size ? table_size * 2 : 256;
        if (!(table = calloc(table_size, sizeof *table)))
            abort();
        for (long k = 1; k < attr_count; k++) {
            long i = hash(&attrs[k]) & (table_size - 1);
            while (table[i])
                i = (i + 1) & (table_size - 1);
            table[i] = k + 1;
        }
    }
    long i = hash(a) & (table_size - 1);
    for (; table[i]; i = (i + 1) & (table_size - 1))
        if (same(&attrs[table[i] - 1], a))
            return table[i] - 1;
    attrs = grow(attrs, &attr_size, attr_count + 1, sizeof *attrs);
    attrs[attr_count] = *a;
    table[i] = attr_count + 1;
    return attr_count++;
}

const vv_attr *vv_attribute(int id) { return id >= 0 && id < attr_count ? &attrs[id] : NULL; }

/* Apply one SGR parameter string, with ; or : separators. */
static void sgr(vv_attr *a, const char *params, long length) {
    long values[32];
    char separators[32];
    int n = 0;
    for (long i = 0; i <= length && n < LENGTH(values);) {
        long value = -1;
        for (; i < length && params[i] >= '0' && params[i] <= '9'; i++)
            value = (value < 0 ? 0 : value * 10) + params[i] - '0';
        values[n] = value;
        separators[n++] = i < length ? params[i] : 0;
        i++;
    }
    for (int i = 0; i < n; i++) {
        int subs = 0;
        while (i + subs + 1 < n && separators[i + subs] == ':')
            subs++;
        long code = values[i] < 0 ? 0 : values[i];
        int32_t *colour = code == 38 ? &a->fg : code == 48 ? &a->bg : code == 58 ? &a->sp : NULL;
        if (colour) {
            /* 38;5;n and 38;2;r;g;b, or 38:5:n and 38:2:[space]:r:g:b. */
            long *v = &values[i + 1];
            int left = subs ? subs : n - i - 1;
            if (left >= 2 && v[0] == 5) {
                *colour = byte(v[1]);
                i += subs ? subs : 2;
            } else if (left >= 4 && v[0] == 2) {
                long *rgb = subs ? &values[i + subs - 2] : &v[1];
                *colour = TRUE_COLOUR | byte(rgb[0]) << 16 | byte(rgb[1]) << 8 | byte(rgb[2]);
                i += subs ? subs : 4;
            } else {
                i += subs;
            }
            continue;
        }
        long sub = subs ? values[i + 1] : -1;
        i += subs;
        if (code == 0) {
            *a = (vv_attr){ -1, -1, -1, 0 };
        } else if (code == 1) {
            a->flags |= BOLD;
        } else if (code == 3) {
            a->flags |= ITALIC;
        } else if (code == 4 || code == 21) {
            long style = code == 21 ? 2 : sub < 0 ? 1 : sub;
            a->flags = (a->flags & ~(7u << UNDERLINE_SHIFT)) | (uint32_t)(style <= 5 ? style : 1) << UNDERLINE_SHIFT;
        } else if (code == 7) {
            a->flags |= REVERSE;
        } else if (code == 9) {
            a->flags |= STRIKETHROUGH;
        } else if (code == 22) {
            a->flags &= ~BOLD;
        } else if (code == 23) {
            a->flags &= ~ITALIC;
        } else if (code == 24) {
            a->flags &= ~(7u << UNDERLINE_SHIFT);
        } else if (code == 27) {
            a->flags &= ~REVERSE;
        } else if (code == 29) {
            a->flags &= ~STRIKETHROUGH;
        } else if (code >= 30 && code <= 37) {
            a->fg = code - 30;
        } else if (code == 39) {
            a->fg = -1;
        } else if (code >= 40 && code <= 47) {
            a->bg = code - 40;
        } else if (code == 49) {
            a->bg = -1;
        } else if (code == 59) {
            a->sp = -1;
        } else if (code >= 90 && code <= 97) {
            a->fg = code - 82;
        } else if (code >= 100 && code <= 107) {
            a->bg = code - 92;
        }
    }
}

/* ---------------------------------------------------------------------- */
/* Lines */

static Line *lines;
static long line_count, line_size;
static long width = 80, text_width = 80;

/* Skip the escape at raw[i]: SGR changes the attributes; hyperlinks and
 * anything else carry no text. */
static long escape(const char *raw, long length, long i, vv_attr *state, int32_t *id) {
    if (i + 1 >= length)
        return length;
    if (raw[i + 1] == '[') {
        long k = i + 2;
        while (k < length && (raw[k] < 0x40 || raw[k] > 0x7e))
            k++;
        if (k < length && raw[k] == 'm') {
            sgr(state, raw + i + 2, k - i - 2);
            *id = attr_id(state);
        }
        return k + 1;
    }
    if (raw[i + 1] == ']') {
        for (long k = i + 2; k < length; k++) {
            if (raw[k] == '\a')
                return k + 1;
            if (raw[k] == 0x1b && k + 1 < length && raw[k + 1] == '\\')
                return k + 2;
        }
        return length;
    }
    return i + 2;
}

/* Remove text[at, at + n), moving the spans after it. */
static void cut(Buf *text, vv_span *spans, long *count, long at, long n) {
    memmove(text->data + at, text->data + at + n, text->length - at - n);
    text->length -= n;
    long kept = 0;
    for (long k = 0; k < *count; k++) {
        vv_span s = spans[k];
        s.start = s.start <= at ? s.start : s.start >= at + n ? s.start - n : at;
        s.stop = s.stop <= at ? s.stop : s.stop >= at + n ? s.stop - n : at;
        if (s.stop > s.start)
            spans[kept++] = s;
    }
    *count = kept;
}

static long trailing_spaces(const Buf *text) {
    long n = 0;
    while (n < text->length && text->data[text->length - 1 - n] == ' ')
        n++;
    return n;
}

/* A line the program fitted to the pane is wider than the text beside the
 * gutter. A rule loses its end and right-aligned text moves into its widest
 * gap; the view cuts other text off. */
static void fit(Buf *text, vv_span *spans, long *count) {
    if (!text->length || (text->length <= text_width && !memchr(text->data, '\t', text->length)))
        return; /* Bytes are at least cells, except for tabs. */
    long over = cells_of(text->data, text->length) - text_width;
    if (over <= 0 || over > width - text_width)
        return;
    uint32_t rule = 0;
    bool only_rule = true;
    for (long i = 0; i < text->length && only_rule;) {
        uint32_t c;
        i += decode(text->data + i, text->length - i, &c);
        if (c != ' ' && !rule)
            rule = c;
        only_rule = c == ' ' || c == rule;
    }
    if (only_rule && (rule == '-' || rule == 0x2500 || rule == 0x2501 || rule == 0x2550) &&
        cells_of(text->data, text->length) >= 3 * text_width / 4) {
        long end = text->length;
        for (long removed = 0; removed < over && end > 0; removed++) {
            end--;
            while (end > 0 && (text->data[end] & 0xC0) == 0x80)
                end--;
        }
        cut(text, spans, count, end, text->length - end);
        cut(text, spans, count, text->length - trailing_spaces(text), trailing_spaces(text));
        return;
    }
    /* Gaps are runs of spaces after the indent and before the end. */
    long first = 0, best = -1, best_length = 0;
    while (first < text->length && text->data[first] == ' ')
        first++;
    for (long i = first; i < text->length;) {
        if (text->data[i] != ' ') {
            i++;
            continue;
        }
        long start = i;
        while (i < text->length && text->data[i] == ' ')
            i++;
        if (i < text->length && i - start - over >= 2 && i - start > best_length)
            best = start, best_length = i - start;
    }
    if (best >= 0)
        cut(text, spans, count, best + best_length - over, over);
}

static bool visible_padding(int32_t id) { return attrs[id].bg != -1 || attrs[id].flags & REVERSE; }

/* Parse one captured line. capture-pane -e writes only changes of attributes,
 * so the state carries over from the line before. */
static void parse(Line *line, const char *raw, long length, vv_attr *state, int32_t *id) {
    static Buf text;
    static vv_span *spans;
    static long span_size;
    long count = 0;
    text.length = 0;
    for (long i = 0; i < length;) {
        if (raw[i] == 0x1b) {
            i = escape(raw, length, i, state, id);
            continue;
        }
        const char *next = memchr(raw + i, 0x1b, length - i);
        long stop = next ? next - raw : length;
        if (*id && count && spans[count - 1].attr == *id && spans[count - 1].stop == text.length) {
            spans[count - 1].stop += stop - i;
        } else if (*id) {
            spans = grow(spans, &span_size, count + 2, sizeof *spans);
            spans[count++] = (vv_span){ text.length, text.length + stop - i, *id };
        }
        append(&text, raw + i, stop - i);
        i = stop;
    }
    /* tmux's rows count the padding programs write to the pane's edge; the
     * view drops it, and coloured padding becomes a fill to the edge. */
    line->cells = cells_of(text.data, text.length);
    long trimmed = text.length - trailing_spaces(&text);
    int32_t fill = 0;
    long kept = 0;
    for (long k = 0; k < count; k++) {
        if (spans[k].stop > trimmed && visible_padding(spans[k].attr))
            fill = spans[k].attr;
        if (spans[k].start < trimmed) {
            spans[k].stop = spans[k].stop < trimmed ? spans[k].stop : trimmed;
            spans[kept++] = spans[k];
        }
    }
    count = kept;
    text.length = trimmed;
    fit(&text, spans, &count);
    if (fill) {
        spans = grow(spans, &span_size, count + 1, sizeof *spans);
        spans[count++] = (vv_span){ text.length, FILL, fill };
    }
    if (!(line->text = malloc(text.length + 1)) || !(line->spans = malloc((count + 1) * sizeof *spans)))
        abort();
    memcpy(line->text, text.data ? text.data : "", text.length);
    line->text[text.length] = 0;
    line->length = text.length;
    if (count)
        memcpy(line->spans, spans, count * sizeof *spans);
    line->count = count;
}

static long rows(long i) {
    long cells = lines[i].cells;
    return cells > width ? (cells + width - 1) / width : 1;
}

/* Forget every line; the next ones are for a pane this wide, beside a gutter. */
void vv_clear(long pane_width, long view_width) {
    for (long i = 0; i < line_count; i++) {
        free(lines[i].text);
        free(lines[i].spans);
    }
    line_count = 0;
    width = pane_width > 0 ? pane_width : 1;
    text_width = view_width > 0 ? view_width : 1;
}

/* Lines in captured output. With -J, tmux ends the output without a newline
 * when its last row continues on the next. */
static long count_lines(const char *data, size_t size) {
    long n = 0;
    for (const char *q = data; size && (q = memchr(q, '\n', data + size - q)); q++)
        n++;
    return n + (size && data[size - 1] != '\n');
}

/* Parse captured output into lines from first on, replacing those, or before
 * all lines. Returns the number of lines parsed. */
long vv_parse(const char *data, size_t size, long first, int prepend) {
    const char *p = data, *end = data + size;
    long n = count_lines(data, size);
    vv_attr state = { -1, -1, -1, 0 };
    int32_t id = 0;
    if (prepend) {
        first = 0;
    } else {
        first = first < 0 || first > line_count ? line_count : first;
        for (long i = first; i < line_count; i++) {
            free(lines[i].text);
            free(lines[i].spans);
        }
        line_count = first;
    }
    lines = grow(lines, &line_size, line_count + n, sizeof *lines);
    if (prepend && n && line_count)
        memmove(lines + n, lines, line_count * sizeof *lines);
    for (long k = 0; k < n; k++) {
        const char *newline = memchr(p, '\n', end - p);
        newline = newline ? newline : end;
        parse(&lines[first + k], p, newline - p, &state, &id);
        p = newline + 1;
    }
    line_count += n;
    return n;
}

/* Colour lines from first up to before stop with output captured with
 * colours: a line of the same text takes its spans. A capture can start or end
 * inside a line, which matches no line. Returns the number of lines coloured. */
long vv_colour(const char *data, size_t size, long first, long stop) {
    const char *p = data, *end = data + size;
    long next = first < 0 ? 0 : first, coloured = 0;
    vv_attr state = { -1, -1, -1, 0 };
    int32_t id = 0;
    stop = stop < line_count ? stop : line_count;
    while (p < end && next < stop) {
        const char *newline = memchr(p, '\n', end - p);
        newline = newline ? newline : end;
        Line line;
        parse(&line, p, newline - p, &state, &id);
        p = newline + 1;
        /* Skip at most one line: a fragment at the start shifts the rest. */
        for (long k = next; k < stop && k <= next + 1; k++) {
            if (line.length == lines[k].length && !memcmp(line.text, lines[k].text, line.length)) {
                free(lines[k].spans);
                lines[k].spans = line.spans;
                lines[k].count = line.count;
                line.spans = NULL;
                next = k + 1;
                coloured++;
                break;
            }
        }
        free(line.text);
        free(line.spans);
    }
    return coloured;
}

/* The first of the lines before line stop that fit in a number of rows, at
 * least one line. Stores the rows they take. */
long vv_chunk(long stop, long most, long *taken) {
    long first = stop < line_count ? stop : line_count, total = 0;
    while (first > 0 && (total == 0 || total + rows(first - 1) <= most))
        total += rows(--first);
    *taken = total;
    return first;
}

static bool same_line(const Line *a, const Line *b) {
    return a->length == b->length && a->cells == b->cells && a->count == b->count &&
           !memcmp(a->text, b->text, a->length) && (!a->count || !memcmp(a->spans, b->spans, a->count * sizeof *a->spans));
}

static void free_line(Line *line) {
    free(line->text);
    free(line->spans);
}

/* Parse captured output as the lines from first on, keeping those that did
 * not change, at either end: lines changed up to before *old_end became
 * lines changed up to before *new_end. Returns changed. */
long vv_replace(const char *data, size_t size, long first, long *old_end, long *new_end) {
    static Line *fresh;
    static long fresh_size;
    const char *p = data, *end = data + size;
    long n = count_lines(data, size);
    vv_attr state = { -1, -1, -1, 0 };
    int32_t id = 0;
    first = first < 0 || first > line_count ? line_count : first;
    fresh = grow(fresh, &fresh_size, n, sizeof *fresh);
    for (long k = 0; k < n; k++) {
        const char *newline = memchr(p, '\n', end - p);
        newline = newline ? newline : end;
        parse(&fresh[k], p, newline - p, &state, &id);
        p = newline + 1;
    }
    long old = line_count - first, prefix = 0, suffix = 0;
    while (prefix < old && prefix < n && same_line(&lines[first + prefix], &fresh[prefix]))
        prefix++;
    while (suffix < old - prefix && suffix < n - prefix &&
           same_line(&lines[line_count - 1 - suffix], &fresh[n - 1 - suffix]))
        suffix++;
    /* The old lines stay at both ends; only the middle is new. */
    for (long k = 0; k < prefix; k++)
        free_line(&fresh[k]);
    for (long k = n - suffix; k < n; k++)
        free_line(&fresh[k]);
    long changed = first + prefix, middle = n - prefix - suffix;
    for (long i = changed; i < line_count - suffix; i++)
        free_line(&lines[i]);
    lines = grow(lines, &line_size, changed + middle + suffix, sizeof *lines);
    if (suffix)
        memmove(lines + changed + middle, lines + line_count - suffix, suffix * sizeof *lines);
    if (middle)
        memcpy(lines + changed, fresh + prefix, middle * sizeof *lines);
    *old_end = line_count - suffix;
    *new_end = changed + middle;
    line_count = changed + middle + suffix;
    return changed;
}

/* pipe-pane writes the viewed program's output to this FIFO. Opened for
 * writing too, so that it never ends when a pipe closes. */
int vv_open_fifo(const char *path) {
    unlink(path);
    if (mkfifo(path, 0600) && errno != EEXIST)
        return -1;
    return open(path, O_RDWR | O_NONBLOCK | O_CLOEXEC);
}

/* Read and drop what the FIFO holds; only its arrival matters. Returns the
 * bytes read. */
long vv_drain(int fd) {
    static char buffer[65536];
    long total = 0;
    for (;;) {
        ssize_t n = read(fd, buffer, sizeof buffer);
        if (n > 0)
            total += n;
        else if (n == 0 || errno != EINTR)
            return total;
    }
}

long vv_count(void) { return line_count; }

const char *vv_text(long i, size_t *length) {
    *length = i >= 0 && i < line_count ? lines[i].length : 0;
    return i >= 0 && i < line_count ? lines[i].text : "";
}

int vv_spans(long i, const vv_span **spans) {
    *spans = i >= 0 && i < line_count ? lines[i].spans : NULL;
    return i >= 0 && i < line_count ? lines[i].count : 0;
}

/* The first line of the last height rows, from line from on: lines before it
 * stay fixed while the program runs. Stores the row it starts on. */
long vv_tail(long from, long height, long total_rows, long *anchor) {
    long count = 0, k = line_count;
    while (k > from && count < height)
        count += rows(--k);
    *anchor = total_rows - count;
    return k;
}

/* The line holding tmux row target, counting from line first on row anchor.
 * Stores the cells before that row in the line. */
long vv_locate(long first, long anchor, long target, long *offset) {
    long line = first, row = anchor;
    while (line < line_count - 1 && row + rows(line) <= target)
        row += rows(line++);
    *offset = (target - row) * width;
    return line;
}
