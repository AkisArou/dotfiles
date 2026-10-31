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

/* Cell widths, from Python's unicodedata for Unicode 16.0.0: East Asian
 * wide and full-width characters take two cells, combining marks and format
 * characters none. A range lasts from its start to the next one's. */
enum { ZERO_WIDTH = 1, WIDE = 2 };
static const unsigned char ascii_properties[128] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};

static const struct { unsigned start : 24, properties : 8; } unicode_ranges[] = {
    {0x80, 0}, {0xAD, 1}, {0xAE, 0}, {0x300, 1}, {0x370, 0}, {0x483, 1}, {0x48A, 0}, {0x591, 1},
    {0x5BE, 0}, {0x5BF, 1}, {0x5C0, 0}, {0x5C1, 1}, {0x5C3, 0}, {0x5C4, 1}, {0x5C6, 0}, {0x5C7, 1},
    {0x5C8, 0}, {0x600, 1}, {0x606, 0}, {0x610, 1}, {0x61B, 0}, {0x61C, 1}, {0x61D, 0}, {0x64B, 1},
    {0x660, 0}, {0x670, 1}, {0x671, 0}, {0x6D6, 1}, {0x6DE, 0}, {0x6DF, 1}, {0x6E5, 0}, {0x6E7, 1},
    {0x6E9, 0}, {0x6EA, 1}, {0x6EE, 0}, {0x70F, 1}, {0x710, 0}, {0x711, 1}, {0x712, 0}, {0x730, 1},
    {0x74B, 0}, {0x7A6, 1}, {0x7B1, 0}, {0x7EB, 1}, {0x7F4, 0}, {0x7FD, 1}, {0x7FE, 0}, {0x816, 1},
    {0x81A, 0}, {0x81B, 1}, {0x824, 0}, {0x825, 1}, {0x828, 0}, {0x829, 1}, {0x82E, 0}, {0x859, 1},
    {0x85C, 0}, {0x890, 1}, {0x892, 0}, {0x897, 1}, {0x8A0, 0}, {0x8CA, 1}, {0x903, 0}, {0x93A, 1},
    {0x93B, 0}, {0x93C, 1}, {0x93D, 0}, {0x941, 1}, {0x949, 0}, {0x94D, 1}, {0x94E, 0}, {0x951, 1},
    {0x958, 0}, {0x962, 1}, {0x964, 0}, {0x981, 1}, {0x982, 0}, {0x9BC, 1}, {0x9BD, 0}, {0x9C1, 1},
    {0x9C5, 0}, {0x9CD, 1}, {0x9CE, 0}, {0x9E2, 1}, {0x9E4, 0}, {0x9FE, 1}, {0x9FF, 0}, {0xA01, 1},
    {0xA03, 0}, {0xA3C, 1}, {0xA3D, 0}, {0xA41, 1}, {0xA43, 0}, {0xA47, 1}, {0xA49, 0}, {0xA4B, 1},
    {0xA4E, 0}, {0xA51, 1}, {0xA52, 0}, {0xA70, 1}, {0xA72, 0}, {0xA75, 1}, {0xA76, 0}, {0xA81, 1},
    {0xA83, 0}, {0xABC, 1}, {0xABD, 0}, {0xAC1, 1}, {0xAC6, 0}, {0xAC7, 1}, {0xAC9, 0}, {0xACD, 1},
    {0xACE, 0}, {0xAE2, 1}, {0xAE4, 0}, {0xAFA, 1}, {0xB00, 0}, {0xB01, 1}, {0xB02, 0}, {0xB3C, 1},
    {0xB3D, 0}, {0xB3F, 1}, {0xB40, 0}, {0xB41, 1}, {0xB45, 0}, {0xB4D, 1}, {0xB4E, 0}, {0xB55, 1},
    {0xB57, 0}, {0xB62, 1}, {0xB64, 0}, {0xB82, 1}, {0xB83, 0}, {0xBC0, 1}, {0xBC1, 0}, {0xBCD, 1},
    {0xBCE, 0}, {0xC00, 1}, {0xC01, 0}, {0xC04, 1}, {0xC05, 0}, {0xC3C, 1}, {0xC3D, 0}, {0xC3E, 1},
    {0xC41, 0}, {0xC46, 1}, {0xC49, 0}, {0xC4A, 1}, {0xC4E, 0}, {0xC55, 1}, {0xC57, 0}, {0xC62, 1},
    {0xC64, 0}, {0xC81, 1}, {0xC82, 0}, {0xCBC, 1}, {0xCBD, 0}, {0xCBF, 1}, {0xCC0, 0}, {0xCC6, 1},
    {0xCC7, 0}, {0xCCC, 1}, {0xCCE, 0}, {0xCE2, 1}, {0xCE4, 0}, {0xD00, 1}, {0xD02, 0}, {0xD3B, 1},
    {0xD3D, 0}, {0xD41, 1}, {0xD45, 0}, {0xD4D, 1}, {0xD4E, 0}, {0xD62, 1}, {0xD64, 0}, {0xD81, 1},
    {0xD82, 0}, {0xDCA, 1}, {0xDCB, 0}, {0xDD2, 1}, {0xDD5, 0}, {0xDD6, 1}, {0xDD7, 0}, {0xE31, 1},
    {0xE32, 0}, {0xE34, 1}, {0xE3B, 0}, {0xE47, 1}, {0xE4F, 0}, {0xEB1, 1}, {0xEB2, 0}, {0xEB4, 1},
    {0xEBD, 0}, {0xEC8, 1}, {0xECF, 0}, {0xF18, 1}, {0xF1A, 0}, {0xF35, 1}, {0xF36, 0}, {0xF37, 1},
    {0xF38, 0}, {0xF39, 1}, {0xF3A, 0}, {0xF71, 1}, {0xF7F, 0}, {0xF80, 1}, {0xF85, 0}, {0xF86, 1},
    {0xF88, 0}, {0xF8D, 1}, {0xF98, 0}, {0xF99, 1}, {0xFBD, 0}, {0xFC6, 1}, {0xFC7, 0}, {0x102D, 1},
    {0x1031, 0}, {0x1032, 1}, {0x1038, 0}, {0x1039, 1}, {0x103B, 0}, {0x103D, 1}, {0x103F, 0}, {0x1058, 1},
    {0x105A, 0}, {0x105E, 1}, {0x1061, 0}, {0x1071, 1}, {0x1075, 0}, {0x1082, 1}, {0x1083, 0}, {0x1085, 1},
    {0x1087, 0}, {0x108D, 1}, {0x108E, 0}, {0x109D, 1}, {0x109E, 0}, {0x1100, 2}, {0x1160, 0}, {0x135D, 1},
    {0x1360, 0}, {0x1712, 1}, {0x1716, 0}, {0x1732, 1}, {0x1735, 0}, {0x1752, 1}, {0x1754, 0}, {0x1772, 1},
    {0x1774, 0}, {0x17B4, 1}, {0x17B6, 0}, {0x17B7, 1}, {0x17BE, 0}, {0x17C6, 1}, {0x17C7, 0}, {0x17C9, 1},
    {0x17D4, 0}, {0x17DD, 1}, {0x17DE, 0}, {0x180B, 1}, {0x1810, 0}, {0x1885, 1}, {0x1887, 0}, {0x18A9, 1},
    {0x18AA, 0}, {0x1920, 1}, {0x1923, 0}, {0x1927, 1}, {0x1929, 0}, {0x1932, 1}, {0x1933, 0}, {0x1939, 1},
    {0x193C, 0}, {0x1A17, 1}, {0x1A19, 0}, {0x1A1B, 1}, {0x1A1C, 0}, {0x1A56, 1}, {0x1A57, 0}, {0x1A58, 1},
    {0x1A5F, 0}, {0x1A60, 1}, {0x1A61, 0}, {0x1A62, 1}, {0x1A63, 0}, {0x1A65, 1}, {0x1A6D, 0}, {0x1A73, 1},
    {0x1A7D, 0}, {0x1A7F, 1}, {0x1A80, 0}, {0x1AB0, 1}, {0x1ACF, 0}, {0x1B00, 1}, {0x1B04, 0}, {0x1B34, 1},
    {0x1B35, 0}, {0x1B36, 1}, {0x1B3B, 0}, {0x1B3C, 1}, {0x1B3D, 0}, {0x1B42, 1}, {0x1B43, 0}, {0x1B44, 1},
    {0x1B45, 0}, {0x1B6B, 1}, {0x1B74, 0}, {0x1B80, 1}, {0x1B82, 0}, {0x1BA2, 1}, {0x1BA6, 0}, {0x1BA8, 1},
    {0x1BAE, 0}, {0x1BE6, 1}, {0x1BE7, 0}, {0x1BE8, 1}, {0x1BEA, 0}, {0x1BED, 1}, {0x1BEE, 0}, {0x1BEF, 1},
    {0x1BF4, 0}, {0x1C2C, 1}, {0x1C34, 0}, {0x1C36, 1}, {0x1C38, 0}, {0x1CD0, 1}, {0x1CD3, 0}, {0x1CD4, 1},
    {0x1CE1, 0}, {0x1CE2, 1}, {0x1CE9, 0}, {0x1CED, 1}, {0x1CEE, 0}, {0x1CF4, 1}, {0x1CF5, 0}, {0x1CF8, 1},
    {0x1CFA, 0}, {0x1DC0, 1}, {0x1E00, 0}, {0x200B, 1}, {0x2010, 0}, {0x202A, 1}, {0x202F, 0}, {0x2060, 1},
    {0x2065, 0}, {0x2066, 1}, {0x2070, 0}, {0x20D0, 1}, {0x20F1, 0}, {0x231A, 2}, {0x231C, 0}, {0x2329, 2},
    {0x232B, 0}, {0x23E9, 2}, {0x23ED, 0}, {0x23F0, 2}, {0x23F1, 0}, {0x23F3, 2}, {0x23F4, 0}, {0x25FD, 2},
    {0x25FF, 0}, {0x2614, 2}, {0x2616, 0}, {0x2630, 2}, {0x2638, 0}, {0x2648, 2}, {0x2654, 0}, {0x267F, 2},
    {0x2680, 0}, {0x268A, 2}, {0x2690, 0}, {0x2693, 2}, {0x2694, 0}, {0x26A1, 2}, {0x26A2, 0}, {0x26AA, 2},
    {0x26AC, 0}, {0x26BD, 2}, {0x26BF, 0}, {0x26C4, 2}, {0x26C6, 0}, {0x26CE, 2}, {0x26CF, 0}, {0x26D4, 2},
    {0x26D5, 0}, {0x26EA, 2}, {0x26EB, 0}, {0x26F2, 2}, {0x26F4, 0}, {0x26F5, 2}, {0x26F6, 0}, {0x26FA, 2},
    {0x26FB, 0}, {0x26FD, 2}, {0x26FE, 0}, {0x2705, 2}, {0x2706, 0}, {0x270A, 2}, {0x270C, 0}, {0x2728, 2},
    {0x2729, 0}, {0x274C, 2}, {0x274D, 0}, {0x274E, 2}, {0x274F, 0}, {0x2753, 2}, {0x2756, 0}, {0x2757, 2},
    {0x2758, 0}, {0x2795, 2}, {0x2798, 0}, {0x27B0, 2}, {0x27B1, 0}, {0x27BF, 2}, {0x27C0, 0}, {0x2B1B, 2},
    {0x2B1D, 0}, {0x2B50, 2}, {0x2B51, 0}, {0x2B55, 2}, {0x2B56, 0}, {0x2CEF, 1}, {0x2CF2, 0}, {0x2D7F, 1},
    {0x2D80, 0}, {0x2DE0, 1}, {0x2E00, 0}, {0x2E80, 2}, {0x2E9A, 0}, {0x2E9B, 2}, {0x2EF4, 0}, {0x2F00, 2},
    {0x2FD6, 0}, {0x2FF0, 2}, {0x302A, 1}, {0x3030, 2}, {0x303F, 0}, {0x3041, 2}, {0x3097, 0}, {0x3099, 1},
    {0x309B, 2}, {0x3100, 0}, {0x3105, 2}, {0x3130, 0}, {0x3131, 2}, {0x318F, 0}, {0x3190, 2}, {0x31E6, 0},
    {0x31EF, 2}, {0x321F, 0}, {0x3220, 2}, {0x3248, 0}, {0x3250, 2}, {0xA48D, 0}, {0xA490, 2}, {0xA4C7, 0},
    {0xA66F, 1}, {0xA673, 0}, {0xA674, 1}, {0xA67E, 0}, {0xA69E, 1}, {0xA6A0, 0}, {0xA6F0, 1}, {0xA6F2, 0},
    {0xA802, 1}, {0xA803, 0}, {0xA806, 1}, {0xA807, 0}, {0xA80B, 1}, {0xA80C, 0}, {0xA825, 1}, {0xA827, 0},
    {0xA82C, 1}, {0xA82D, 0}, {0xA8C4, 1}, {0xA8C6, 0}, {0xA8E0, 1}, {0xA8F2, 0}, {0xA8FF, 1}, {0xA900, 0},
    {0xA926, 1}, {0xA92E, 0}, {0xA947, 1}, {0xA952, 0}, {0xA953, 1}, {0xA954, 0}, {0xA960, 2}, {0xA97D, 0},
    {0xA980, 1}, {0xA983, 0}, {0xA9B3, 1}, {0xA9B4, 0}, {0xA9B6, 1}, {0xA9BA, 0}, {0xA9BC, 1}, {0xA9BE, 0},
    {0xA9C0, 1}, {0xA9C1, 0}, {0xA9E5, 1}, {0xA9E6, 0}, {0xAA29, 1}, {0xAA2F, 0}, {0xAA31, 1}, {0xAA33, 0},
    {0xAA35, 1}, {0xAA37, 0}, {0xAA43, 1}, {0xAA44, 0}, {0xAA4C, 1}, {0xAA4D, 0}, {0xAA7C, 1}, {0xAA7D, 0},
    {0xAAB0, 1}, {0xAAB1, 0}, {0xAAB2, 1}, {0xAAB5, 0}, {0xAAB7, 1}, {0xAAB9, 0}, {0xAABE, 1}, {0xAAC0, 0},
    {0xAAC1, 1}, {0xAAC2, 0}, {0xAAEC, 1}, {0xAAEE, 0}, {0xAAF6, 1}, {0xAAF7, 0}, {0xABE5, 1}, {0xABE6, 0},
    {0xABE8, 1}, {0xABE9, 0}, {0xABED, 1}, {0xABEE, 0}, {0xAC00, 2}, {0xD7A4, 0}, {0xF900, 2}, {0xFB00, 0},
    {0xFB1E, 1}, {0xFB1F, 0}, {0xFE00, 1}, {0xFE10, 2}, {0xFE1A, 0}, {0xFE20, 1}, {0xFE30, 2}, {0xFE53, 0},
    {0xFE54, 2}, {0xFE67, 0}, {0xFE68, 2}, {0xFE6C, 0}, {0xFEFF, 1}, {0xFF00, 0}, {0xFF01, 2}, {0xFF61, 0},
    {0xFFE0, 2}, {0xFFE7, 0}, {0xFFF9, 1}, {0xFFFC, 0}, {0x101FD, 1}, {0x101FE, 0}, {0x102E0, 1}, {0x102E1, 0},
    {0x10376, 1}, {0x1037B, 0}, {0x10A01, 1}, {0x10A04, 0}, {0x10A05, 1}, {0x10A07, 0}, {0x10A0C, 1}, {0x10A10, 0},
    {0x10A38, 1}, {0x10A3B, 0}, {0x10A3F, 1}, {0x10A40, 0}, {0x10AE5, 1}, {0x10AE7, 0}, {0x10D24, 1}, {0x10D28, 0},
    {0x10D69, 1}, {0x10D6E, 0}, {0x10EAB, 1}, {0x10EAD, 0}, {0x10EFC, 1}, {0x10F00, 0}, {0x10F46, 1}, {0x10F51, 0},
    {0x10F82, 1}, {0x10F86, 0}, {0x11001, 1}, {0x11002, 0}, {0x11038, 1}, {0x11047, 0}, {0x11070, 1}, {0x11071, 0},
    {0x11073, 1}, {0x11075, 0}, {0x1107F, 1}, {0x11082, 0}, {0x110B3, 1}, {0x110B7, 0}, {0x110B9, 1}, {0x110BB, 0},
    {0x110BD, 1}, {0x110BE, 0}, {0x110C2, 1}, {0x110C3, 0}, {0x110CD, 1}, {0x110CE, 0}, {0x11100, 1}, {0x11103, 0},
    {0x11127, 1}, {0x1112C, 0}, {0x1112D, 1}, {0x11135, 0}, {0x11173, 1}, {0x11174, 0}, {0x11180, 1}, {0x11182, 0},
    {0x111B6, 1}, {0x111BF, 0}, {0x111C0, 1}, {0x111C1, 0}, {0x111C9, 1}, {0x111CD, 0}, {0x111CF, 1}, {0x111D0, 0},
    {0x1122F, 1}, {0x11232, 0}, {0x11234, 1}, {0x11238, 0}, {0x1123E, 1}, {0x1123F, 0}, {0x11241, 1}, {0x11242, 0},
    {0x112DF, 1}, {0x112E0, 0}, {0x112E3, 1}, {0x112EB, 0}, {0x11300, 1}, {0x11302, 0}, {0x1133B, 1}, {0x1133D, 0},
    {0x11340, 1}, {0x11341, 0}, {0x1134D, 1}, {0x1134E, 0}, {0x11366, 1}, {0x1136D, 0}, {0x11370, 1}, {0x11375, 0},
    {0x113BB, 1}, {0x113C1, 0}, {0x113CE, 1}, {0x113D1, 0}, {0x113D2, 1}, {0x113D3, 0}, {0x113E1, 1}, {0x113E3, 0},
    {0x11438, 1}, {0x11440, 0}, {0x11442, 1}, {0x11445, 0}, {0x11446, 1}, {0x11447, 0}, {0x1145E, 1}, {0x1145F, 0},
    {0x114B3, 1}, {0x114B9, 0}, {0x114BA, 1}, {0x114BB, 0}, {0x114BF, 1}, {0x114C1, 0}, {0x114C2, 1}, {0x114C4, 0},
    {0x115B2, 1}, {0x115B6, 0}, {0x115BC, 1}, {0x115BE, 0}, {0x115BF, 1}, {0x115C1, 0}, {0x115DC, 1}, {0x115DE, 0},
    {0x11633, 1}, {0x1163B, 0}, {0x1163D, 1}, {0x1163E, 0}, {0x1163F, 1}, {0x11641, 0}, {0x116AB, 1}, {0x116AC, 0},
    {0x116AD, 1}, {0x116AE, 0}, {0x116B0, 1}, {0x116B8, 0}, {0x1171D, 1}, {0x1171E, 0}, {0x1171F, 1}, {0x11720, 0},
    {0x11722, 1}, {0x11726, 0}, {0x11727, 1}, {0x1172C, 0}, {0x1182F, 1}, {0x11838, 0}, {0x11839, 1}, {0x1183B, 0},
    {0x1193B, 1}, {0x1193F, 0}, {0x11943, 1}, {0x11944, 0}, {0x119D4, 1}, {0x119D8, 0}, {0x119DA, 1}, {0x119DC, 0},
    {0x119E0, 1}, {0x119E1, 0}, {0x11A01, 1}, {0x11A0B, 0}, {0x11A33, 1}, {0x11A39, 0}, {0x11A3B, 1}, {0x11A3F, 0},
    {0x11A47, 1}, {0x11A48, 0}, {0x11A51, 1}, {0x11A57, 0}, {0x11A59, 1}, {0x11A5C, 0}, {0x11A8A, 1}, {0x11A97, 0},
    {0x11A98, 1}, {0x11A9A, 0}, {0x11C30, 1}, {0x11C37, 0}, {0x11C38, 1}, {0x11C3E, 0}, {0x11C3F, 1}, {0x11C40, 0},
    {0x11C92, 1}, {0x11CA8, 0}, {0x11CAA, 1}, {0x11CB1, 0}, {0x11CB2, 1}, {0x11CB4, 0}, {0x11CB5, 1}, {0x11CB7, 0},
    {0x11D31, 1}, {0x11D37, 0}, {0x11D3A, 1}, {0x11D3B, 0}, {0x11D3C, 1}, {0x11D3E, 0}, {0x11D3F, 1}, {0x11D46, 0},
    {0x11D47, 1}, {0x11D48, 0}, {0x11D90, 1}, {0x11D92, 0}, {0x11D95, 1}, {0x11D96, 0}, {0x11D97, 1}, {0x11D98, 0},
    {0x11EF3, 1}, {0x11EF5, 0}, {0x11F00, 1}, {0x11F02, 0}, {0x11F36, 1}, {0x11F3B, 0}, {0x11F40, 1}, {0x11F43, 0},
    {0x11F5A, 1}, {0x11F5B, 0}, {0x13430, 1}, {0x13441, 0}, {0x13447, 1}, {0x13456, 0}, {0x1611E, 1}, {0x1612A, 0},
    {0x1612D, 1}, {0x16130, 0}, {0x16AF0, 1}, {0x16AF5, 0}, {0x16B30, 1}, {0x16B37, 0}, {0x16F4F, 1}, {0x16F50, 0},
    {0x16F8F, 1}, {0x16F93, 0}, {0x16FE0, 2}, {0x16FE4, 1}, {0x16FE5, 0}, {0x16FF0, 1}, {0x16FF2, 0}, {0x17000, 2},
    {0x187F8, 0}, {0x18800, 2}, {0x18CD6, 0}, {0x18CFF, 2}, {0x18D09, 0}, {0x1AFF0, 2}, {0x1AFF4, 0}, {0x1AFF5, 2},
    {0x1AFFC, 0}, {0x1AFFD, 2}, {0x1AFFF, 0}, {0x1B000, 2}, {0x1B123, 0}, {0x1B132, 2}, {0x1B133, 0}, {0x1B150, 2},
    {0x1B153, 0}, {0x1B155, 2}, {0x1B156, 0}, {0x1B164, 2}, {0x1B168, 0}, {0x1B170, 2}, {0x1B2FC, 0}, {0x1BC9D, 1},
    {0x1BC9F, 0}, {0x1BCA0, 1}, {0x1BCA4, 0}, {0x1CF00, 1}, {0x1CF2E, 0}, {0x1CF30, 1}, {0x1CF47, 0}, {0x1D165, 1},
    {0x1D16A, 0}, {0x1D16D, 1}, {0x1D183, 0}, {0x1D185, 1}, {0x1D18C, 0}, {0x1D1AA, 1}, {0x1D1AE, 0}, {0x1D242, 1},
    {0x1D245, 0}, {0x1D300, 2}, {0x1D357, 0}, {0x1D360, 2}, {0x1D377, 0}, {0x1DA00, 1}, {0x1DA37, 0}, {0x1DA3B, 1},
    {0x1DA6D, 0}, {0x1DA75, 1}, {0x1DA76, 0}, {0x1DA84, 1}, {0x1DA85, 0}, {0x1DA9B, 1}, {0x1DAA0, 0}, {0x1DAA1, 1},
    {0x1DAB0, 0}, {0x1E000, 1}, {0x1E007, 0}, {0x1E008, 1}, {0x1E019, 0}, {0x1E01B, 1}, {0x1E022, 0}, {0x1E023, 1},
    {0x1E025, 0}, {0x1E026, 1}, {0x1E02B, 0}, {0x1E08F, 1}, {0x1E090, 0}, {0x1E130, 1}, {0x1E137, 0}, {0x1E2AE, 1},
    {0x1E2AF, 0}, {0x1E2EC, 1}, {0x1E2F0, 0}, {0x1E4EC, 1}, {0x1E4F0, 0}, {0x1E5EE, 1}, {0x1E5F0, 0}, {0x1E8D0, 1},
    {0x1E8D7, 0}, {0x1E944, 1}, {0x1E94B, 0}, {0x1F004, 2}, {0x1F005, 0}, {0x1F0CF, 2}, {0x1F0D0, 0}, {0x1F18E, 2},
    {0x1F18F, 0}, {0x1F191, 2}, {0x1F19B, 0}, {0x1F200, 2}, {0x1F203, 0}, {0x1F210, 2}, {0x1F23C, 0}, {0x1F240, 2},
    {0x1F249, 0}, {0x1F250, 2}, {0x1F252, 0}, {0x1F260, 2}, {0x1F266, 0}, {0x1F300, 2}, {0x1F321, 0}, {0x1F32D, 2},
    {0x1F336, 0}, {0x1F337, 2}, {0x1F37D, 0}, {0x1F37E, 2}, {0x1F394, 0}, {0x1F3A0, 2}, {0x1F3CB, 0}, {0x1F3CF, 2},
    {0x1F3D4, 0}, {0x1F3E0, 2}, {0x1F3F1, 0}, {0x1F3F4, 2}, {0x1F3F5, 0}, {0x1F3F8, 2}, {0x1F43F, 0}, {0x1F440, 2},
    {0x1F441, 0}, {0x1F442, 2}, {0x1F4FD, 0}, {0x1F4FF, 2}, {0x1F53E, 0}, {0x1F54B, 2}, {0x1F54F, 0}, {0x1F550, 2},
    {0x1F568, 0}, {0x1F57A, 2}, {0x1F57B, 0}, {0x1F595, 2}, {0x1F597, 0}, {0x1F5A4, 2}, {0x1F5A5, 0}, {0x1F5FB, 2},
    {0x1F650, 0}, {0x1F680, 2}, {0x1F6C6, 0}, {0x1F6CC, 2}, {0x1F6CD, 0}, {0x1F6D0, 2}, {0x1F6D3, 0}, {0x1F6D5, 2},
    {0x1F6D8, 0}, {0x1F6DC, 2}, {0x1F6E0, 0}, {0x1F6EB, 2}, {0x1F6ED, 0}, {0x1F6F4, 2}, {0x1F6FD, 0}, {0x1F7E0, 2},
    {0x1F7EC, 0}, {0x1F7F0, 2}, {0x1F7F1, 0}, {0x1F90C, 2}, {0x1F93B, 0}, {0x1F93C, 2}, {0x1F946, 0}, {0x1F947, 2},
    {0x1FA00, 0}, {0x1FA70, 2}, {0x1FA7D, 0}, {0x1FA80, 2}, {0x1FA8A, 0}, {0x1FA8F, 2}, {0x1FAC7, 0}, {0x1FACE, 2},
    {0x1FADD, 0}, {0x1FADF, 2}, {0x1FAEA, 0}, {0x1FAF0, 2}, {0x1FAF9, 0}, {0x20000, 2}, {0x2FFFE, 0}, {0x30000, 2},
    {0x3FFFE, 0}, {0xE0001, 1}, {0xE0002, 0}, {0xE0020, 1}, {0xE0080, 0}, {0xE0100, 1}, {0xE01F0, 0},
};

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
