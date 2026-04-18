/* Data.Text shims for Frankenstein self-hosted binary.
 *
 * Frankenstein strings are already kk_string ropes (UTF-8).
 * Data.Text in the compiled IR is just int64_t holding a kk_string pointer.
 * Most operations flatten the rope, do C string work, and re-wrap.
 *
 * Required symbols (from nm -u), 54 total:
 *   Data_Text_all$2, Data_Text_any$2, Data_Text_breakOn$2,
 *   Data_Text_breakOn$3, Data_Text_breakOnEnd$3, Data_Text_concat$1,
 *   Data_Text_drop$1, Data_Text_drop$2, Data_Text_dropEnd$1,
 *   Data_Text_dropEnd$2, Data_Text_dropWhile$1, Data_Text_dropWhile$2,
 *   Data_Text_dropWhileEnd$2, Data_Text_Encoding_decodeUtf8With$2,
 *   Data_Text_Encoding_encodeUtf8$1, Data_Text_Encoding_Error_lenientDecode$0,
 *   Data_Text_init$1, Data_Text_intercalate$1, Data_Text_intercalate$2,
 *   Data_Text_Internal_empty$0, Data_Text_Internal_pack$0,
 *   Data_Text_Internal_pack$1, Data_Text_IO_readFile$1,
 *   Data_Text_IO_writeFile$2, Data_Text_isInfixOf$1, Data_Text_isInfixOf$2,
 *   Data_Text_isPrefixOf$1, Data_Text_isPrefixOf$2, Data_Text_isSuffixOf$2,
 *   Data_Text_last$1, Data_Text_length$1, Data_Text_lines$1,
 *   Data_Text_map$1, Data_Text_null$0, Data_Text_null$1,
 *   Data_Text_Show_singleton$1, Data_Text_Show_unpack$1,
 *   Data_Text_span$2, Data_Text_splitOn$2, Data_Text_splitOn$3,
 *   Data_Text_strip$0, Data_Text_strip$1, Data_Text_stripEnd$1,
 *   Data_Text_stripPrefix$2, Data_Text_stripStart$0, Data_Text_stripStart$1,
 *   Data_Text_take$2, Data_Text_takeWhile$1, Data_Text_takeWhile$2,
 *   Data_Text_uncons$1, Data_Text_unlines$0, Data_Text_unlines$1,
 *   Data_ByteString_length$1, Data_ByteString_unpack$1
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "../runtime/kk_runtime.h"

/* O(1) string slicing — returns a view into parent's buffer */
extern int64_t kk_str_slice(int64_t parent, int64_t byte_offset, int64_t byte_len);

/* ------------------------------------------------------------------ */
/*  Internal helpers                                                    */
/* ------------------------------------------------------------------ */

/* Flatten a kk_string rope to a malloc'd NUL-terminated C string.
 * Stores the byte length in *out_len.  Caller must free the result. */
static char* text_flatten(int64_t s, int64_t *out_len) {
    char* cstr = kk_str_dup_cstr(s);
    if (!cstr) { *out_len = 0; return NULL; }
    *out_len = kk_str_len(s);
    return cstr;
}

/* Borrow: flatten the kk_string rope in-place (via kk_str_flatten) and
 * return a pointer to the leaf's internal bytes WITHOUT copying.
 * The caller must NOT free the result — the string owns the memory.
 * This is O(n) on first call for a rope but O(1) for already-flat leaves. */
#define KK_STR_SLICE 2
typedef struct { int64_t rc; int64_t byte_len; int32_t kind; int32_t owns; union { const char* bytes; struct { void* l; void* r; } cat; } u; } kk_str_hdr_t;
static const char* text_borrow(int64_t s, int64_t *out_len) {
    if (!s) { *out_len = 0; return ""; }
    int64_t flat = kk_str_flatten(s);
    kk_str_hdr_t* h = (kk_str_hdr_t*)flat;
    *out_len = h->byte_len;
    /* Slices store bytes pointer in cat.r, not u.bytes */
    if (h->kind == KK_STR_SLICE) return (const char*)h->u.cat.r;
    return h->u.bytes;
}

/* Build a kk_string slice from a flat parent at a byte offset. O(1). */
static int64_t text_slice(int64_t flat_parent, int64_t byte_offset, int64_t byte_len) {
    return kk_str_slice(flat_parent, byte_offset, byte_len);
}

/* Build a kk_string from a C buffer of byte_len bytes (copies buf). */
static int64_t text_from_buf(const char* buf, int64_t byte_len) {
    if (byte_len <= 0) return kk_string_empty();
    char* owned = (char*)malloc((size_t)byte_len + 1);
    if (!owned) return kk_string_empty();
    memcpy(owned, buf, (size_t)byte_len);
    owned[byte_len] = '\0';
    return kk_str_alloc_leaf_owned(owned, byte_len);
}

/* Count UTF-8 codepoints in [p, p+len). */
static int64_t utf8_char_count(const char* p, int64_t len) {
    int64_t count = 0;
    for (int64_t i = 0; i < len; i++) {
        if (((unsigned char)p[i] & 0xC0) != 0x80) count++;
    }
    return count;
}

/* Advance past n codepoints in [p, p+max_bytes).  Returns byte offset. */
static int64_t utf8_advance(const char* p, int64_t max_bytes, int64_t n) {
    int64_t off = 0, count = 0;
    while (off < max_bytes && count < n) {
        unsigned char c = (unsigned char)p[off];
        if      (c < 0x80)                 off += 1;
        else if ((c & 0xE0) == 0xC0)       off += 2;
        else if ((c & 0xF0) == 0xE0)       off += 3;
        else                                off += 4;
        count++;
    }
    return off;
}

/* Decode the codepoint at p[0..].  Returns the codepoint and sets *bytes_used. */
static int64_t utf8_decode(const char* p, int64_t *bytes_used) {
    unsigned char c = (unsigned char)p[0];
    if (c < 0x80) { *bytes_used = 1; return c; }
    if ((c & 0xE0) == 0xC0) { *bytes_used = 2; return ((c & 0x1F) << 6) | (p[1] & 0x3F); }
    if ((c & 0xF0) == 0xE0) { *bytes_used = 3; return ((c & 0x0F) << 12) | ((p[1] & 0x3F) << 6) | (p[2] & 0x3F); }
    *bytes_used = 4;
    return ((c & 0x07) << 18) | ((p[1] & 0x3F) << 12) | ((p[2] & 0x3F) << 6) | (p[3] & 0x3F);
}

/* Encode a codepoint to buf, returning bytes written (1-4). */
static int utf8_encode(int64_t cp, char* buf) {
    if (cp < 0x80)    { buf[0] = (char)cp; return 1; }
    if (cp < 0x800)   { buf[0] = (char)(0xC0 | (cp >> 6)); buf[1] = (char)(0x80 | (cp & 0x3F)); return 2; }
    if (cp < 0x10000) { buf[0] = (char)(0xE0 | (cp >> 12)); buf[1] = (char)(0x80 | ((cp >> 6) & 0x3F)); buf[2] = (char)(0x80 | (cp & 0x3F)); return 3; }
    buf[0] = (char)(0xF0 | (cp >> 18)); buf[1] = (char)(0x80 | ((cp >> 12) & 0x3F)); buf[2] = (char)(0x80 | ((cp >> 6) & 0x3F)); buf[3] = (char)(0x80 | (cp & 0x3F)); return 4;
}

/* Box a Char codepoint: compiled Haskell code expects Char values as
 * heap-allocated constructors with the codepoint in field 0.
 * The tag doesn't matter (code uses kk_field(x,0) to unbox), but we
 * use C# = stableConTag "C#" = 30786 for consistency. */
#define CHAR_BOX_TAG 30786
static int64_t box_char(int64_t cp) {
    int64_t c = kk_alloc_con(CHAR_BOX_TAG, 1);
    kk_set_field(c, 0, cp);
    return c;
}
static int64_t unbox_char(int64_t c) {
    if (kk_is_heap_ptr(c) && kk_tag(c) == CHAR_BOX_TAG)
        return kk_field(c, 0);
    return c;
}

/* Call a Haskell closure: field 0 = fn ptr.
 * For predicates (Char -> Bool), the closure takes (closure, char_codepoint)
 * and returns 0/1. */
static int64_t call_closure_1(int64_t closure, int64_t arg) {
    /* Retain arg — closure may consume it via Perceus drop */
    kk_retain(arg);
    closure = kk_thunk_force(closure);
    if (!kk_is_heap_ptr(closure)) {
        typedef int64_t (*raw1_t)(int64_t);
        return ((raw1_t)(intptr_t)closure)(arg);
    }
    int64_t fn_ptr = kk_field(closure, 0);
    typedef int64_t (*fn1_t)(int64_t, int64_t);
    return ((fn1_t)fn_ptr)(closure, arg);
}

/* Find needle (buf_n, len_n) in haystack (buf_h, len_h).
 * Returns byte offset or -1. */
static int64_t memmem_offset(const char* h, int64_t lh,
                              const char* n, int64_t ln) {
    if (ln == 0) return 0;
    if (ln > lh) return -1;
    for (int64_t i = 0; i <= lh - ln; i++) {
        if (memcmp(h + i, n, (size_t)ln) == 0) return i;
    }
    return -1;
}

/* ------------------------------------------------------------------ */
/*  Text operations (static)                                           */
/* ------------------------------------------------------------------ */

static int64_t text_null(int64_t s) {
    return kk_str_len(s) == 0 ? 1 : 0;
}

static int64_t text_length(int64_t s) {
    return kk_str_char_len(s);
}

static int64_t text_take(int64_t n, int64_t s) {
    int64_t flat = kk_str_flatten(s);
    int64_t len;
    const char* buf = text_borrow(flat, &len);
    if (len == 0) return kk_string_empty();
    int64_t byte_off = utf8_advance(buf, len, n);
    return text_slice(flat, 0, byte_off);
}

static int64_t text_drop(int64_t n, int64_t s) {
    int64_t flat = kk_str_flatten(s);
    int64_t len;
    const char* buf = text_borrow(flat, &len);
    if (len == 0) return kk_string_empty();
    int64_t byte_off = utf8_advance(buf, len, n);
    return text_slice(flat, byte_off, len - byte_off);
}

static int64_t text_drop_end(int64_t n, int64_t s) {
    /* Drop n chars from the end = take (length - n) */
    int64_t total = text_length(s);
    int64_t keep = total - n;
    if (keep <= 0) return kk_string_empty();
    return text_take(keep, s);
}

static int64_t text_init(int64_t s) {
    /* All but last char = take (length - 1) */
    int64_t total = text_length(s);
    if (total <= 0) return kk_string_empty();
    return text_take(total - 1, s);
}

static int64_t text_last(int64_t s) {
    int64_t len;
    const char* buf = text_borrow(s, &len);
    if (len == 0) return 0;
    /* Walk backwards to find last codepoint start */
    int64_t i = len - 1;
    while (i > 0 && ((unsigned char)buf[i] & 0xC0) == 0x80) i--;
    int64_t bytes_used;
    int64_t cp = utf8_decode(buf + i, &bytes_used);
    return cp;
}

/* uncons: Text -> Maybe (Char, Text) */
static int64_t text_uncons(int64_t s) {
    int64_t flat = kk_str_flatten(s);
    int64_t len;
    const char* buf = text_borrow(flat, &len);
    if (len == 0) return kk_nothing();
    int64_t bytes_used;
    int64_t cp = utf8_decode(buf, &bytes_used);
    int64_t rest = text_slice(flat, bytes_used, len - bytes_used);
    return kk_just(kk_pair(box_char(cp), rest));
}

static int64_t text_concat(int64_t list) {
    int64_t result = kk_string_empty();
    while (!kk_is_nil(list)) {
        result = kk_str_concat(result, kk_list_head(list));
        list = kk_list_tail(list);
    }
    return result;
}

static int64_t text_intercalate(int64_t sep, int64_t list) {
    if (kk_is_nil(list)) return kk_string_empty();
    int64_t result = kk_list_head(list);
    list = kk_list_tail(list);
    while (!kk_is_nil(list)) {
        kk_str_retain(sep);
        result = kk_str_concat(result, sep);
        result = kk_str_concat(result, kk_list_head(list));
        list = kk_list_tail(list);
    }
    kk_str_drop(sep);
    return result;
}

static int64_t text_is_prefix_of(int64_t pfx, int64_t s) {
    int64_t plen, slen;
    const char* pb = text_borrow(pfx, &plen);
    const char* sb = text_borrow(s, &slen);
    int r = (plen <= slen && memcmp(pb, sb, (size_t)plen) == 0) ? 1 : 0;
    return r;
}

static int64_t text_is_suffix_of(int64_t sfx, int64_t s) {
    int64_t flen, slen;
    const char* fb = text_borrow(sfx, &flen);
    const char* sb = text_borrow(s, &slen);
    int r = (flen <= slen && memcmp(sb + slen - flen, fb, (size_t)flen) == 0) ? 1 : 0;
    return r;
}

static int64_t text_is_infix_of(int64_t needle, int64_t haystack) {
    int64_t nlen, hlen;
    const char* nb = text_borrow(needle, &nlen);
    const char* hb = text_borrow(haystack, &hlen);
    int r = memmem_offset(hb, hlen, nb, nlen) >= 0 ? 1 : 0;
    return r;
}

/* stripPrefix: Text -> Text -> Maybe Text */
static int64_t text_strip_prefix(int64_t pfx, int64_t s) {
    int64_t plen, slen;
    const char* pb = text_borrow(pfx, &plen);
    const char* sb = text_borrow(s, &slen);
    if (plen <= slen && memcmp(pb, sb, (size_t)plen) == 0) {
        return kk_just(text_from_buf(sb + plen, slen - plen));
    }
    return kk_nothing();
}

static int64_t text_strip_start(int64_t s) {
    int64_t len;
    const char* buf = text_borrow(s, &len);
    if (len == 0) return kk_string_empty();
    int64_t i = 0;
    while (i < len && ((unsigned char)buf[i] == ' ' || (unsigned char)buf[i] == '\t'
                       || (unsigned char)buf[i] == '\n' || (unsigned char)buf[i] == '\r'))
        i++;
    int64_t r = text_from_buf(buf + i, len - i);
    return r;
}

static int64_t text_strip_end(int64_t s) {
    int64_t len;
    const char* buf = text_borrow(s, &len);
    if (len == 0) return kk_string_empty();
    int64_t i = len;
    while (i > 0 && ((unsigned char)buf[i-1] == ' ' || (unsigned char)buf[i-1] == '\t'
                      || (unsigned char)buf[i-1] == '\n' || (unsigned char)buf[i-1] == '\r'))
        i--;
    int64_t r = text_from_buf(buf, i);
    return r;
}

static int64_t text_strip(int64_t s) {
    return text_strip_end(text_strip_start(s));
}

/* splitOn: Text -> Text -> [Text] */
static int64_t text_split_on(int64_t delim, int64_t s) {
    int64_t dlen, slen;
    const char* db = text_borrow(delim, &dlen);
    const char* sb = text_borrow(s, &slen);
    if (dlen == 0) {
        /* Empty delimiter: return singleton list */
        return kk_cons(s, kk_nil());
    }
    /* Collect pieces in reverse, then reverse */
    int64_t pieces = kk_nil();
    int64_t piece_count = 0;
    int64_t start = 0;
    while (start <= slen) {
        int64_t pos = memmem_offset(sb + start, slen - start, db, dlen);
        if (pos < 0) {
            pieces = kk_cons(text_from_buf(sb + start, slen - start), pieces);
            piece_count++;
            break;
        }
        pieces = kk_cons(text_from_buf(sb + start, pos), pieces);
        piece_count++;
        start += pos + dlen;
        if (start > slen) {
            pieces = kk_cons(kk_string_empty(), pieces);
            piece_count++;
        }
    }
    /* Reverse the list */
    int64_t result = kk_nil();
    while (!kk_is_nil(pieces)) {
        result = kk_cons(kk_list_head(pieces), result);
        pieces = kk_list_tail(pieces);
    }
    return result;
}

/* breakOn: Text -> Text -> (Text, Text) */
static int64_t text_break_on(int64_t needle, int64_t s) {
    int64_t nlen, slen;
    const char* nb = text_borrow(needle, &nlen);
    const char* sb = text_borrow(s, &slen);
    int64_t pos = memmem_offset(sb, slen, nb, nlen);
    int64_t result;
    if (pos < 0) {
        result = kk_pair(s, kk_string_empty());
    } else {
        result = kk_pair(text_from_buf(sb, pos), text_from_buf(sb + pos, slen - pos));
    }
    return result;
}

/* breakOnEnd: Text -> Text -> (Text, Text) */
static int64_t text_break_on_end(int64_t needle, int64_t s) {
    int64_t nlen, slen;
    const char* nb = text_borrow(needle, &nlen);
    const char* sb = text_borrow(s, &slen);
    /* Find last occurrence */
    int64_t last_pos = -1;
    if (nlen > 0) {
        for (int64_t i = slen - nlen; i >= 0; i--) {
            if (memcmp(sb + i, nb, (size_t)nlen) == 0) { last_pos = i; break; }
        }
    }
    int64_t result;
    if (last_pos < 0) {
        result = kk_pair(kk_string_empty(), s);
    } else {
        int64_t split_at = last_pos + nlen;
        result = kk_pair(text_from_buf(sb, split_at), text_from_buf(sb + split_at, slen - split_at));
    }
    return result;
}

/* lines: Text -> [Text] */
static int64_t text_lines(int64_t s) {
    int64_t len;
    const char* buf = text_borrow(s, &len);
    if (len == 0) return kk_nil();
    int64_t pieces = kk_nil();
    int64_t start = 0;
    for (int64_t i = 0; i <= len; i++) {
        if (i == len || buf[i] == '\n') {
            int64_t end = i;
            if (end > start && buf[end-1] == '\r') end--;
            pieces = kk_cons(text_from_buf(buf + start, end - start), pieces);
            start = i + 1;
        }
    }
    /* Reverse */
    int64_t result = kk_nil();
    while (!kk_is_nil(pieces)) {
        result = kk_cons(kk_list_head(pieces), result);
        pieces = kk_list_tail(pieces);
    }
    return result;
}

/* unlines: [Text] -> Text  (join with \n) */
static int64_t text_unlines(int64_t list) {
    /* Use kk_str_concat rope with proper string refcounting */
    int64_t nl = kk_string_from_literal((int64_t)"\n", 1);
    int64_t result = kk_string_empty();
    while (!kk_is_nil(list)) {
        int64_t h = kk_list_head(list);
        if (!kk_is_string(h)) {
            /* Non-string element (e.g. empty constructor 'E' used as "")
             * — treat as empty string, skip it */
            list = kk_list_tail(list);
            continue;
        }
        kk_str_retain(h);  /* retain: concat takes ownership */
        result = kk_str_concat(result, h);
        kk_str_retain(nl);
        result = kk_str_concat(result, nl);
        list = kk_list_tail(list);
    }
    kk_str_drop(nl);
    return result;
}

/* map: (Char -> Char) -> Text -> Text */
static int64_t text_map(int64_t f, int64_t s) {
    int64_t len;
    const char* buf = text_borrow(s, &len);
    if (len == 0) return kk_string_empty();
    /* Worst case: each input byte becomes 4 output bytes */
    char* out = (char*)malloc((size_t)len * 4 + 1);
    if (!out) return kk_string_empty();
    int64_t opos = 0;
    int64_t i = 0;
    while (i < len) {
        int64_t bytes_used;
        int64_t cp = utf8_decode(buf + i, &bytes_used);
        i += bytes_used;
        int64_t new_cp = call_closure_1(f, box_char(cp));
        opos += utf8_encode(unbox_char(new_cp), out + opos);
    }
    int64_t r = text_from_buf(out, opos);
    free(out);
    return r;
}

/* all: (Char -> Bool) -> Text -> Bool */
static int64_t text_all(int64_t f, int64_t s) {
    int64_t len;
    const char* buf = text_borrow(s, &len);
    int64_t i = 0;
    while (i < len) {
        int64_t bytes_used;
        int64_t cp = utf8_decode(buf + i, &bytes_used);
        i += bytes_used;
        if (!call_closure_1(f, box_char(cp))) return 0;
    }
    return 1;
}

/* any: (Char -> Bool) -> Text -> Bool */
static int64_t text_any(int64_t f, int64_t s) {
    int64_t len;
    const char* buf = text_borrow(s, &len);
    int64_t i = 0;
    while (i < len) {
        int64_t bytes_used;
        int64_t cp = utf8_decode(buf + i, &bytes_used);
        i += bytes_used;
        if (call_closure_1(f, box_char(cp))) return 1;
    }
    return 0;
}

/* takeWhile: (Char -> Bool) -> Text -> Text */
static int64_t text_take_while(int64_t f, int64_t s) {
    int64_t flat = kk_str_flatten(s);
    int64_t len;
    const char* buf = text_borrow(flat, &len);
    if (len == 0) return kk_string_empty();
    int64_t i = 0;
    while (i < len) {
        int64_t bytes_used;
        int64_t cp = utf8_decode(buf + i, &bytes_used);
        if (!call_closure_1(f, box_char(cp))) break;
        i += bytes_used;
    }
    return text_slice(flat, 0, i);
}

/* dropWhile: (Char -> Bool) -> Text -> Text */
static int64_t text_drop_while(int64_t f, int64_t s) {
    int64_t flat = kk_str_flatten(s);
    int64_t len;
    const char* buf = text_borrow(flat, &len);
    if (len == 0) return kk_string_empty();
    int64_t i = 0;
    while (i < len) {
        int64_t bytes_used;
        int64_t cp = utf8_decode(buf + i, &bytes_used);
        if (!call_closure_1(f, box_char(cp))) break;
        i += bytes_used;
    }
    return text_slice(flat, i, len - i);
}

/* dropWhileEnd: (Char -> Bool) -> Text -> Text */
static int64_t text_drop_while_end(int64_t f, int64_t s) {
    int64_t flat = kk_str_flatten(s);
    int64_t len;
    const char* buf = text_borrow(flat, &len);
    if (len == 0) return kk_string_empty();
    int64_t end = len;
    while (end > 0) {
        int64_t cp_start = end - 1;
        while (cp_start > 0 && ((unsigned char)buf[cp_start] & 0xC0) == 0x80)
            cp_start--;
        int64_t bytes_used;
        int64_t cp = utf8_decode(buf + cp_start, &bytes_used);
        if (!call_closure_1(f, box_char(cp))) break;
        end = cp_start;
    }
    return text_slice(flat, 0, end);
}

/* span: (Char -> Bool) -> Text -> (Text, Text) */
static int64_t text_span(int64_t f, int64_t s) {
    int64_t flat = kk_str_flatten(s);
    int64_t len;
    const char* buf = text_borrow(flat, &len);
    if (len == 0) return kk_pair(kk_string_empty(), kk_string_empty());
    int64_t i = 0;
    while (i < len) {
        int64_t bytes_used;
        int64_t cp = utf8_decode(buf + i, &bytes_used);
        if (!call_closure_1(f, box_char(cp))) break;
        i += bytes_used;
    }
    int64_t r = kk_pair(text_slice(flat, 0, i), text_slice(flat, i, len - i));
    return r;
}

/* pack: [Char] -> Text  (list of codepoints to string) */
static int64_t text_pack(int64_t list) {
    /* If already a string (e.g. from show$1), return as-is */
    if (kk_is_string(list)) return list;
    /* Safety: if list is not a heap pointer, return empty string */
    if (!kk_is_heap_ptr(list)) return kk_string_empty();
    /* Safety: if list is a heap pointer but not a cons/nil list,
     * it might be a showsPrec result or other non-list value.
     * Check if it looks like a cons or nil. */
    {
        int64_t tag = kk_tag(list);
        if (tag != 46589 /* KK_CONS_TAG */ && tag != 31636 /* KK_NIL_TAG */) {
            /* Non-list heap value — this happens when show/printf returns
             * a rope string that kk_is_string doesn't recognize (e.g. after
             * arena rollback or GC), or when a [Char] is actually Text.
             * Try to recover: format as the integer value. */
            return kk_str_show_int(list);
        }
    }
    /* First pass: count total bytes needed */
    int64_t total = 0;
    int64_t tmp = list;
    while (!kk_is_nil(tmp)) {
        int64_t cp = unbox_char(kk_list_head(tmp));
        char dummy[4];
        total += utf8_encode(cp, dummy);
        tmp = kk_list_tail(tmp);
    }
    if (total == 0) return kk_string_empty();
    char* buf = (char*)malloc((size_t)total + 1);
    if (!buf) return kk_string_empty();
    int64_t pos = 0;
    while (!kk_is_nil(list)) {
        int64_t cp = unbox_char(kk_list_head(list));
        pos += utf8_encode(cp, buf + pos);
        list = kk_list_tail(list);
    }
    buf[total] = '\0';
    return kk_str_alloc_leaf_owned(buf, total);
}

/* unpack: Text -> [Char]  (string to list of codepoints) */
static int64_t text_unpack(int64_t s) {
    int64_t len;
    const char* buf = text_borrow(s, &len);
    if (len == 0) return kk_nil();
    /* Build list backwards from end */
    /* First, collect codepoints into temp array */
    int64_t* cps = (int64_t*)malloc((size_t)len * sizeof(int64_t));
    if (!cps) return kk_nil();
    int64_t n = 0;
    int64_t i = 0;
    while (i < len) {
        int64_t bytes_used;
        cps[n++] = utf8_decode(buf + i, &bytes_used);
        i += bytes_used;
    }
    int64_t result = kk_nil();
    for (int64_t j = n - 1; j >= 0; j--) {
        result = kk_cons(box_char(cps[j]), result);
    }
    free(cps);
    return result;
}

/* singleton: Char -> Text  (single codepoint to string) */
static int64_t text_singleton(int64_t cp) {
    char buf[4];
    int n = utf8_encode(cp, buf);
    return text_from_buf(buf, n);
}

/* =================================================================== */
/*  Exported symbols with asm labels                                   */
/* =================================================================== */

/* ------------------------------------------------------------------ */
/*  Closure trampolines                                                 */
/*  The closure dispatch convention calls fn(closure, args...) with     */
/*  the closure itself as the first argument.  Raw C functions like     */
/*  text_map(f, s) don't follow this convention, so we need            */
/*  trampolines that extract captures from the closure and forward.     */
/* ------------------------------------------------------------------ */
#define CLOS_TAG_T 0x434C4F53

/* 1-capture trampolines: closure has [fptr, cap1], called as fn(clos, arg) */
static int64_t tram_pack(int64_t clos, int64_t arg)       { (void)clos; return text_pack(arg); }
static int64_t tram_null(int64_t clos, int64_t arg)       { (void)clos; return text_null(arg); }
static int64_t tram_strip(int64_t clos, int64_t arg)      { (void)clos; return text_strip(arg); }
static int64_t tram_stripStart(int64_t clos, int64_t arg) { (void)clos; return text_strip_start(arg); }
static int64_t tram_unlines(int64_t clos, int64_t arg)    { (void)clos; return text_unlines(arg); }

/* 1-capture trampolines: closure has [fptr, cap1], called as fn(clos, arg)
 * where cap1 is the first param of the 2-arg function */
static int64_t tram_drop(int64_t clos, int64_t s)       { return text_drop(kk_field(clos,1), s); }
static int64_t tram_dropEnd(int64_t clos, int64_t s)    { return text_drop_end(kk_field(clos,1), s); }
static int64_t tram_takeWhile(int64_t clos, int64_t s)  { return text_take_while(kk_field(clos,1), s); }
static int64_t tram_dropWhile(int64_t clos, int64_t s)  { return text_drop_while(kk_field(clos,1), s); }
static int64_t tram_map(int64_t clos, int64_t s)        { return text_map(kk_field(clos,1), s); }
static int64_t tram_intercalate(int64_t clos, int64_t s){ return text_intercalate(kk_field(clos,1), s); }
static int64_t tram_isPrefixOf(int64_t clos, int64_t s) { return text_is_prefix_of(kk_field(clos,1), s); }
static int64_t tram_isInfixOf(int64_t clos, int64_t s)  { return text_is_infix_of(kk_field(clos,1), s); }

/* =================================================================== */

/* --- Internal --- */
int64_t text_internal_empty_0(void) __asm__("Data_Text_Internal_empty$0");
int64_t text_internal_empty_0(void) { return kk_string_empty(); }

int64_t text_internal_pack_1(int64_t list) __asm__("Data_Text_Internal_pack$1");
int64_t text_internal_pack_1(int64_t list) { return text_pack(list); }

int64_t text_internal_pack_0(void) __asm__("Data_Text_Internal_pack$0");
int64_t text_internal_pack_0(void) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 1);
    kk_set_field(c, 0, (int64_t)&tram_pack);
    return c;
}

/* --- Queries --- */
int64_t text_null_1(int64_t s) __asm__("Data_Text_null$1");
int64_t text_null_1(int64_t s) { return text_null(s); }

int64_t text_null_0(void) __asm__("Data_Text_null$0");
int64_t text_null_0(void) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 1);
    kk_set_field(c, 0, (int64_t)&tram_null);
    return c;
}

int64_t text_length_1(int64_t s) __asm__("Data_Text_length$1");
int64_t text_length_1(int64_t s) { return text_length(s); }

int64_t text_last_1(int64_t s) __asm__("Data_Text_last$1");
int64_t text_last_1(int64_t s) { return text_last(s); }

/* --- Slicing --- */
int64_t text_take_2(int64_t n, int64_t s) __asm__("Data_Text_take$2");
int64_t text_take_2(int64_t n, int64_t s) { return text_take(n, s); }

int64_t text_drop_2(int64_t n, int64_t s) __asm__("Data_Text_drop$2");
int64_t text_drop_2(int64_t n, int64_t s) { return text_drop(n, s); }

int64_t text_drop_1(int64_t n) __asm__("Data_Text_drop$1");
int64_t text_drop_1(int64_t n) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 2);
    kk_set_field(c, 0, (int64_t)&tram_drop);
    kk_set_field(c, 1, n);
    return c;
}

int64_t text_dropEnd_2(int64_t n, int64_t s) __asm__("Data_Text_dropEnd$2");
int64_t text_dropEnd_2(int64_t n, int64_t s) { return text_drop_end(n, s); }

int64_t text_dropEnd_1(int64_t n) __asm__("Data_Text_dropEnd$1");
int64_t text_dropEnd_1(int64_t n) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 2);
    kk_set_field(c, 0, (int64_t)&tram_dropEnd);
    kk_set_field(c, 1, n);
    return c;
}

int64_t text_init_1(int64_t s) __asm__("Data_Text_init$1");
int64_t text_init_1(int64_t s) { return text_init(s); }

int64_t text_uncons_1(int64_t s) __asm__("Data_Text_uncons$1");
int64_t text_uncons_1(int64_t s) { return text_uncons(s); }

/* --- Predicate-based --- */
int64_t text_takeWhile_2(int64_t f, int64_t s) __asm__("Data_Text_takeWhile$2");
int64_t text_takeWhile_2(int64_t f, int64_t s) { return text_take_while(f, s); }

int64_t text_takeWhile_1(int64_t f) __asm__("Data_Text_takeWhile$1");
int64_t text_takeWhile_1(int64_t f) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 2);
    kk_set_field(c, 0, (int64_t)&tram_takeWhile);
    kk_set_field(c, 1, f);
    return c;
}

int64_t text_dropWhile_2(int64_t f, int64_t s) __asm__("Data_Text_dropWhile$2");
int64_t text_dropWhile_2(int64_t f, int64_t s) { return text_drop_while(f, s); }

int64_t text_dropWhile_1(int64_t f) __asm__("Data_Text_dropWhile$1");
int64_t text_dropWhile_1(int64_t f) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 2);
    kk_set_field(c, 0, (int64_t)&tram_dropWhile);
    kk_set_field(c, 1, f);
    return c;
}

int64_t text_dropWhileEnd_2(int64_t f, int64_t s) __asm__("Data_Text_dropWhileEnd$2");
int64_t text_dropWhileEnd_2(int64_t f, int64_t s) { return text_drop_while_end(f, s); }

int64_t text_all_2(int64_t f, int64_t s) __asm__("Data_Text_all$2");
int64_t text_all_2(int64_t f, int64_t s) { return text_all(f, s); }

int64_t text_any_2(int64_t f, int64_t s) __asm__("Data_Text_any$2");
int64_t text_any_2(int64_t f, int64_t s) { return text_any(f, s); }

int64_t text_span_2(int64_t f, int64_t s) __asm__("Data_Text_span$2");
int64_t text_span_2(int64_t f, int64_t s) { return text_span(f, s); }

int64_t text_map_1(int64_t f) __asm__("Data_Text_map$1");
int64_t text_map_1(int64_t f) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 2);
    kk_set_field(c, 0, (int64_t)&tram_map);
    kk_set_field(c, 1, f);
    return c;
}

/* --- Concatenation --- */
int64_t text_concat_1(int64_t list) __asm__("Data_Text_concat$1");
int64_t text_concat_1(int64_t list) { return text_concat(list); }

/* concatMap :: (Char -> Text) -> Text -> Text */
static int64_t text_concatMap(int64_t f, int64_t s) {
    int64_t len;
    const char* buf = text_borrow(s, &len);
    if (len == 0) return kk_string_empty();
    int64_t result = kk_string_empty();
    int64_t i = 0;
    while (i < len) {
        int64_t bytes_used;
        int64_t cp = utf8_decode(buf + i, &bytes_used);
        i += bytes_used;
        kk_retain(f);
        int64_t piece = call_closure_1(f, box_char(cp));
        result = kk_str_concat(result, piece);
    }
    return result;
}

static int64_t tram_concatMap(int64_t clos, int64_t s) {
    return text_concatMap(kk_field(clos, 1), s);
}

int64_t text_concatMap_1(int64_t f) __asm__("Data_Text_concatMap$1");
int64_t text_concatMap_1(int64_t f) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 2);
    kk_set_field(c, 0, (int64_t)&tram_concatMap);
    kk_set_field(c, 1, f);
    return c;
}

int64_t text_intercalate_2(int64_t sep, int64_t list) __asm__("Data_Text_intercalate$2");
int64_t text_intercalate_2(int64_t sep, int64_t list) { return text_intercalate(sep, list); }

int64_t text_intercalate_1(int64_t sep) __asm__("Data_Text_intercalate$1");
int64_t text_intercalate_1(int64_t sep) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 2);
    kk_set_field(c, 0, (int64_t)&tram_intercalate);
    kk_set_field(c, 1, sep);
    return c;
}

/* --- Searching --- */
int64_t text_isPrefixOf_2(int64_t pfx, int64_t s) __asm__("Data_Text_isPrefixOf$2");
int64_t text_isPrefixOf_2(int64_t pfx, int64_t s) { return text_is_prefix_of(pfx, s); }

int64_t text_isPrefixOf_1(int64_t pfx) __asm__("Data_Text_isPrefixOf$1");
int64_t text_isPrefixOf_1(int64_t pfx) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 2);
    kk_set_field(c, 0, (int64_t)&tram_isPrefixOf);
    kk_set_field(c, 1, pfx);
    return c;
}

int64_t text_isSuffixOf_2(int64_t sfx, int64_t s) __asm__("Data_Text_isSuffixOf$2");
int64_t text_isSuffixOf_2(int64_t sfx, int64_t s) { return text_is_suffix_of(sfx, s); }

int64_t text_isInfixOf_2(int64_t needle, int64_t s) __asm__("Data_Text_isInfixOf$2");
int64_t text_isInfixOf_2(int64_t needle, int64_t s) { return text_is_infix_of(needle, s); }

int64_t text_isInfixOf_1(int64_t needle) __asm__("Data_Text_isInfixOf$1");
int64_t text_isInfixOf_1(int64_t needle) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 2);
    kk_set_field(c, 0, (int64_t)&tram_isInfixOf);
    kk_set_field(c, 1, needle);
    return c;
}

/* --- Breaking --- */
int64_t text_breakOn_2(int64_t needle, int64_t s) __asm__("Data_Text_breakOn$2");
int64_t text_breakOn_2(int64_t needle, int64_t s) { return text_break_on(needle, s); }

int64_t text_breakOn_3(int64_t _dict, int64_t needle, int64_t s) __asm__("Data_Text_breakOn$3");
int64_t text_breakOn_3(int64_t _dict, int64_t needle, int64_t s) {
    (void)_dict;
    return text_break_on(needle, s);
}

int64_t text_breakOnEnd_3(int64_t _dict, int64_t needle, int64_t s) __asm__("Data_Text_breakOnEnd$3");
int64_t text_breakOnEnd_3(int64_t _dict, int64_t needle, int64_t s) {
    (void)_dict;
    return text_break_on_end(needle, s);
}

/* --- splitOn --- */
int64_t text_splitOn_2(int64_t delim, int64_t s) __asm__("Data_Text_splitOn$2");
int64_t text_splitOn_2(int64_t delim, int64_t s) { return text_split_on(delim, s); }

int64_t text_splitOn_3(int64_t _dict, int64_t delim, int64_t s) __asm__("Data_Text_splitOn$3");
int64_t text_splitOn_3(int64_t _dict, int64_t delim, int64_t s) {
    (void)_dict;
    return text_split_on(delim, s);
}

/* --- Stripping --- */
int64_t text_strip_1(int64_t s) __asm__("Data_Text_strip$1");
int64_t text_strip_1(int64_t s) { return text_strip(s); }

int64_t text_strip_0(void) __asm__("Data_Text_strip$0");
int64_t text_strip_0(void) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 1);
    kk_set_field(c, 0, (int64_t)&tram_strip);
    return c;
}

int64_t text_stripStart_1(int64_t s) __asm__("Data_Text_stripStart$1");
int64_t text_stripStart_1(int64_t s) { return text_strip_start(s); }

int64_t text_stripStart_0(void) __asm__("Data_Text_stripStart$0");
int64_t text_stripStart_0(void) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 1);
    kk_set_field(c, 0, (int64_t)&tram_stripStart);
    return c;
}

int64_t text_stripEnd_1(int64_t s) __asm__("Data_Text_stripEnd$1");
int64_t text_stripEnd_1(int64_t s) { return text_strip_end(s); }

int64_t text_stripPrefix_2(int64_t pfx, int64_t s) __asm__("Data_Text_stripPrefix$2");
int64_t text_stripPrefix_2(int64_t pfx, int64_t s) { return text_strip_prefix(pfx, s); }

/* --- Lines --- */
int64_t text_lines_1(int64_t s) __asm__("Data_Text_lines$1");
int64_t text_lines_1(int64_t s) { return text_lines(s); }

int64_t text_unlines_1(int64_t list) __asm__("Data_Text_unlines$1");
int64_t text_unlines_1(int64_t list) { return text_unlines(list); }

int64_t text_unlines_0(void) __asm__("Data_Text_unlines$0");
int64_t text_unlines_0(void) {
    int64_t c = kk_alloc_con(CLOS_TAG_T, 1);
    kk_set_field(c, 0, (int64_t)&tram_unlines);
    return c;
}

/* --- Show --- */
int64_t text_show_singleton_1(int64_t cp) __asm__("Data_Text_Show_singleton$1");
int64_t text_show_singleton_1(int64_t cp) {
    /* cp is a boxed Char (C# tag=30786, codepoint in field 0) */
    int64_t raw = (kk_is_heap_ptr(cp) && kk_tag(cp) == CHAR_BOX_TAG) ? kk_field(cp, 0) : cp;
    return text_singleton(raw);
}

int64_t text_show_unpack_1(int64_t s) __asm__("Data_Text_Show_unpack$1");
int64_t text_show_unpack_1(int64_t s) { return text_unpack(s); }

/* --- Encoding --- */
/* decodeUtf8With: lenient decode already produces a kk_string, so identity */
int64_t text_decodeUtf8With_2(int64_t _handler, int64_t bs)
    __asm__("Data_Text_Encoding_decodeUtf8With$2");
int64_t text_decodeUtf8With_2(int64_t _handler, int64_t bs) {
    (void)_handler;
    return bs; /* ByteString and Text share the same kk_string representation */
}

/* encodeUtf8: Text -> ByteString  (identity — same representation) */
int64_t text_encodeUtf8_1(int64_t s)
    __asm__("Data_Text_Encoding_encodeUtf8$1");
int64_t text_encodeUtf8_1(int64_t s) { return s; }

/* lenientDecode: returns a closure (the error handler for decodeUtf8With) */
int64_t text_lenientDecode_0(void)
    __asm__("Data_Text_Encoding_Error_lenientDecode$0");
int64_t text_lenientDecode_0(void) {
    /* Return a dummy closure — decodeUtf8With ignores the handler anyway */
    int64_t c = kk_alloc_con(CLOS_TAG_T, 1);
    kk_set_field(c, 0, 0);
    return c;
}

/* --- I/O --- */
int64_t text_io_readFile_1(int64_t path)
    __asm__("Data_Text_IO_readFile$1");
int64_t text_io_readFile_1(int64_t path) {
    return kk_read_file(path);
}

int64_t text_io_writeFile_2(int64_t path, int64_t content)
    __asm__("Data_Text_IO_writeFile$2");
int64_t text_io_writeFile_2(int64_t path, int64_t content) {
    return kk_write_file(path, content);
}

/* --- ByteString --- */
int64_t bs_length_1(int64_t b) __asm__("Data_ByteString_length$1");
int64_t bs_length_1(int64_t b) { return kk_bytes_len(b); }

int64_t bs_unpack_1(int64_t b) __asm__("Data_ByteString_unpack$1");
int64_t bs_unpack_1(int64_t b) {
    /* ByteString -> [Word8]: byte list */
    int64_t len = kk_bytes_len(b);
    int64_t result = kk_nil();
    for (int64_t i = len - 1; i >= 0; i--) {
        result = kk_cons(kk_bytes_index(b, i), result);
    }
    return result;
}

/* --- head / tail --- */

static int64_t text_head(int64_t s) {
    int64_t len;
    const char* buf = text_borrow(s, &len);
    if (len == 0) return box_char(0);
    int64_t bytes_used;
    int64_t cp = utf8_decode(buf, &bytes_used);
    return box_char(cp);
}

static int64_t text_tail(int64_t s) {
    int64_t flat = kk_str_flatten(s);
    int64_t len;
    const char* buf = text_borrow(flat, &len);
    if (len == 0) return kk_string_empty();
    int64_t bytes_used;
    utf8_decode(buf, &bytes_used);
    return text_slice(flat, bytes_used, len - bytes_used);
}

int64_t text_head_1(int64_t s) __asm__("Data_Text_head$1");
int64_t text_head_1(int64_t s) { return text_head(s); }

int64_t text_tail_1(int64_t s) __asm__("Data_Text_tail$1");
int64_t text_tail_1(int64_t s) { return text_tail(s); }

/* --- break (predicate version) --- */

/* T.break pred text: split at first char where pred is true.
 * Returns (before, after) as a Haskell tuple. */
static int64_t text_break_pred(int64_t pred, int64_t s) {
    int64_t flat = kk_str_flatten(s);
    int64_t len;
    const char* buf = text_borrow(flat, &len);
    if (len == 0) return kk_pair(kk_string_empty(), kk_string_empty());
    int64_t off = 0;
    while (off < len) {
        int64_t bytes_used;
        int64_t cp = utf8_decode(buf + off, &bytes_used);
        int64_t r = call_closure_1(pred, box_char(cp));
        if (r) {
            int64_t before = text_slice(flat, 0, off);
            int64_t after  = text_slice(flat, off, len - off);
            return kk_pair(before, after);
        }
        off += bytes_used;
    }
    /* No match — return (whole, empty). Retain the original string. */
    kk_str_retain(flat);
    return kk_pair(flat, kk_string_empty());
}

int64_t text_break_2(int64_t pred, int64_t s) __asm__("Data_Text_break$2");
int64_t text_break_2(int64_t pred, int64_t s) { return text_break_pred(pred, s); }

/* --- splitAt --- */

static int64_t text_split_at(int64_t n, int64_t s) {
    int64_t flat = kk_str_flatten(s);
    int64_t len;
    const char* buf = text_borrow(flat, &len);
    if (len == 0) return kk_pair(kk_string_empty(), kk_string_empty());
    int64_t byte_off = utf8_advance(buf, len, n);
    int64_t before = text_slice(flat, 0, byte_off);
    int64_t after  = text_slice(flat, byte_off, len - byte_off);
    return kk_pair(before, after);
}

int64_t text_splitAt_2(int64_t n, int64_t s) __asm__("Data_Text_splitAt$2");
int64_t text_splitAt_2(int64_t n, int64_t s) { return text_split_at(n, s); }

/* --- foldl' --- */

/* T.foldl' f z text: strict left fold over codepoints */
static int64_t text_foldl_strict(int64_t f, int64_t z, int64_t s) {
    int64_t len;
    const char* buf = text_borrow(s, &len);
    if (len == 0) return z;
    int64_t acc = z;
    int64_t off = 0;
    while (off < len) {
        int64_t bytes_used;
        int64_t cp = utf8_decode(buf + off, &bytes_used);
        /* The compiled step function may have a hidden 'self' parameter
         * (GHC where-clause binding). The PAP wrapper expects 4 args:
         * pap(clos, self, acc, char). Pass the closure as both the
         * PAP's clos and the inner function's self parameter. */
        int64_t fn_ptr = kk_field(kk_thunk_force(f), 0);
        typedef int64_t (*fn3_t)(int64_t, int64_t, int64_t, int64_t);
        acc = ((fn3_t)(intptr_t)fn_ptr)(f, f, acc, box_char(cp));
        off += bytes_used;
    }
    /* buf is borrowed — do NOT free */
    return acc;
}

/* Data_Text_foldl_$2(f, z) is a partial application: T.foldl' f z
 * Returns a closure that, when called with text, does the fold.
 * From the MLIR: called as foldl'(closure_wrapping_lambda, 0) and the
 * result is returned as a Text->a closure. */
static int64_t foldl_trampoline(int64_t clos, int64_t text_arg) {
    int64_t f = kk_field(clos, 1);
    int64_t z = kk_field(clos, 2);
    return text_foldl_strict(f, z, text_arg);
}

int64_t text_foldl__2(int64_t f, int64_t z) __asm__("Data_Text_foldl_$2");
int64_t text_foldl__2(int64_t f, int64_t z) {
    /* Return a closure: when called with text, executes foldl' f z text */
    int64_t c = kk_alloc_con(CLOS_TAG_T, 3);
    kk_set_field(c, 0, (int64_t)(intptr_t)foldl_trampoline);
    kk_set_field(c, 1, f);
    kk_set_field(c, 2, z);
    return c;
}
