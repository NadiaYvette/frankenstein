/* C replacement for sanitizeName / encodeChar from MlirEmit.Emitter.
 *
 * The compiled Haskell version (via T.concatMap encodeChar) non-deterministically
 * corrupts characters — e.g. 'l' (108) → '0' (48) — causing @frankenstein_fld$0()
 * external calls instead of local pattern-match references in stage2 MLIR.
 *
 * This C version does the same Z-encoding in a simple, deterministic loop.
 * It overrides the compiled MLIR version via --allow-multiple-definition
 * (this file sorts before MlirEmit_Emitter.o alphabetically).
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "../runtime/kk_runtime.h"

/* Forward declarations from the runtime / shims */
extern int64_t kk_str_flatten(int64_t s);
extern int64_t kk_str_len(int64_t s);
extern char*   kk_str_dup_cstr(int64_t s);
extern int64_t kk_str_alloc_leaf_owned(const char* bytes, int64_t byte_len);
extern int64_t kk_string_empty(void);
extern int64_t kk_thunk_create_forced(int64_t val);

#define CLOS_TAG 0x434C4F53  /* "CLOS" */

/* ------------------------------------------------------------------ */
/*  encodeChar: map a single codepoint to its Z-encoded string         */
/*  Returns a pointer to a static string and sets *out_len.            */
/* ------------------------------------------------------------------ */
static const char* encode_char(int64_t cp, int *out_len) {
    switch (cp) {
        case '$': *out_len = 2; return "zd";
        case '+': *out_len = 2; return "zp";
        case '*': *out_len = 2; return "zt";
        case '-': *out_len = 2; return "zm";
        case '=': *out_len = 2; return "ze";
        case '<': *out_len = 2; return "zl";
        case '>': *out_len = 2; return "zg";
        case '!': *out_len = 2; return "zn";
        case '@': *out_len = 2; return "za";
        case '#': *out_len = 2; return "zh";
        case '%': *out_len = 2; return "zv";
        case '^': *out_len = 2; return "zc";
        case '&': *out_len = 2; return "zb";
        case '|': *out_len = 2; return "zo";
        case '~': *out_len = 2; return "zw";
        case '/': case '.': case ',': case '(': case ')':
        case '[': case ']': case '{': case '}':
        case '\'': case '"': case '\\': case ' ': case '\t':
            *out_len = 1; return "_";
        default:
            *out_len = 0; return NULL;  /* signal: use original byte */
    }
}

/* ------------------------------------------------------------------ */
/*  sanitize_name_c: the actual Text -> Text sanitization              */
/* ------------------------------------------------------------------ */
static int64_t sanitize_name_c(int64_t s) {
    if (!s) return kk_string_empty();

    /* Flatten the input rope to get raw bytes */
    int64_t flat = kk_str_flatten(s);
    int64_t byte_len = kk_str_len(flat);
    if (byte_len <= 0) return kk_string_empty();

    char* src = kk_str_dup_cstr(flat);
    if (!src) return kk_string_empty();

    /* Worst case: every char expands to 2 bytes (Z-encoding) */
    char* out = (char*)malloc((size_t)(byte_len * 2 + 1));
    if (!out) { free(src); return kk_string_empty(); }

    int64_t opos = 0;
    for (int64_t i = 0; i < byte_len; i++) {
        unsigned char c = (unsigned char)src[i];
        /* Handle multi-byte UTF-8: pass through non-ASCII bytes as-is.
         * sanitizeName only Z-encodes ASCII special chars. */
        if (c >= 0x80) {
            out[opos++] = (char)c;
            continue;
        }
        int elen;
        const char* enc = encode_char(c, &elen);
        if (enc) {
            memcpy(out + opos, enc, (size_t)elen);
            opos += elen;
        } else {
            /* Default: pass through */
            out[opos++] = (char)c;
        }
    }
    out[opos] = '\0';
    free(src);

    int64_t result = kk_str_alloc_leaf_owned(out, opos);
    return result;
}

/* ------------------------------------------------------------------ */
/*  Closure trampoline for sanitize_name_c                             */
/* ------------------------------------------------------------------ */
static int64_t tram_sanitize(int64_t clos, int64_t s) {
    (void)clos;
    return sanitize_name_c(s);
}

/* ------------------------------------------------------------------ */
/*  Override: Frankenstein_MlirEmit_Emitter_sanitizeName   */
/*                                                                      */
/*  Both inline and plotkin modes: the compiled call sites pass Text    */
/*  directly in %rdi and use the returned Text directly (no closure     */
/*  dispatch).  Plotkin mode adds a leading evv arg which we ignore.    */
/* ------------------------------------------------------------------ */
#ifdef PLOTKIN_EVIDENCE
int64_t c_sanitizeName(int64_t evv, int64_t text)
    __asm__("Frankenstein_MlirEmit_Emitter_sanitizeName");
int64_t c_sanitizeName(int64_t evv, int64_t text) {
    (void)evv;
    return sanitize_name_c(text);
}
#else
int64_t c_sanitizeName(int64_t text)
    __asm__("Frankenstein_MlirEmit_Emitter_sanitizeName");
int64_t c_sanitizeName(int64_t text) {
    return sanitize_name_c(text);
}
#endif

/* ------------------------------------------------------------------ */
/*  Override: Frankenstein_MlirEmit_Emitter_nameToSsa      */
/*                                                                      */
/*  nameToSsa n = sanitizeName (nameText n) <> T.pack (show (nameUnique n))  */
/*                                                                      */
/*  Plotkin mode: extra leading evv arg (ignored).                      */
/* ------------------------------------------------------------------ */
#ifdef PLOTKIN_EVIDENCE
int64_t c_nameToSsa(int64_t evv, int64_t name)
    __asm__("Frankenstein_MlirEmit_Emitter_nameToSsa");
int64_t c_nameToSsa(int64_t evv, int64_t name) {
    (void)evv;
#else
int64_t c_nameToSsa(int64_t name)
    __asm__("Frankenstein_MlirEmit_Emitter_nameToSsa");
int64_t c_nameToSsa(int64_t name) {
#endif
    int64_t text   = kk_field(name, 0);  /* nameText   :: !Text */
    int64_t unique = kk_field(name, 1);  /* nameUnique :: !Int  */

    int64_t sanitized  = sanitize_name_c(text);
    int64_t unique_str = kk_str_show_int(unique);
    int64_t result     = kk_str_concat(sanitized, unique_str);
    return result;
}

/* Also override encodeChar for defence in depth.
 * Plotkin mode: extra leading evv arg (ignored). */
#ifdef PLOTKIN_EVIDENCE
int64_t c_encodeChar(int64_t evv, int64_t cp)
    __asm__("frankenstein_encodeCharzd6989586621679048837_u6989586621679048837");
int64_t c_encodeChar(int64_t evv, int64_t cp) {
    (void)evv;
#else
int64_t c_encodeChar(int64_t cp)
    __asm__("frankenstein_encodeCharzd6989586621679048837_u6989586621679048837");
int64_t c_encodeChar(int64_t cp) {
#endif
    int elen;
    const char* enc = encode_char(cp, &elen);
    if (enc) {
        char* buf = (char*)malloc((size_t)elen + 1);
        memcpy(buf, enc, (size_t)elen);
        buf[elen] = '\0';
        return kk_str_alloc_leaf_owned(buf, elen);
    }
    /* Default: T.singleton c — encode codepoint as UTF-8 */
    char buf[4];
    int n;
    if (cp < 0x80)    { buf[0] = (char)cp; n = 1; }
    else if (cp < 0x800)   { buf[0] = (char)(0xC0 | (cp >> 6)); buf[1] = (char)(0x80 | (cp & 0x3F)); n = 2; }
    else if (cp < 0x10000) { buf[0] = (char)(0xE0 | (cp >> 12)); buf[1] = (char)(0x80 | ((cp >> 6) & 0x3F)); buf[2] = (char)(0x80 | (cp & 0x3F)); n = 3; }
    else { buf[0] = (char)(0xF0 | (cp >> 18)); buf[1] = (char)(0x80 | ((cp >> 12) & 0x3F)); buf[2] = (char)(0x80 | ((cp >> 6) & 0x3F)); buf[3] = (char)(0x80 | (cp & 0x3F)); n = 4; }
    char* owned = (char*)malloc((size_t)n + 1);
    memcpy(owned, buf, (size_t)n);
    owned[n] = '\0';
    return kk_str_alloc_leaf_owned(owned, n);
}
