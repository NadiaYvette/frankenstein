/* Idris2 RefC string-primitive shims.
 *
 * Idris2's stdlib declares a small set of string primitives as
 * `%foreign "RefC:fastUnpack" ...` etc.  The Idris2 RefC backend
 * provides C implementations under the bare names (fastUnpack,
 * fastPack, fastConcat), but those names use the RefC backend's
 * own opaque value type (Idris2_Value, Idris2_Constructor) and
 * cannot link against Frankenstein's runtime directly.
 *
 * idris2-shim renames `RefC:foo` foreign targets to `idris2_foo`
 * (see parseCName in idris2-shim/src/Main.idr) so the call resolves
 * to one of the shims below instead of suffix-matching the Idris2
 * declaration itself and self-recursing.
 *
 * The shims operate on Frankenstein's runtime types:
 *   String      → kk_string_t* (rope tree, byte-counted)
 *   List Char   → kk_alloc_con cells with KK_CONS_TAG (46589) / KK_NIL_TAG (31636)
 *   List String → same shape, each head is a kk_string_t*
 *   Char        → unboxed i64 (Unicode codepoint)
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "../runtime/kk_runtime.h"

/* idris2_fastUnpack : String -> List Char
 * Build a cons-list of Char from the bytes of the input string.
 * One Char per UTF-8 codepoint.  Empty string → Nil. */
int64_t idris2_fastUnpack(int64_t s);
int64_t idris2_fastUnpack(int64_t s) {
    if (s == 0) return kk_nil();
    /* kk_str_dup_cstr flattens the rope and returns a malloc'd
     * NUL-terminated copy.  We free it after walking. */
    char* bytes = kk_str_dup_cstr(s);
    if (!bytes) return kk_nil();
    int64_t n = (int64_t)strlen(bytes);
    if (n <= 0) { free(bytes); return kk_nil(); }

    int64_t result = kk_nil();
    /* Walk UTF-8 from end to beginning so we cons up in forward order. */
    int64_t i = n;
    while (i > 0) {
        int64_t j = i - 1;
        while (j > 0 && (((unsigned char)bytes[j] & 0xC0) == 0x80)) j--;
        unsigned char c0 = (unsigned char)bytes[j];
        int64_t cp;
        if (c0 < 0x80) {
            cp = c0;
        } else if ((c0 & 0xE0) == 0xC0) {
            cp = (c0 & 0x1F) << 6;
            cp |= (unsigned char)bytes[j+1] & 0x3F;
        } else if ((c0 & 0xF0) == 0xE0) {
            cp = (c0 & 0x0F) << 12;
            cp |= ((unsigned char)bytes[j+1] & 0x3F) << 6;
            cp |= (unsigned char)bytes[j+2] & 0x3F;
        } else {
            cp = (c0 & 0x07) << 18;
            cp |= ((unsigned char)bytes[j+1] & 0x3F) << 12;
            cp |= ((unsigned char)bytes[j+2] & 0x3F) << 6;
            cp |= (unsigned char)bytes[j+3] & 0x3F;
        }
        result = kk_cons(cp, result);
        i = j;
    }
    free(bytes);
    return result;
}

/* idris2_fastPack : List Char -> String
 * Inverse of fastUnpack: build a byte-counted string by emitting the
 * UTF-8 encoding of each Char in the cons list. */
int64_t idris2_fastPack(int64_t chars);
int64_t idris2_fastPack(int64_t chars) {
    /* First pass: count total bytes needed. */
    int64_t cur = chars;
    int64_t total = 0;
    while (kk_is_heap_ptr(cur) && kk_tag(cur) == 46589 /* KK_CONS_TAG */) {
        int64_t cp = kk_list_head(cur);
        if (cp < 0x80) total += 1;
        else if (cp < 0x800) total += 2;
        else if (cp < 0x10000) total += 3;
        else total += 4;
        cur = kk_list_tail(cur);
    }
    if (total == 0) return kk_string_empty();

    /* Allocate, fill, return. */
    char* buf = (char*)malloc((size_t)total);
    if (!buf) return kk_string_empty();
    int64_t pos = 0;
    cur = chars;
    while (kk_is_heap_ptr(cur) && kk_tag(cur) == 46589) {
        int64_t cp = kk_list_head(cur);
        if (cp < 0x80) {
            buf[pos++] = (char)cp;
        } else if (cp < 0x800) {
            buf[pos++] = (char)(0xC0 | (cp >> 6));
            buf[pos++] = (char)(0x80 | (cp & 0x3F));
        } else if (cp < 0x10000) {
            buf[pos++] = (char)(0xE0 | (cp >> 12));
            buf[pos++] = (char)(0x80 | ((cp >> 6) & 0x3F));
            buf[pos++] = (char)(0x80 | (cp & 0x3F));
        } else {
            buf[pos++] = (char)(0xF0 | (cp >> 18));
            buf[pos++] = (char)(0x80 | ((cp >> 12) & 0x3F));
            buf[pos++] = (char)(0x80 | ((cp >> 6) & 0x3F));
            buf[pos++] = (char)(0x80 | (cp & 0x3F));
        }
        cur = kk_list_tail(cur);
    }
    int64_t result = kk_str_alloc_leaf_owned(buf, total);
    free(buf);
    return result;
}

/* idris2_fastConcat : List String -> String
 * Concatenate a list of strings using the runtime's O(1) rope concat. */
int64_t idris2_fastConcat(int64_t strs);
int64_t idris2_fastConcat(int64_t strs) {
    int64_t result = kk_string_empty();
    int64_t cur = strs;
    while (kk_is_heap_ptr(cur) && kk_tag(cur) == 46589) {
        int64_t s = kk_list_head(cur);
        result = kk_str_concat(result, s);
        cur = kk_list_tail(cur);
    }
    return result;
}
