/* Frankenstein minimal runtime — Perceus refcounting + boxed values
 *
 * Boxed value layout (all fields int64_t):
 *   [refcount] [tag] [field0] [field1] ...
 *              ^--- the "pointer" points here (to the tag)
 *
 * The refcount lives at (ptr - 8). Fields start at (ptr + 8).
 * A null/zero pointer means "unboxed integer" and is not dereferenced.
 *
 * The high byte of the refcount encodes the cycle collector color
 * (see kk_cycle.h). Normal RC operations use the low 56 bits.
 */

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "kk_runtime.h"
#include "kk_cycle.h"
#include "kk_arena.h"

/* Forward declarations for string tracking (defined later) */
static void kk_register_string(int64_t ptr);
static void kk_unregister_string(int64_t ptr);
/* Forward declarations for string refcounting (different layout than cons) */
void kk_str_retain(int64_t s_i);
void kk_str_drop(int64_t s_i);

/* Raw allocation: size in bytes */
void* kk_alloc(int64_t size) {
    return malloc((size_t)size);
}

void kk_free(void* ptr) {
    free(ptr);
}

/* Check if a value is a heap pointer (vs an unboxed integer).
 * Heap pointers from kk_alloc_con are 8-byte aligned, so the low 3 bits
 * are zero and the value is above a reasonable threshold.
 * Small integers and other non-pointer values are skipped. */
/* Exported version (declared in kk_runtime.h) */
int64_t kk_is_heap_ptr(int64_t ptr) {
    /* Must be non-zero, 8-byte aligned, and in a plausible heap range.
     * Values below 4096 are almost certainly not valid heap pointers. */
    return ptr != 0 && (ptr & 7) == 0 && ptr > 4096;
}

/* Refcount helpers — pointer to refcount is at (ptr - 8) */
static inline int64_t* kk_rc_ptr(int64_t ptr) {
    return (int64_t*)(ptr - 8);
}

void kk_retain(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return;
    /* Strings have a different layout (rc at offset 0, not -8) */
    if (kk_is_string(ptr)) { kk_str_retain(ptr); return; }
    int64_t* rc = kk_rc_ptr(ptr);
    /* Increment only the count bits, preserve color */
    int64_t count = (*rc & KK_RC_MASK) + 1;
    *rc = (*rc & KK_COLOR_MASK) | count;
    /* Retained objects are live — mark black */
    *rc = (*rc & KK_RC_MASK) | KK_COLOR_BLACK;
}

int64_t kk_tag(int64_t ptr);  /* forward decl */

/* Closure objects store a raw function pointer in field 0 and captured
 * values (which may be heap pointers) in fields 1..n. The code pointer
 * is *not* a heap pointer and must not be dropped, so the codegen tags
 * closures with KK_CLOSURE_TAG and kk_drop skips field 0 for them. */
#define KK_CLOSURE_TAG 0x434C4F53  /* "CLOS" */

void kk_drop(int64_t ptr) {
    /* BOOTSTRAPPING MODE: kk_drop is a no-op.
     *
     * GHC compiles lazy let-bindings as selector thunks that share a cached
     * pair.  Perceus inserts drops in each selector for the field it doesn't
     * use.  When two selectors force the same cached pair, the first selector's
     * drop frees a field the second selector still needs → use-after-free.
     * This pattern is pervasive (464 thunk forces in Emitter.o alone).
     *
     * For the self-hosted binary, correctness matters more than memory
     * management.  Disabling kk_drop eliminates ALL use-after-free and
     * double-free bugs.  The binary leaks memory but produces correct output
     * and exits immediately afterward — acceptable for bootstrapping.
     *
     * A proper fix requires either:
     * (a) Making Perceus aware of shared lazy thunks (don't insert drops in
     *     selectors when the thunk is shared), or
     * (b) Using a tracing GC instead of refcounting for lazy evaluation. */
    (void)ptr;
}

void kk_release(int64_t ptr) {
    kk_drop(ptr);
}

int64_t kk_reuse(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    int64_t* rc = kk_rc_ptr(ptr);
    if ((*rc & KK_RC_MASK) == 1) {
        /* Sole owner — reuse the allocation */
        return ptr;
    }
    /* Shared — can't reuse, caller must allocate fresh */
    kk_drop(ptr);
    return 0;
}

/* Read the tag from a boxed value */
int64_t kk_tag(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    return *(int64_t*)ptr;
}

/* Read field[idx] from a boxed value (fields start after the tag) */
int64_t kk_field(int64_t ptr, int64_t idx) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    int64_t* fields = (int64_t*)(ptr + 8);
    return fields[idx];
}

/* Allocate a constructor: tag + nfields payload slots.
 * Returns pointer to the tag (not the refcount).
 * Layout: [rc=1] [tag] [f0] [f1] ... */
int64_t kk_alloc_con(int64_t tag, int64_t nfields) {
    int64_t total = (2 + nfields) * 8;  /* rc + tag + fields */
    /* Constructor cells go into the bump arena by default. The arena
     * returns NULL when KK_NO_ARENA is set, in which case we fall back
     * to libc malloc and rely on the matching kk_arena_free in kk_drop
     * to call free() (since kk_arena_owns will report not-owned). */
    int64_t* block = (int64_t*)kk_arena_alloc((size_t)total);
    if (!block) block = (int64_t*)malloc((size_t)total);
    if (!block) return 0;
    block[0] = KK_COLOR_BLACK | 1;  /* color=black, refcount = 1 */
    block[1] = tag;                  /* tag */
    /* Zero-init fields */
    for (int64_t i = 0; i < nfields; i++) {
        block[2 + i] = 0;
    }
    /* Return pointer to the tag slot */
    int64_t ptr = (int64_t)&block[1];
    /* Register field count for cycle collector scanning */
    kk_register_nfields(ptr, nfields);
    return ptr;
}

/* Print an ADT-valued result as an s-expression.
 *
 * Output form:
 *   - heap pointer with N fields → "(#TAG f0 f1 ... fN-1)" (fields recursively)
 *   - heap pointer with 0 fields → "(#TAG)"
 *   - non-pointer scalar         → decimal integer
 *
 * The top-level call appends a newline; recursive calls do not. We use a
 * helper to keep the public API a single one-shot printer.
 */
static void kk_print_con_inner(int64_t v) {
    if (!kk_is_heap_ptr(v)) {
        printf("%lld", (long long)v);
        return;
    }
    int64_t tag = kk_tag(v);
    int64_t nf  = kk_nfields(v);
    printf("(#%lld", (long long)tag);
    int64_t* fields = (int64_t*)(v + 8);
    for (int64_t i = 0; i < nf; i++) {
        printf(" ");
        kk_print_con_inner(fields[i]);
    }
    printf(")");
}

void kk_println_con(int64_t v) {
    kk_print_con_inner(v);
    printf("\n");
}

/* First-class strings — rope-based, UTF-8.
 *
 * A Frankenstein string is a heap-allocated kk_string_t header. The
 * header carries a refcount and either a leaf (a contiguous UTF-8
 * byte run, owned or borrowed) or a concat node (two child strings).
 *
 *   LEAF   { bytes, byte_len, owns_bytes }
 *   CONCAT { left,  right,    cached_len }
 *
 * - kk_str_concat is O(1): it allocates one CONCAT header.
 * - kk_str_len is O(1): the total byte length is cached in the header.
 * - kk_str_char_len is O(n): walks the rope, counts UTF-8 lead bytes.
 * - kk_str_flatten collapses a rope into a single owned LEAF.
 * - kk_println_str fast-paths a leaf via fwrite; ropes are flattened
 *   into a temporary buffer.
 *
 * Refcounting follows the same Perceus discipline as boxed values:
 * each header starts at rc=1; kk_str_retain bumps it; kk_str_drop
 * decrements and, on zero, recursively drops children and frees
 * owned byte buffers. Static literals use owns_bytes=0 so the
 * .rodata buffer is never freed.
 *
 * ByteStrings reuse the same kk_string_t representation but expose
 * a byte-oriented API (random byte indexing, no UTF-8 awareness).
 */

#define KK_STR_LEAF   0
#define KK_STR_CONCAT 1

typedef struct kk_string_s kk_string_t;
struct kk_string_s {
    int64_t  rc;          /* refcount (Perceus) */
    int64_t  byte_len;    /* total UTF-8 byte length, cached */
    int32_t  kind;        /* KK_STR_LEAF | KK_STR_CONCAT */
    int32_t  owns_bytes;  /* leaf only: 1 → free(bytes) on drop */
    union {
        const char* bytes;                              /* LEAF */
        struct { kk_string_t* l; kk_string_t* r; } cat; /* CONCAT */
    } u;
};

/* Forward declarations (the public header isn't included in this TU). */
int64_t kk_string_empty(void);
void    kk_str_drop(int64_t s_i);
void    kk_set_field(int64_t ptr, int64_t idx, int64_t value);

static kk_string_t* kk_str_alloc_leaf(const char* bytes, int64_t byte_len, int owns) {
    kk_string_t* s = (kk_string_t*)malloc(sizeof(kk_string_t));
    if (!s) return NULL;
    s->rc         = 1;
    s->byte_len   = byte_len;
    s->kind       = KK_STR_LEAF;
    s->owns_bytes = owns;
    s->u.bytes    = bytes;
    return s;
}

static kk_string_t* kk_str_alloc_concat(kk_string_t* l, kk_string_t* r) {
    kk_string_t* s = (kk_string_t*)malloc(sizeof(kk_string_t));
    if (!s) return NULL;
    s->rc         = 1;
    s->byte_len   = (l ? l->byte_len : 0) + (r ? r->byte_len : 0);
    s->kind       = KK_STR_CONCAT;
    s->owns_bytes = 0;
    s->u.cat.l    = l;
    s->u.cat.r    = r;
    return s;
}

int64_t kk_string_from_literal(int64_t bytes_ptr, int64_t byte_len) {
    int64_t r = (int64_t)kk_str_alloc_leaf((const char*)bytes_ptr, byte_len, 0);
    kk_register_string(r);
    return r;
}

int64_t kk_string_from_cstr(int64_t cstr_ptr) {
    const char* p = (const char*)cstr_ptr;
    if (p == NULL) return kk_string_empty();
    int64_t n = 0;
    while (p[n] != '\0') n++;
    int64_t r = (int64_t)kk_str_alloc_leaf(p, n, 0);
    kk_register_string(r);
    return r;
}

int64_t kk_string_empty(void) {
    int64_t r = (int64_t)kk_str_alloc_leaf("", 0, 0);
    kk_register_string(r);
    return r;
}

int64_t kk_str_len(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    if (s == NULL) return 0;
    return s->byte_len;
}

/* Walk a rope leaf-by-leaf, counting UTF-8 codepoints (lead bytes). */
static int64_t kk_str_char_count_rec(kk_string_t* s) {
    if (s == NULL) return 0;
    if (s->kind == KK_STR_LEAF) {
        const unsigned char* p = (const unsigned char*)s->u.bytes;
        int64_t count = 0;
        for (int64_t i = 0; i < s->byte_len; i++) {
            /* Continuation bytes start with 0b10xxxxxx */
            if ((p[i] & 0xC0) != 0x80) count++;
        }
        return count;
    }
    return kk_str_char_count_rec(s->u.cat.l) + kk_str_char_count_rec(s->u.cat.r);
}

int64_t kk_str_char_len(int64_t s_i) {
    return kk_str_char_count_rec((kk_string_t*)s_i);
}

int64_t kk_str_concat(int64_t a_i, int64_t b_i) {
    kk_string_t* a = (kk_string_t*)a_i;
    kk_string_t* b = (kk_string_t*)b_i;
    if (a == NULL || a->byte_len == 0) {
        if (a != NULL) kk_str_drop(a_i);
        return b_i;
    }
    if (b == NULL || b->byte_len == 0) {
        if (b != NULL) kk_str_drop(b_i);
        return a_i;
    }
    int64_t r = (int64_t)kk_str_alloc_concat(a, b);
    kk_register_string(r);
    return r;
}

/* Copy a rope into a contiguous buffer at *out, advancing *out. */
static void kk_str_copy_into(kk_string_t* s, char** out) {
    if (s == NULL) return;
    if (s->kind == KK_STR_LEAF) {
        for (int64_t i = 0; i < s->byte_len; i++) (*out)[i] = s->u.bytes[i];
        *out += s->byte_len;
    } else {
        kk_str_copy_into(s->u.cat.l, out);
        kk_str_copy_into(s->u.cat.r, out);
    }
}

int64_t kk_str_flatten(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    if (s == NULL) return kk_string_empty();
    if (s->kind == KK_STR_LEAF) return s_i;
    int64_t n = s->byte_len;
    char* buf = (char*)malloc((size_t)n + 1);
    if (!buf) return 0;
    char* p = buf;
    kk_str_copy_into(s, &p);
    buf[n] = '\0';
    int64_t r = (int64_t)kk_str_alloc_leaf(buf, n, 1);
    kk_register_string(r);
    return r;
}

void kk_print_str(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    if (s == NULL) return;
    if (s->kind == KK_STR_LEAF) {
        if (s->byte_len > 0) fwrite(s->u.bytes, 1, (size_t)s->byte_len, stdout);
        return;
    }
    /* Rope: flatten through a temporary buffer (no header allocation). */
    int64_t n = s->byte_len;
    char* buf = (char*)malloc((size_t)n);
    if (!buf) return;
    char* p = buf;
    kk_str_copy_into(s, &p);
    fwrite(buf, 1, (size_t)n, stdout);
    free(buf);
}

void kk_println_str(int64_t s_i) {
    kk_print_str(s_i);
    putchar('\n');
}

int64_t kk_str_eq(int64_t a_i, int64_t b_i) {
    kk_string_t* a = (kk_string_t*)a_i;
    kk_string_t* b = (kk_string_t*)b_i;
    if (a == b) return 1;
    int64_t la = (a ? a->byte_len : 0);
    int64_t lb = (b ? b->byte_len : 0);
    if (la != lb) return 0;
    if (la == 0) return 1;
    /* Flatten both into temporaries and compare. A smarter implementation
     * would walk both ropes lazily; for now this is correct and simple. */
    char* fa = (char*)malloc((size_t)la);
    char* fb = (char*)malloc((size_t)lb);
    if (!fa || !fb) { free(fa); free(fb); return 0; }
    char* pa = fa; kk_str_copy_into(a, &pa);
    char* pb = fb; kk_str_copy_into(b, &pb);
    int eq = 1;
    for (int64_t i = 0; i < la; i++) {
        if (fa[i] != fb[i]) { eq = 0; break; }
    }
    free(fa);
    free(fb);
    return eq;
}

int64_t kk_str_show_int(int64_t n) {
    /* Format a signed 64-bit integer as decimal digits, no libc deps. */
    int neg = 0;
    uint64_t u;
    if (n < 0) { neg = 1; u = (uint64_t)(-(n + 1)) + 1; }
    else       { u = (uint64_t)n; }
    char tmp[24];
    int len = 0;
    if (u == 0) { tmp[len++] = '0'; }
    else { while (u > 0) { tmp[len++] = (char)('0' + (u % 10)); u /= 10; } }
    int total = len + (neg ? 1 : 0);
    char* buf = (char*)malloc((size_t)total + 1);
    if (!buf) return 0;
    int pos = 0;
    if (neg) buf[pos++] = '-';
    for (int i = len - 1; i >= 0; i--) buf[pos++] = tmp[i];
    buf[total] = '\0';
    int64_t r = (int64_t)kk_str_alloc_leaf(buf, total, 1);
    kk_register_string(r);
    return r;
}

void kk_str_retain(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    if (s != NULL) s->rc++;
}

void kk_str_drop(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    if (s == NULL) return;
    if (--s->rc > 0) return;
    kk_unregister_string(s_i);
    if (s->kind == KK_STR_LEAF) {
        if (s->owns_bytes) free((void*)s->u.bytes);
    } else {
        kk_str_drop((int64_t)s->u.cat.l);
        kk_str_drop((int64_t)s->u.cat.r);
    }
    free(s);
}

/* Public wrappers for shim use */
int64_t kk_str_alloc_leaf_owned(const char* bytes, int64_t byte_len) {
    int64_t r = (int64_t)kk_str_alloc_leaf(bytes, byte_len, 1);
    kk_register_string(r);
    return r;
}

int64_t kk_str_byte_len(int64_t s) {
    return kk_str_len(s);
}

/* ByteString — same kk_string_t representation, byte-oriented API. */

int64_t kk_bytes_from_literal(int64_t bytes_ptr, int64_t byte_len) {
    return kk_string_from_literal(bytes_ptr, byte_len);
}

int64_t kk_bytes_len(int64_t b) {
    return kk_str_len(b);
}

int64_t kk_bytes_concat(int64_t a, int64_t b) {
    return kk_str_concat(a, b);
}

int64_t kk_bytes_eq(int64_t a, int64_t b) {
    return kk_str_eq(a, b);
}

/* Random byte access — walks the rope to find the leaf containing index i. */
static int64_t kk_bytes_index_rec(kk_string_t* s, int64_t i) {
    if (s == NULL) return -1;
    if (s->kind == KK_STR_LEAF) {
        if (i < 0 || i >= s->byte_len) return -1;
        return (int64_t)(unsigned char)s->u.bytes[i];
    }
    int64_t left_len = (s->u.cat.l ? s->u.cat.l->byte_len : 0);
    if (i < left_len) return kk_bytes_index_rec(s->u.cat.l, i);
    return kk_bytes_index_rec(s->u.cat.r, i - left_len);
}

int64_t kk_bytes_index(int64_t b_i, int64_t i) {
    return kk_bytes_index_rec((kk_string_t*)b_i, i);
}

/* File I/O, process, environment.
 *
 * Frankenstein strings flow through these intrinsics as int64_t pointers
 * to kk_string_t headers. Paths and contents are converted to NUL-
 * terminated C strings via a small helper that flattens the rope into
 * a freshly malloc'd buffer the caller must free. */

char* kk_str_dup_cstr(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    int64_t n = (s ? s->byte_len : 0);
    char* buf = (char*)malloc((size_t)n + 1);
    if (!buf) return NULL;
    char* p = buf;
    if (s) kk_str_copy_into(s, &p);
    buf[n] = '\0';
    return buf;
}

int64_t kk_read_file(int64_t path_str) {
    char* path = kk_str_dup_cstr(path_str);
    if (!path) return kk_string_empty();
    FILE* f = fopen(path, "rb");
    free(path);
    if (!f) return kk_string_empty();
    if (fseek(f, 0, SEEK_END) != 0) { fclose(f); return kk_string_empty(); }
    long size = ftell(f);
    if (size < 0) { fclose(f); return kk_string_empty(); }
    rewind(f);
    char* buf = (char*)malloc((size_t)size + 1);
    if (!buf) { fclose(f); return kk_string_empty(); }
    size_t got = fread(buf, 1, (size_t)size, f);
    fclose(f);
    buf[got] = '\0';
    { int64_t r = (int64_t)kk_str_alloc_leaf(buf, (int64_t)got, 1);
      kk_register_string(r); return r; }
}

int64_t kk_write_file(int64_t path_str, int64_t content_str) {
    char* path = kk_str_dup_cstr(path_str);
    if (!path) return -1;
    FILE* f = fopen(path, "wb");
    free(path);
    if (!f) return -1;
    kk_string_t* s = (kk_string_t*)content_str;
    int64_t n = (s ? s->byte_len : 0);
    if (n > 0) {
        char* buf = (char*)malloc((size_t)n);
        if (!buf) { fclose(f); return -1; }
        char* p = buf;
        kk_str_copy_into(s, &p);
        size_t wrote = fwrite(buf, 1, (size_t)n, f);
        free(buf);
        if (wrote != (size_t)n) { fclose(f); return -1; }
    }
    fclose(f);
    return 0;
}

int64_t kk_file_exists(int64_t path_str) {
    char* path = kk_str_dup_cstr(path_str);
    if (!path) return 0;
    FILE* f = fopen(path, "rb");
    free(path);
    if (!f) return 0;
    fclose(f);
    return 1;
}

int64_t kk_read_line(void) {
    /* Read up to a newline or EOF from stdin. Strip the trailing '\n'. */
    size_t cap = 128, len = 0;
    char* buf = (char*)malloc(cap);
    if (!buf) return kk_string_empty();
    int c;
    while ((c = getchar()) != EOF && c != '\n') {
        if (len + 1 >= cap) {
            cap *= 2;
            char* nb = (char*)realloc(buf, cap);
            if (!nb) { free(buf); return kk_string_empty(); }
            buf = nb;
        }
        buf[len++] = (char)c;
    }
    buf[len] = '\0';
    { int64_t r = (int64_t)kk_str_alloc_leaf(buf, (int64_t)len, 1);
      kk_register_string(r); return r; }
}

int64_t kk_system(int64_t cmd_str) {
    char* cmd = kk_str_dup_cstr(cmd_str);
    if (!cmd) return -1;
    int rc = system(cmd);
    free(cmd);
    return (int64_t)rc;
}

int64_t kk_getenv(int64_t name_str) {
    char* name = kk_str_dup_cstr(name_str);
    if (!name) return kk_string_empty();
    const char* val = getenv(name);
    free(name);
    if (!val) return kk_string_empty();
    /* Borrow the libc-owned string — getenv result is process-lifetime. */
    int64_t n = 0;
    while (val[n] != '\0') n++;
    { int64_t r = (int64_t)kk_str_alloc_leaf(val, n, 0);
      kk_register_string(r); return r; }
}

/* ---- Command-line arguments ----
 *
 * The main() wrapper stashes argc/argv into these globals at program
 * start. Programs read them through the intrinsics args_count / args_get,
 * which excludes argv[0] (the program name) — matching Haskell's
 * System.Environment.getArgs. The program name is available separately
 * through args_progname.
 */
static int   kk_g_argc = 0;
static char** kk_g_argv = NULL;

void kk_args_init(int argc, char** argv) {
    kk_g_argc = argc;
    kk_g_argv = argv;
}

int64_t kk_args_count(void) {
    return (int64_t)(kk_g_argc > 0 ? kk_g_argc - 1 : 0);
}

int64_t kk_args_get(int64_t i) {
    /* Map i in [0..argc-2] to argv[i+1]. Out-of-range returns empty. */
    if (i < 0 || i >= (int64_t)(kk_g_argc - 1)) return kk_string_empty();
    const char* s = kk_g_argv[i + 1];
    int64_t n = 0;
    while (s[n] != '\0') n++;
    /* Borrow argv memory — it's process-lifetime. */
    { int64_t r = (int64_t)kk_str_alloc_leaf(s, n, 0);
      kk_register_string(r); return r; }
}

int64_t kk_args_progname(void) {
    if (kk_g_argc <= 0 || kk_g_argv == NULL) return kk_string_empty();
    const char* s = kk_g_argv[0];
    int64_t n = 0;
    while (s[n] != '\0') n++;
    { int64_t r = (int64_t)kk_str_alloc_leaf(s, n, 0);
      kk_register_string(r); return r; }
}

/* Exit the process with the given status code. Wrapped so callers can
 * use it as an i64 intrinsic from MLIR without pulling in an llvm.func
 * declaration for libc exit(). */
void kk_exit(int64_t code) {
    exit((int)code);
}

/* IORef — single-field mutable cell allocated as a kk_alloc_con box.
 * Tag is "REF0"; payload index 0 holds the current value. */
#define KK_REF_TAG 0x52454630

int64_t kk_ref_new(int64_t initial) {
    int64_t cell = kk_alloc_con(KK_REF_TAG, 1);
    if (cell == 0) return 0;
    kk_set_field(cell, 0, initial);
    return cell;
}

int64_t kk_ref_get(int64_t ref) {
    return kk_field(ref, 0);
}

int64_t kk_ref_set(int64_t ref, int64_t value) {
    kk_set_field(ref, 0, value);
    return 0;
}

/* Write field[idx] of a boxed value */
void kk_set_field(int64_t ptr, int64_t idx, int64_t value) {
    if (!kk_is_heap_ptr(ptr)) return;
    int64_t* fields = (int64_t*)(ptr + 8);
    fields[idx] = value;
}

/* Evidence vector operations for algebraic effects
 *
 * An evidence vector holds function pointers (as i64) to handler
 * operations for a particular effect.  It is allocated as a regular
 * boxed constructor with tag 0xEVV0.
 */

#include <stdio.h>

#define KK_EVV_TAG 0x45565630  /* "EVV0" */

/* Create an evidence vector with nops operation slots */
int64_t kk_evv_create(int64_t nops) {
    return kk_alloc_con(KK_EVV_TAG, nops);
}

/* Set operation at index idx in the evidence vector */
void kk_evv_set(int64_t evv, int64_t idx, int64_t handler_fn) {
    kk_set_field(evv, idx, handler_fn);
}

/* Get the operation function pointer at index idx */
int64_t kk_evv_get(int64_t evv, int64_t idx) {
    return kk_field(evv, idx);
}

/* Default handler for unhandled effect operations -- abort with message */
int64_t kk_unhandled_effect(void) {
    fprintf(stderr, "frankenstein: unhandled effect operation\n");
    exit(1);
    return 0;
}

/* Mercury choice effect support — multi-shot effect via iterative path enumeration
 *
 * mercury_choose() returns 0 or 1 based on a global path variable.
 * mercury_collect_choices(fn_ptr) runs the body for all binary choice paths,
 * summing the results.  Each call to mercury_choose() reads one bit from
 * the path, so N choices yield up to 2^N paths.
 *
 * Short paths (where the body makes fewer choices than the max depth)
 * are deduplicated: a result is only counted when the unused high bits
 * of the path are all zero.
 */

#include <string.h>

static int64_t mercury_choice_decisions[64];
static int64_t mercury_choice_pos = 0;

int64_t mercury_choose(void) {
    return mercury_choice_decisions[mercury_choice_pos++];
}

int64_t mercury_collect_choices(int64_t fn_ptr) {
    typedef int64_t (*body_fn_t)(void);
    body_fn_t body = (body_fn_t)fn_ptr;

    /* Phase 1: discover max depth by running with all-0 decisions */
    memset(mercury_choice_decisions, 0, sizeof(mercury_choice_decisions));
    mercury_choice_pos = 0;
    body();
    int64_t max_depth = mercury_choice_pos;
    if (max_depth == 0) {
        /* No choices made — single deterministic result */
        mercury_choice_pos = 0;
        return body();
    }

    /* Phase 2: enumerate all 2^max_depth paths */
    int64_t total_paths = 1LL << max_depth;
    int64_t sum = 0;
    for (int64_t path = 0; path < total_paths; path++) {
        /* Set decisions from bits of path */
        for (int64_t i = 0; i < max_depth; i++) {
            mercury_choice_decisions[i] = (path >> i) & 1;
        }
        mercury_choice_pos = 0;
        int64_t result = body();
        int64_t depth = mercury_choice_pos;

        /* Only count if unused bits (beyond depth) are all zero.
         * This avoids double-counting short paths that are reached
         * by multiple path prefixes. */
        int64_t used_mask = (1LL << depth) - 1;
        if ((path & used_mask) == path) {
            sum += result;
        }
    }
    return sum;
}

/* Thunk support for lazy evaluation (Haskell bridge)
 *
 * Thunk layout (using kk_alloc_con with tag=0xLAZY):
 *   [refcount] [tag=0x4C415A59] [evaluated_flag] [value_or_fn_ptr]
 *
 * evaluated_flag: 0 = unevaluated (value_or_fn_ptr is fn_ptr)
 *                 1 = evaluated   (value_or_fn_ptr is cached result)
 */

#define KK_THUNK_TAG 0x4C415A59  /* "LAZY" */

/* Create a thunk wrapping a zero-arg function pointer */
int64_t kk_thunk_create(int64_t fn_ptr) {
    int64_t thunk = kk_alloc_con(KK_THUNK_TAG, 2);
    if (thunk == 0) return 0;
    kk_set_field(thunk, 0, 0);        /* evaluated_flag = 0 */
    kk_set_field(thunk, 1, fn_ptr);   /* the function pointer */
    return thunk;
}

/* Force a thunk: if unevaluated, call the function and cache the result */
int64_t kk_thunk_force(int64_t thunk) {
    if (!kk_is_heap_ptr(thunk)) return thunk;  /* not a thunk, return as-is */
    int64_t tag = kk_tag(thunk);
    if (tag != KK_THUNK_TAG) return thunk;     /* not a thunk, return as-is */
    int64_t evaluated = kk_field(thunk, 0);
    if (evaluated) {
        return kk_field(thunk, 1);             /* already forced */
    }
    /* Call the zero-arg function */
    int64_t fn_ptr = kk_field(thunk, 1);
    typedef int64_t (*thunk_fn_t)(void);
    int64_t result = ((thunk_fn_t)fn_ptr)();
    /* Cache the result */
    kk_set_field(thunk, 0, 1);                /* mark as evaluated */
    kk_set_field(thunk, 1, result);           /* store result */
    return result;
}

/* ================================================================== */
/*  String tracking — distinguish kk_string_t from kk_alloc_con       */
/* ================================================================== */

#define KK_STRING_TABLE_SIZE 8192
static int64_t string_table[KK_STRING_TABLE_SIZE];

static void kk_register_string(int64_t ptr) {
    int64_t idx = (ptr >> 3) & (KK_STRING_TABLE_SIZE - 1);
    for (int64_t i = 0; i < KK_STRING_TABLE_SIZE; i++) {
        int64_t probe = (idx + i) & (KK_STRING_TABLE_SIZE - 1);
        if (string_table[probe] == 0 || string_table[probe] == ptr) {
            string_table[probe] = ptr;
            return;
        }
    }
}

static void kk_unregister_string(int64_t ptr) {
    int64_t idx = (ptr >> 3) & (KK_STRING_TABLE_SIZE - 1);
    for (int64_t i = 0; i < KK_STRING_TABLE_SIZE; i++) {
        int64_t probe = (idx + i) & (KK_STRING_TABLE_SIZE - 1);
        if (string_table[probe] == ptr) {
            string_table[probe] = 0;
            return;
        }
        if (string_table[probe] == 0) return;
    }
}

int64_t kk_is_string(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    int64_t idx = (ptr >> 3) & (KK_STRING_TABLE_SIZE - 1);
    for (int64_t i = 0; i < KK_STRING_TABLE_SIZE; i++) {
        int64_t probe = (idx + i) & (KK_STRING_TABLE_SIZE - 1);
        if (string_table[probe] == ptr) return 1;
        if (string_table[probe] == 0)  return 0;
    }
    return 0;
}

/* ================================================================== */
/*  String comparison                                                  */
/* ================================================================== */

/* Flatten a kk_string to a C string for comparison.
 * Returns a malloc'd buffer that the caller must free. */
static const char* kk_str_flatten_cmp(int64_t s, int64_t *out_len) {
    kk_string_t* str = (kk_string_t*)s;
    if (!str) { *out_len = 0; return ""; }
    /* Flatten the rope so we get contiguous bytes */
    int64_t flat = kk_str_flatten(s);
    kk_string_t* f = (kk_string_t*)flat;
    *out_len = f->byte_len;
    return f->u.bytes;
}

int64_t kk_str_compare(int64_t a, int64_t b) {
    if (a == b) return 0;
    int64_t len_a, len_b;
    const char* sa = kk_str_flatten_cmp(a, &len_a);
    const char* sb = kk_str_flatten_cmp(b, &len_b);
    int64_t min_len = len_a < len_b ? len_a : len_b;
    int cmp = memcmp(sa, sb, (size_t)min_len);
    if (cmp != 0) return cmp;
    return (len_a > len_b) - (len_a < len_b);
}

/* ================================================================== */
/*  Generic structural comparison                                      */
/* ================================================================== */

int64_t kk_compare(int64_t a, int64_t b) {
    if (a == b) return 0;
    /* Both strings? */
    if (kk_is_string(a) && kk_is_string(b))
        return kk_str_compare(a, b);
    /* Both heap objects (con)? Compare by tag, then fields. */
    if (kk_is_heap_ptr(a) && kk_is_heap_ptr(b) &&
        !kk_is_string(a) && !kk_is_string(b)) {
        int64_t ta = kk_tag(a), tb = kk_tag(b);
        if (ta != tb) return (ta > tb) - (ta < tb);
        int64_t nfa = kk_nfields(a), nfb = kk_nfields(b);
        int64_t n = nfa < nfb ? nfa : nfb;
        for (int64_t i = 0; i < n; i++) {
            int64_t c = kk_compare(kk_field(a, i), kk_field(b, i));
            if (c != 0) return c;
        }
        return (nfa > nfb) - (nfa < nfb);
    }
    /* Fallback: integer comparison */
    return (a > b) - (a < b);
}

/* ================================================================== */
/*  List / Tuple / Maybe helpers                                       */
/* ================================================================== */

int64_t kk_nil(void) {
    return kk_alloc_con(KK_NIL_TAG, 0);
}

int64_t kk_cons(int64_t head, int64_t tail) {
    int64_t c = kk_alloc_con(KK_CONS_TAG, 2);
    kk_set_field(c, 0, head);
    kk_set_field(c, 1, tail);
    return c;
}

int64_t kk_list_head(int64_t list) { return kk_field(list, 0); }
int64_t kk_list_tail(int64_t list) { return kk_field(list, 1); }

int64_t kk_is_nil(int64_t list) {
    if (!kk_is_heap_ptr(list)) return 1;
    return kk_tag(list) == KK_NIL_TAG && kk_nfields(list) == 0;
}

int64_t kk_pair(int64_t a, int64_t b) {
    int64_t p = kk_alloc_con(0, 2);
    kk_set_field(p, 0, a);
    kk_set_field(p, 1, b);
    return p;
}

int64_t kk_fst(int64_t pair) { return kk_field(pair, 0); }
int64_t kk_snd(int64_t pair) { return kk_field(pair, 1); }

int64_t kk_nothing(void) { return kk_alloc_con(KK_NOTHING_TAG, 0); }
int64_t kk_just(int64_t x) {
    int64_t j = kk_alloc_con(KK_JUST_TAG, 1);
    kk_set_field(j, 0, x);
    return j;
}
