/* GHC List, Foldable, Traversable, OldList, Maybe, Either, Functor,
 * Tuple, Data.String shims.
 *
 * All values are int64_t.  Lists: cons (tag 1, 2 fields), nil (tag 0).
 * Closures: field 0 = fptr, fields 1..n = captures.
 * Operator names use Z-encoding (!! → znzn, <$> → zlzdzg, etc.).
 * Dictionary selectors: $f... → zdf..., $p... → zdp...
 */

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../runtime/kk_runtime.h"

#define KK_CLOSURE_TAG 0x434C4F53  /* 'CLOS' */

/* State monad fmap runner (defined in shim_ghc_prim.c, used for dispatch) */
extern int64_t kk_pair(int64_t a, int64_t b);
extern int64_t kk_fst(int64_t p);
extern int64_t kk_snd(int64_t p);

typedef int64_t (*fn1_t)(int64_t, int64_t);
typedef int64_t (*fn2_t)(int64_t, int64_t, int64_t);

static int64_t call1(int64_t clos, int64_t a) {
    /* Retain argument — the closure (compiled with Perceus) may consume it. */
    kk_retain(a);
    clos = kk_thunk_force(clos);
    if (!kk_is_heap_ptr(clos)) {
        typedef int64_t (*raw1_t)(int64_t);
        raw1_t fn = (raw1_t)(intptr_t)clos;
        if (__builtin_expect(!fn, 0)) {
            fprintf(stderr, "FATAL: call1 null closure! a=%p (0x%lx)\n",
                    (void*)a, (unsigned long)a);
            exit(99);
        }
        return fn(a);
    }
    int64_t fp = kk_field(clos, 0);
    if (fp == 0) {
        fprintf(stderr, "FATAL: call1 null fp! clos=%p tag=%ld nf=%ld a=%p\n",
                (void*)clos, (long)kk_tag(clos), (long)kk_nfields(clos), (void*)a);
        for (int64_t i = 0; i < kk_nfields(clos) && i < 8; i++) {
            int64_t fi = kk_field(clos, i);
            fprintf(stderr, "  field[%ld] = %ld (0x%lx) heap=%d",
                    (long)i, (long)fi, (unsigned long)fi, kk_is_heap_ptr(fi));
            if (kk_is_heap_ptr(fi) && kk_is_string(fi)) {
                char* s = kk_str_dup_cstr(fi);
                fprintf(stderr, " str=\"%.40s\"", s);
                free(s);
            }
            fprintf(stderr, "\n");
        }
        exit(99);
    }
    return ((fn1_t)(intptr_t)fp)(clos, a);
}
static int64_t call2(int64_t clos, int64_t a, int64_t b) {
    kk_retain(a);
    kk_retain(b);
    clos = kk_thunk_force(clos);
    if (!kk_is_heap_ptr(clos)) {
        typedef int64_t (*raw2_t)(int64_t, int64_t);
        return ((raw2_t)(intptr_t)clos)(a, b);
    }
    return ((fn2_t)(intptr_t)kk_field(clos, 0))(clos, a, b);
}

#define CLOS_TAG 0x434C4F53
static int64_t make_closure0(void* fp) {
    int64_t c = kk_alloc_con(CLOS_TAG, 1);
    kk_set_field(c, 0, (int64_t)(intptr_t)fp);
    return c;
}
static int64_t make_closure1(void* fp, int64_t c1) {
    int64_t c = kk_alloc_con(CLOS_TAG, 2);
    kk_set_field(c, 0, (int64_t)(intptr_t)fp);
    kk_retain(c1);
    kk_set_field(c, 1, c1);
    return c;
}
static int64_t make_closure2(void* fp, int64_t c1, int64_t c2) {
    int64_t c = kk_alloc_con(CLOS_TAG, 3);
    kk_set_field(c, 0, (int64_t)(intptr_t)fp);
    kk_retain(c1);
    kk_retain(c2);
    kk_set_field(c, 1, c1);
    kk_set_field(c, 2, c2);
    return c;
}

/* ------------------------------------------------------------------ */
/*  Bool representation helpers.                                         */
/*  Compiled Haskell code represents True/False as heap-allocated        */
/*  constructors with stableConTag tags.  C shims use 0/1.               */
/* ------------------------------------------------------------------ */
#define KK_TRUE_TAG  24914   /* stableConTag "True"  */
#define KK_FALSE_TAG 44872   /* stableConTag "False" */

/* Convert a tagged Bool (or C int) to C int.  Handles both:
 *  - C-style: 0 = false, nonzero = true
 *  - Tagged:  heap object with tag KK_TRUE_TAG or KK_FALSE_TAG        */
static inline int tobool(int64_t v) {
    if (v == 0) return 0;
    if (v == 1) return 1;
    if (kk_is_heap_ptr(v)) return kk_tag(v) == KK_TRUE_TAG;
    return v != 0;  /* fallback for raw ints */
}

/* ------------------------------------------------------------------ */
/*  Forward declarations for closure code pointers                      */
/* ------------------------------------------------------------------ */

static int64_t filter_apply(int64_t clos, int64_t xs);
static int64_t reverse_apply(int64_t clos, int64_t xs);
static int64_t dropWhile_apply(int64_t clos, int64_t xs);
static int64_t any_apply(int64_t clos, int64_t xs);
static int64_t concat_code(int64_t clos, int64_t xss);
static int64_t elem_1_code(int64_t clos, int64_t x);
static int64_t elem_2_code(int64_t clos, int64_t xs);
static int64_t find_apply(int64_t clos, int64_t xs);
static int64_t forM_apply(int64_t clos, int64_t f);
static int64_t sum_code(int64_t clos, int64_t xs);
static int64_t fst_code(int64_t clos, int64_t p);
static int64_t snd_code(int64_t clos, int64_t p);
static int64_t isDigit_code(int64_t clos, int64_t c);
static int64_t isSpace_code(int64_t clos, int64_t c);

/* helper: list to array (caller frees), returns count */
static int64_t list_to_array(int64_t xs, int64_t **out) {
    int64_t cap = 16, n = 0;
    int64_t *arr = malloc((size_t)cap * sizeof(int64_t));
    int64_t cur = xs;
    while (!kk_is_nil(cur)) {
        if (n >= cap) { cap *= 2; arr = realloc(arr, (size_t)cap * sizeof(int64_t)); }
        arr[n++] = kk_list_head(cur);
        cur = kk_list_tail(cur);
    }
    *out = arr;
    return n;
}

static int64_t array_to_list(int64_t *arr, int64_t n) {
    int64_t result = kk_nil();
    for (int64_t i = n - 1; i >= 0; i--) result = kk_cons(arr[i], result);
    return result;
}

/* ================================================================== */
/*  GHC.Internal.List                                                   */
/* ================================================================== */

/* list_append helper (used by !! indexing fallback etc.) */
static int64_t list_append(int64_t xs, int64_t ys) {
    int64_t *arr; int64_t n = list_to_array(xs, &arr);
    int64_t result = ys;
    for (int64_t i = n - 1; i >= 0; i--) result = kk_cons(arr[i], result);
    free(arr);
    return result;
}

/* !! (Z-encoded: znzn) — list index */
int64_t ghc_list_index_2(int64_t xs, int64_t n) __asm__("GHC_Internal_List_znzn$2");
int64_t ghc_list_index_2(int64_t xs, int64_t n) {
    while (n > 0 && !kk_is_nil(xs)) { xs = kk_list_tail(xs); n--; }
    if (kk_is_nil(xs)) { fprintf(stderr, "Prelude.!!: index too large\n"); abort(); }
    return kk_list_head(xs);
}
int64_t ghc_list_index_3(int64_t d, int64_t xs, int64_t n) __asm__("GHC_Internal_List_znzn$3");
int64_t ghc_list_index_3(int64_t d, int64_t xs, int64_t n) { (void)d; return ghc_list_index_2(xs, n); }

int64_t ghc_list_head_1(int64_t xs) __asm__("GHC_Internal_List_head$1");
int64_t ghc_list_head_1(int64_t xs) { return kk_list_head(xs); }

int64_t ghc_list_init_2(int64_t xs) __asm__("GHC_Internal_List_init$2");
int64_t ghc_list_init_2(int64_t xs) {
    int64_t *arr; int64_t n = list_to_array(xs, &arr);
    int64_t result = n > 0 ? array_to_list(arr, n - 1) : kk_nil();
    free(arr);
    return result;
}

int64_t ghc_list_last_1(int64_t xs) __asm__("GHC_Internal_List_last$1");
int64_t ghc_list_last_1(int64_t xs) {
    int64_t v = 0;
    while (!kk_is_nil(xs)) { v = kk_list_head(xs); xs = kk_list_tail(xs); }
    return v;
}
int64_t ghc_list_last_2(int64_t d, int64_t xs) __asm__("GHC_Internal_List_last$2");
int64_t ghc_list_last_2(int64_t d, int64_t xs) { (void)d; return ghc_list_last_1(xs); }

int64_t ghc_list_drop_2(int64_t n, int64_t xs) __asm__("GHC_Internal_List_drop$2");
int64_t ghc_list_drop_2(int64_t n, int64_t xs) {
    while (n > 0 && !kk_is_nil(xs)) { xs = kk_list_tail(xs); n--; }
    return xs;
}

int64_t ghc_list_take_2(int64_t n, int64_t xs) __asm__("GHC_Internal_List_take$2");
int64_t ghc_list_take_2(int64_t n, int64_t xs) {
    int64_t *arr = malloc((size_t)n * sizeof(int64_t));
    int64_t count = 0;
    while (count < n && !kk_is_nil(xs)) {
        arr[count++] = kk_list_head(xs);
        xs = kk_list_tail(xs);
    }
    int64_t result = array_to_list(arr, count);
    free(arr);
    return result;
}

int64_t ghc_list_filter_1(int64_t p) __asm__("GHC_Internal_List_filter$1");
int64_t ghc_list_filter_1(int64_t p) { return make_closure1(&filter_apply, p); }
int64_t ghc_list_filter_2(int64_t p, int64_t xs) __asm__("GHC_Internal_List_filter$2");
int64_t ghc_list_filter_2(int64_t p, int64_t xs) {
    int64_t *arr; int64_t n = list_to_array(xs, &arr);
    int64_t count = 0;
    for (int64_t i = 0; i < n; i++)
        if (tobool(call1(p, arr[i]))) arr[count++] = arr[i];
    int64_t result = array_to_list(arr, count);
    free(arr);
    return result;
}
static int64_t filter_apply(int64_t clos, int64_t xs) {
    return ghc_list_filter_2(kk_field(clos, 1), xs);
}

int64_t ghc_list_reverse_0(void) __asm__("GHC_Internal_List_reverse$0");
int64_t ghc_list_reverse_0(void) { return make_closure0(&reverse_apply); }
int64_t ghc_list_reverse_1(int64_t xs) __asm__("GHC_Internal_List_reverse$1");
int64_t ghc_list_reverse_1(int64_t xs) {
    int64_t result = kk_nil();
    while (!kk_is_nil(xs)) { result = kk_cons(kk_list_head(xs), result); xs = kk_list_tail(xs); }
    return result;
}
static int64_t reverse_apply(int64_t clos, int64_t xs) { (void)clos; return ghc_list_reverse_1(xs); }

int64_t ghc_list_zip_2(int64_t xs, int64_t ys) __asm__("GHC_Internal_List_zip$2");
int64_t ghc_list_zip_2(int64_t xs, int64_t ys) {
    int64_t *arr; int64_t cap = 16, n = 0;
    arr = malloc((size_t)cap * sizeof(int64_t));
    while (!kk_is_nil(xs) && !kk_is_nil(ys)) {
        if (n >= cap) { cap *= 2; arr = realloc(arr, (size_t)cap * sizeof(int64_t)); }
        arr[n++] = kk_pair(kk_list_head(xs), kk_list_head(ys));
        xs = kk_list_tail(xs); ys = kk_list_tail(ys);
    }
    int64_t result = array_to_list(arr, n);
    free(arr);
    return result;
}

int64_t ghc_list_zipWith_3(int64_t f, int64_t xs, int64_t ys) __asm__("GHC_Internal_List_zipWith$3");
int64_t ghc_list_zipWith_3(int64_t f, int64_t xs, int64_t ys) {
    int64_t *arr; int64_t cap = 16, n = 0;
    arr = malloc((size_t)cap * sizeof(int64_t));
    while (!kk_is_nil(xs) && !kk_is_nil(ys)) {
        if (n >= cap) { cap *= 2; arr = realloc(arr, (size_t)cap * sizeof(int64_t)); }
        arr[n++] = call2(f, kk_list_head(xs), kk_list_head(ys));
        xs = kk_list_tail(xs); ys = kk_list_tail(ys);
    }
    int64_t result = array_to_list(arr, n);
    free(arr);
    return result;
}

int64_t ghc_list_span_2(int64_t p, int64_t xs) __asm__("GHC_Internal_List_span$2");
int64_t ghc_list_span_2(int64_t p, int64_t xs) {
    int64_t *arr; int64_t n = list_to_array(xs, &arr);
    int64_t split = 0;
    while (split < n && call1(p, arr[split])) split++;
    int64_t left = array_to_list(arr, split);
    int64_t right = array_to_list(arr + split, n - split);
    free(arr);
    return kk_pair(left, right);
}

int64_t ghc_list_splitAt_2(int64_t n, int64_t xs) __asm__("GHC_Internal_List_splitAt$2");
int64_t ghc_list_splitAt_2(int64_t n, int64_t xs) {
    int64_t left = ghc_list_take_2(n, xs);
    int64_t right = ghc_list_drop_2(n, xs);
    return kk_pair(left, right);
}

int64_t ghc_list_replicate_2(int64_t n, int64_t x) __asm__("GHC_Internal_List_replicate$2");
int64_t ghc_list_replicate_2(int64_t n, int64_t x) {
    int64_t result = kk_nil();
    for (int64_t i = 0; i < n; i++) result = kk_cons(x, result);
    return result;
}

int64_t ghc_list_repeat_1(int64_t x) __asm__("GHC_Internal_List_repeat$1");
int64_t ghc_list_repeat_1(int64_t x) { return ghc_list_replicate_2(10000, x); }

int64_t ghc_list_lookup_2(int64_t key, int64_t xs) __asm__("GHC_Internal_List_lookup$2");
int64_t ghc_list_lookup_2(int64_t key, int64_t xs) {
    while (!kk_is_nil(xs)) {
        int64_t pair = kk_list_head(xs);
        int64_t k = kk_fst(pair);
        if (kk_is_string(key) && kk_is_string(k)) {
            if (kk_str_eq(key, k)) return kk_just(kk_snd(pair));
        } else if (key == k) {
            return kk_just(kk_snd(pair));
        }
        xs = kk_list_tail(xs);
    }
    return kk_nothing();
}

/* lookup$0: lookup as a closure (2-arg function value) */
static int64_t tram_lookup(int64_t clos, int64_t key, int64_t xs) {
    (void)clos;
    return ghc_list_lookup_2(key, xs);
}
int64_t ghc_list_lookup_0(void) __asm__("GHC_Internal_List_lookup$0");
int64_t ghc_list_lookup_0(void) {
    int64_t c = kk_alloc_con(0x434C4F53, 1);
    kk_set_field(c, 0, (int64_t)(intptr_t)tram_lookup);
    return c;
}

int64_t ghc_list_dropWhile_1(int64_t p) __asm__("GHC_Internal_List_dropWhile$1");
int64_t ghc_list_dropWhile_1(int64_t p) { return make_closure1(&dropWhile_apply, p); }
int64_t ghc_list_dropWhile_2(int64_t p, int64_t xs) __asm__("GHC_Internal_List_dropWhile$2");
int64_t ghc_list_dropWhile_2(int64_t p, int64_t xs) {
    while (!kk_is_nil(xs) && call1(p, kk_list_head(xs))) xs = kk_list_tail(xs);
    return xs;
}
static int64_t dropWhile_apply(int64_t clos, int64_t xs) {
    return ghc_list_dropWhile_2(kk_field(clos, 1), xs);
}

int64_t ghc_list_takeWhile_2(int64_t p, int64_t xs) __asm__("GHC_Internal_List_takeWhile$2");
int64_t ghc_list_takeWhile_2(int64_t p, int64_t xs) {
    int64_t *arr; int64_t cap = 16, n = 0;
    arr = malloc((size_t)cap * sizeof(int64_t));
    while (!kk_is_nil(xs)) {
        int64_t h = kk_list_head(xs);
        if (!call1(p, h)) break;
        if (n >= cap) { cap *= 2; arr = realloc(arr, (size_t)cap * sizeof(int64_t)); }
        arr[n++] = h;
        xs = kk_list_tail(xs);
    }
    int64_t result = array_to_list(arr, n);
    free(arr);
    return result;
}

int64_t ghc_list_scanl_3(int64_t f, int64_t z, int64_t xs) __asm__("GHC_Internal_List_scanl$3");
int64_t ghc_list_scanl_3(int64_t f, int64_t z, int64_t xs) {
    int64_t *arr; int64_t cap = 16, n = 0;
    arr = malloc((size_t)cap * sizeof(int64_t));
    int64_t acc = z;
    if (n >= cap) { cap *= 2; arr = realloc(arr, (size_t)cap * sizeof(int64_t)); }
    arr[n++] = acc;
    while (!kk_is_nil(xs)) {
        acc = call2(f, acc, kk_list_head(xs));
        if (n >= cap) { cap *= 2; arr = realloc(arr, (size_t)cap * sizeof(int64_t)); }
        arr[n++] = acc;
        xs = kk_list_tail(xs);
    }
    int64_t result = array_to_list(arr, n);
    free(arr);
    return result;
}

/* ================================================================== */
/*  GHC.Internal.Data.Foldable                                          */
/* ================================================================== */

int64_t ghc_foldable_all_2(int64_t p, int64_t xs) __asm__("GHC_Internal_Data_Foldable_all$2");
int64_t ghc_foldable_all_2(int64_t p, int64_t xs) {
    while (!kk_is_nil(xs)) {
        if (!tobool(call1(p, kk_list_head(xs)))) return 0;
        xs = kk_list_tail(xs);
    }
    return 1;
}

int64_t ghc_foldable_any_1(int64_t p) __asm__("GHC_Internal_Data_Foldable_any$1");
int64_t ghc_foldable_any_1(int64_t p) { return make_closure1(&any_apply, p); }
int64_t ghc_foldable_any_2(int64_t p, int64_t xs) __asm__("GHC_Internal_Data_Foldable_any$2");
int64_t ghc_foldable_any_2(int64_t p, int64_t xs) {
    while (!kk_is_nil(xs)) {
        if (tobool(call1(p, kk_list_head(xs)))) return 1;
        xs = kk_list_tail(xs);
    }
    return 0;
}
static int64_t any_apply(int64_t clos, int64_t xs) { return ghc_foldable_any_2(kk_field(clos,1), xs); }

int64_t ghc_foldable_concat_0(void) __asm__("GHC_Internal_Data_Foldable_concat$0");
int64_t ghc_foldable_concat_0(void) { return make_closure0(&concat_code); }
int64_t ghc_foldable_concat_1(int64_t xss) __asm__("GHC_Internal_Data_Foldable_concat$1");
int64_t ghc_foldable_concat_1(int64_t xss) {
    int64_t result = kk_nil();
    /* Collect all sub-lists, then append in reverse order */
    int64_t *arr; int64_t n = list_to_array(xss, &arr);
    for (int64_t i = n - 1; i >= 0; i--) result = list_append(arr[i], result);
    free(arr);
    return result;
}
static int64_t concat_code(int64_t clos, int64_t xss) { (void)clos; return ghc_foldable_concat_1(xss); }

int64_t ghc_foldable_concatMap_2(int64_t f, int64_t xs) __asm__("GHC_Internal_Data_Foldable_concatMap$2");
int64_t ghc_foldable_concatMap_2(int64_t f, int64_t xs) {
    int64_t result = kk_nil();
    int64_t *arr; int64_t n = list_to_array(xs, &arr);
    for (int64_t i = n - 1; i >= 0; i--) result = list_append(call1(f, arr[i]), result);
    free(arr);
    return result;
}

int64_t ghc_foldable_elem_0(void) __asm__("GHC_Internal_Data_Foldable_elem$0");
int64_t ghc_foldable_elem_0(void) { return make_closure0(&elem_1_code); }
int64_t ghc_foldable_elem_2(int64_t x, int64_t xs) __asm__("GHC_Internal_Data_Foldable_elem$2");
int64_t ghc_foldable_elem_2(int64_t x, int64_t xs) {
    /* xs may be a rope/Text string (e.g. from kk_string_from_literal used for
     * a [Char] literal with OverloadedStrings).  In that case, iterate UTF-8
     * codepoints and compare against x (a boxed Char, field 0 = codepoint). */
    if (kk_is_string(xs)) {
        char* buf = kk_str_dup_cstr(xs);
        int64_t len = kk_str_len(xs);
        /* Extract the codepoint from x (boxed Char, field 0 = codepoint) */
        int64_t cp_x = kk_is_heap_ptr(x) ? kk_field(x, 0) : x;
        for (int64_t i = 0; i < len; ) {
            unsigned char b = (unsigned char)buf[i];
            int64_t cp;
            if (b < 0x80)      { cp = b; i += 1; }
            else if (b < 0xE0) { cp = ((b & 0x1F) << 6) | (buf[i+1] & 0x3F); i += 2; }
            else if (b < 0xF0) { cp = ((b & 0x0F) << 12) | ((buf[i+1] & 0x3F) << 6) | (buf[i+2] & 0x3F); i += 3; }
            else               { cp = ((b & 0x07) << 18) | ((buf[i+1] & 0x3F) << 12) | ((buf[i+2] & 0x3F) << 6) | (buf[i+3] & 0x3F); i += 4; }
            if (cp == cp_x) { free(buf); return 1; }
        }
        free(buf);
        return 0;
    }
    while (!kk_is_nil(xs)) {
        int64_t h = kk_list_head(xs);
        if (kk_is_string(x) && kk_is_string(h)) { if (kk_str_eq(x, h)) return 1; }
        else if (x == h) return 1;
        xs = kk_list_tail(xs);
    }
    return 0;
}
static int64_t elem_1_code(int64_t clos, int64_t x) {
    (void)clos;
    return make_closure1(&elem_2_code, x);
}
static int64_t elem_2_code(int64_t clos, int64_t xs) {
    return ghc_foldable_elem_2(kk_field(clos, 1), xs);
}

/* notElem :: Eq a => a -> [a] -> Bool */
static int64_t notelem_1_code(int64_t clos, int64_t x);
static int64_t notelem_2_code(int64_t clos, int64_t xs);

int64_t ghc_foldable_notElem_0(void) __asm__("GHC_Internal_Data_Foldable_notElem$0");
int64_t ghc_foldable_notElem_0(void) { return make_closure0(&notelem_1_code); }
int64_t ghc_foldable_notElem_2(int64_t x, int64_t xs) __asm__("GHC_Internal_Data_Foldable_notElem$2");
int64_t ghc_foldable_notElem_2(int64_t x, int64_t xs) {
    return ghc_foldable_elem_2(x, xs) ? 0 : 1;
}
static int64_t notelem_1_code(int64_t clos, int64_t x) {
    (void)clos;
    return make_closure1(&notelem_2_code, x);
}
static int64_t notelem_2_code(int64_t clos, int64_t xs) {
    return ghc_foldable_notElem_2(kk_field(clos, 1), xs);
}

int64_t ghc_foldable_find_1(int64_t p) __asm__("GHC_Internal_Data_Foldable_find$1");
int64_t ghc_foldable_find_1(int64_t p) { return make_closure1(&find_apply, p); }
int64_t ghc_foldable_find_2(int64_t p, int64_t xs) __asm__("GHC_Internal_Data_Foldable_find$2");
int64_t ghc_foldable_find_2(int64_t p, int64_t xs) {
    while (!kk_is_nil(xs)) {
        int64_t h = kk_list_head(xs);
        if (tobool(call1(p, h))) return kk_just(h);
        xs = kk_list_tail(xs);
    }
    return kk_nothing();
}
static int64_t find_apply(int64_t clos, int64_t xs) { return ghc_foldable_find_2(kk_field(clos,1), xs); }

int64_t ghc_foldable_foldl_3(int64_t f, int64_t z, int64_t xs) __asm__("GHC_Internal_Data_Foldable_foldl$3");
int64_t ghc_foldable_foldl_3(int64_t f, int64_t z, int64_t xs) {
    int64_t acc = z;
    while (!kk_is_nil(xs)) { acc = call2(f, acc, kk_list_head(xs)); xs = kk_list_tail(xs); }
    return acc;
}

int64_t ghc_foldable_foldr_3(int64_t f, int64_t z, int64_t xs) __asm__("GHC_Internal_Data_Foldable_foldr$3");
int64_t ghc_foldable_foldr_3(int64_t f, int64_t z, int64_t xs) {
    /* Force xs: in plotkin mode the list may arrive as a thunk
     * (kk_thunk_force is a no-op on non-thunks). See ABI audit
     * boundary B in docs/plotkin-abi-audit.md. */
    xs = kk_thunk_force(xs);
    if (kk_is_nil(xs)) return z;
    return call2(f, kk_list_head(xs), ghc_foldable_foldr_3(f, z, kk_list_tail(xs)));
}

/* forM_ for State monad: like mapM_ but with args flipped.
 * Returns a State action that threads state through f applied to each element. */
static int64_t forM_state_runner(int64_t clos, int64_t s) {
    int64_t xs = kk_field(clos, 1);
    int64_t f  = kk_field(clos, 2);
    kk_retain(f);
    while (!kk_is_nil(xs)) {
        kk_retain(f);
        int64_t action = call1(f, kk_list_head(xs));
        int64_t pair = call1(action, s);
        s = kk_snd(pair);
        xs = kk_list_tail(xs);
    }
    return kk_pair(0, s);
}
static int64_t forM_apply(int64_t clos, int64_t f) {
    int64_t xs = kk_field(clos, 1);
    return make_closure2(&forM_state_runner, xs, f);
}
int64_t ghc_foldable_forM_1(int64_t xs) __asm__("GHC_Internal_Data_Foldable_forM_$1");
int64_t ghc_foldable_forM_1(int64_t xs) { return make_closure1(&forM_apply, xs); }

int64_t ghc_foldable_length_1(int64_t xs) __asm__("GHC_Internal_Data_Foldable_length$1");
int64_t ghc_foldable_length_1(int64_t xs) {
    int64_t n = 0;
    while (!kk_is_nil(xs)) { n++; xs = kk_list_tail(xs); }
    return n;
}

int64_t ghc_foldable_maximum_1(int64_t xs) __asm__("GHC_Internal_Data_Foldable_maximum$1");
int64_t ghc_foldable_maximum_1(int64_t xs) {
    int64_t m = kk_list_head(xs); xs = kk_list_tail(xs);
    while (!kk_is_nil(xs)) { int64_t h = kk_list_head(xs); if (h > m) m = h; xs = kk_list_tail(xs); }
    return m;
}

int64_t ghc_foldable_null_1(int64_t xs) __asm__("GHC_Internal_Data_Foldable_null$1");
int64_t ghc_foldable_null_1(int64_t xs) { return kk_is_nil(xs) ? 1 : 0; }

int64_t ghc_foldable_sum_0(void) __asm__("GHC_Internal_Data_Foldable_sum$0");
int64_t ghc_foldable_sum_0(void) { return make_closure0(&sum_code); }
int64_t ghc_foldable_sum_1(int64_t xs) __asm__("GHC_Internal_Data_Foldable_sum$1");
int64_t ghc_foldable_sum_1(int64_t xs) {
    int64_t s = 0;
    while (!kk_is_nil(xs)) { s += kk_list_head(xs); xs = kk_list_tail(xs); }
    return s;
}
static int64_t sum_code(int64_t clos, int64_t xs) { (void)clos; return ghc_foldable_sum_1(xs); }

/* ================================================================== */
/*  GHC.Internal.Data.Traversable                                       */
/* ================================================================== */

/* mapM for the State monad: f returns State actions (closures s -> (a, s')).
 * mapM f xs = \s -> fold over xs threading state, collecting results.
 * Returns a State action closure that, when applied to state, threads it. */
static int64_t either_mapM(int64_t f, int64_t xs);  /* forward decl */

static int64_t mapM_state_runner(int64_t clos, int64_t s) {
    int64_t f  = kk_field(clos, 1);
    int64_t xs = kk_field(clos, 2);
    kk_retain(f);
    int64_t *arr = NULL; int64_t cap = 0, n = 0;
    while (!kk_is_nil(xs)) {
        int64_t x = kk_list_head(xs);
        kk_retain(f);
        int64_t action = call1(f, x);
        int64_t pair = call1(action, s);
        int64_t a  = kk_fst(pair);
        s = kk_snd(pair);
        if (n >= cap) { cap = cap ? cap * 2 : 16; arr = realloc(arr, (size_t)cap * sizeof(int64_t)); }
        arr[n++] = a;
        xs = kk_list_tail(xs);
    }
    int64_t result_list = array_to_list(arr, n);
    free(arr);
    return kk_pair(result_list, s);
}

#ifndef KK_EITHER_MONAD_MARKER
#define KK_EITHER_MONAD_MARKER 0xEE17E8LL
#endif
/* mapM$2 receives (monad_dict, f) and returns a closure waiting for xs.
 * For Either monad (dict == KK_EITHER_MONAD_MARKER), uses either_mapM.
 * For other monads (State etc.), returns a State-style closure. */
static int64_t mapM_either_runner(int64_t clos, int64_t xs) {
    int64_t f = kk_field(clos, 1);
    return either_mapM(f, xs);
}
int64_t ghc_traversable_mapM_2(int64_t monad_dict, int64_t f) __asm__("GHC_Internal_Data_Traversable_mapM$2");
int64_t ghc_traversable_mapM_2(int64_t monad_dict, int64_t f) {
    if (monad_dict == KK_EITHER_MONAD_MARKER) {
        return make_closure1(&mapM_either_runner, f);
    }
    /* Non-Either: treat args as (f, xs) for backward compat with State monad.
     * In State monad context, GHC may pass (f, xs) directly. */
    return make_closure2(&mapM_state_runner, monad_dict, f);
}
/* Either monad mapM: traverse list, short-circuit on Left */
#define KK_EITHER_MONAD_MARKER 0xEE17E8LL
/* Either Left tags from different modules */
static int is_either_left(int64_t v) {
    if (!kk_is_heap_ptr(v) || kk_is_string(v)) return 0;
    return kk_tag(v) == 50386;  /* stableConTag "Left" */
}
static int is_either_right(int64_t v) {
    if (!kk_is_heap_ptr(v) || kk_is_string(v)) return 0;
    return kk_tag(v) == 11965;  /* stableConTag "Right" */
}

static int64_t either_mapM(int64_t f, int64_t xs) {
    int64_t *arr = NULL; int64_t cap = 0, n = 0;
    while (!kk_is_nil(xs)) {
        int64_t x = kk_list_head(xs);
        kk_retain(f);
        int64_t r = call1(f, x);
        /* Check for Left (error) */
        if (is_either_left(r)) {
            free(arr);
            return r;  /* propagate Left */
        }
        /* Right: unwrap the value */
        int64_t a = is_either_right(r) ? kk_field(r, 0) : r;
        if (n >= cap) { cap = cap ? cap * 2 : 16; arr = realloc(arr, (size_t)cap * sizeof(int64_t)); }
        arr[n++] = a;
        xs = kk_list_tail(xs);
    }
    int64_t result_list = array_to_list(arr, n);
    free(arr);
    /* Wrap in Right (stableConTag "Right" = 11965) */
    int64_t right = kk_alloc_con(11965, 1);
    kk_set_field(right, 0, result_list);
    return right;
}

int64_t ghc_traversable_mapM_3(int64_t d, int64_t f, int64_t xs) __asm__("GHC_Internal_Data_Traversable_mapM$3");
int64_t ghc_traversable_mapM_3(int64_t d, int64_t f, int64_t xs) {
    (void)d;
    return either_mapM(f, xs);
}

/* ================================================================== */
/*  GHC.Internal.Data.OldList                                           */
/* ================================================================== */

int64_t ghc_oldlist_isPrefixOf_2(int64_t pfx, int64_t xs) __asm__("GHC_Internal_Data_OldList_isPrefixOf$2");
int64_t ghc_oldlist_isPrefixOf_2(int64_t pfx, int64_t xs) {
    while (!kk_is_nil(pfx)) {
        if (kk_is_nil(xs)) return 0;
        int64_t a = kk_list_head(pfx), b = kk_list_head(xs);
        if (kk_is_string(a) && kk_is_string(b)) { if (!kk_str_eq(a, b)) return 0; }
        else if (a != b) return 0;
        pfx = kk_list_tail(pfx); xs = kk_list_tail(xs);
    }
    return 1;
}

int64_t ghc_oldlist_partition_2(int64_t p, int64_t xs) __asm__("GHC_Internal_Data_OldList_partition$2");
int64_t ghc_oldlist_partition_2(int64_t p, int64_t xs) {
    int64_t *yes_arr, *no_arr;
    int64_t yes_n = 0, no_n = 0, cap = 16;
    yes_arr = malloc((size_t)cap * sizeof(int64_t));
    no_arr  = malloc((size_t)cap * sizeof(int64_t));
    while (!kk_is_nil(xs)) {
        int64_t h = kk_list_head(xs);
        if (tobool(call1(p, h))) {
            if (yes_n >= cap) { cap *= 2; yes_arr = realloc(yes_arr, (size_t)cap * sizeof(int64_t)); no_arr = realloc(no_arr, (size_t)cap * sizeof(int64_t)); }
            yes_arr[yes_n++] = h;
        } else {
            if (no_n >= cap) { cap *= 2; yes_arr = realloc(yes_arr, (size_t)cap * sizeof(int64_t)); no_arr = realloc(no_arr, (size_t)cap * sizeof(int64_t)); }
            no_arr[no_n++] = h;
        }
        xs = kk_list_tail(xs);
    }
    int64_t yes_list = array_to_list(yes_arr, yes_n);
    int64_t no_list  = array_to_list(no_arr, no_n);
    int64_t result = kk_pair(yes_list, no_list);
    free(yes_arr); free(no_arr);
    /* Workaround for Perceus lazy-selector issue (see runState shim).
     * GHC compiles let (a, b) = partition ... as two lazy selectors. */
    if (kk_is_heap_ptr(yes_list)) kk_retain(yes_list);
    if (kk_is_heap_ptr(no_list))  kk_retain(no_list);
    return result;
}

/* ================================================================== */
/*  GHC.Internal.Data.Maybe / Either / Functor / Tuple / String         */
/* ================================================================== */

int64_t ghc_maybe_maybe_3(int64_t def, int64_t f, int64_t m) __asm__("GHC_Internal_Data_Maybe_maybe$3");
int64_t ghc_maybe_maybe_3(int64_t def, int64_t f, int64_t m) {
    if (!kk_is_heap_ptr(m) || kk_tag(m) == KK_NOTHING_TAG) return def;
    return call1(f, kk_field(m, 0));
}

int64_t ghc_maybe_mapMaybe_2(int64_t f, int64_t xs) __asm__("GHC_Internal_Data_Maybe_mapMaybe$2");
int64_t ghc_maybe_mapMaybe_2(int64_t f, int64_t xs) {
    int64_t *arr; int64_t cap = 16, n = 0;
    arr = malloc((size_t)cap * sizeof(int64_t));
    while (!kk_is_nil(xs)) {
        int64_t r = call1(f, kk_list_head(xs));
        if (kk_is_heap_ptr(r) && kk_tag(r) == KK_JUST_TAG) {
            if (n >= cap) { cap *= 2; arr = realloc(arr, (size_t)cap * sizeof(int64_t)); }
            arr[n++] = kk_field(r, 0);
        }
        xs = kk_list_tail(xs);
    }
    int64_t result = array_to_list(arr, n);
    free(arr);
    return result;
}

int64_t ghc_maybe_fEqMaybe_0(void) __asm__("GHC_Internal_Maybe_zdfEqMaybe$0");
int64_t ghc_maybe_fEqMaybe_0(void) { return 0; }


int64_t ghc_either_fApplicative_0(void) __asm__("GHC_Internal_Data_Either_zdfApplicativeEither$0");
int64_t ghc_either_fApplicative_0(void) { return KK_EITHER_MONAD_MARKER; }
int64_t ghc_either_fFunctor_0(void) __asm__("GHC_Internal_Data_Either_zdfFunctorEither$0");
int64_t ghc_either_fFunctor_0(void) { return KK_EITHER_MONAD_MARKER; }
int64_t ghc_either_fMonad_0(void) __asm__("GHC_Internal_Data_Either_zdfMonadEither$0");
int64_t ghc_either_fMonad_0(void) { return KK_EITHER_MONAD_MARKER; }

/* <$> (Z-encoded: zlzdzg) = fmap */

/* State monad fmap: fmap f action = \s -> let (a,s') = action s in (f a, s') */
static int64_t fmap_state_runner2(int64_t clos, int64_t s) {
    int64_t f      = kk_field(clos, 1);
    int64_t action = kk_field(clos, 2);
    int64_t result = call1(action, s);
    int64_t a  = kk_fst(result);
    int64_t s2 = kk_snd(result);
    return kk_pair(call1(f, a), s2);
}

/* Polymorphic fmap: dispatches on whether xs is a list or a State monad action */
static int64_t fmap_list(int64_t f, int64_t xs) {
    /* If xs is a closure (State monad action), use State fmap */
    if (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CLOSURE_TAG) {
        return make_closure2(&fmap_state_runner2, f, xs);
    }
    int64_t *arr; int64_t n = list_to_array(xs, &arr);
    for (int64_t i = 0; i < n; i++) arr[i] = call1(f, arr[i]);
    int64_t result = array_to_list(arr, n);
    free(arr);
    return result;
}
/* Either fmap: fmap f (Right x) = Right (f x); fmap f (Left x) = Left x */
#define KK_LEFT_TAG  50386
#define KK_RIGHT_TAG 11965
static int64_t fmap_either(int64_t f, int64_t xs) {
    if (!kk_is_heap_ptr(xs)) return xs;
    int64_t tag = kk_tag(xs);
    if (tag == KK_RIGHT_TAG) {
        int64_t val = kk_field(xs, 0);
        int64_t mapped = call1(f, val);
        int64_t r = kk_alloc_con(KK_RIGHT_TAG, 1);
        kk_set_field(r, 0, mapped);
        return r;
    }
    return xs;  /* Left unchanged */
}
/* Maybe fmap: fmap f (Just x) = Just (f x); fmap f Nothing = Nothing */
#define KK_NOTHING_TAG 53440
#define KK_JUST_TAG    61886
static int64_t fmap_maybe(int64_t f, int64_t xs) {
    if (!kk_is_heap_ptr(xs)) return xs;
    int64_t tag = kk_tag(xs);
    if (tag == KK_JUST_TAG) {
        int64_t val = kk_field(xs, 0);
        int64_t mapped = call1(f, val);
        int64_t r = kk_alloc_con(KK_JUST_TAG, 1);
        kk_set_field(r, 0, mapped);
        return r;
    }
    return xs;  /* Nothing unchanged */
}
int64_t ghc_functor_fmap_2(int64_t f, int64_t xs) __asm__("GHC_Internal_Data_Functor_zlzdzg$2");
int64_t ghc_functor_fmap_2(int64_t f, int64_t xs) { return fmap_list(f, xs); }
int64_t ghc_functor_fmap_3(int64_t d, int64_t f, int64_t xs) __asm__("GHC_Internal_Data_Functor_zlzdzg$3");
int64_t ghc_functor_fmap_3(int64_t d, int64_t f, int64_t xs) {
    if (d == KK_EITHER_MONAD_MARKER) return fmap_either(f, xs);
    /* Heuristic: if xs looks like an Either or Maybe, dispatch accordingly */
    if (kk_is_heap_ptr(xs)) {
        int64_t tag = kk_tag(xs);
        if (tag == KK_LEFT_TAG || tag == KK_RIGHT_TAG) return fmap_either(f, xs);
        if (tag == KK_NOTHING_TAG || tag == KK_JUST_TAG) return fmap_maybe(f, xs);
    }
    return fmap_list(f, xs);
}

int64_t ghc_tuple_fst_0(void) __asm__("GHC_Internal_Data_Tuple_fst$0");
int64_t ghc_tuple_fst_0(void) { return make_closure0(&fst_code); }
static int64_t fst_code(int64_t clos, int64_t p) { (void)clos; return kk_fst(p); }
int64_t ghc_tuple_fst_1(int64_t p) __asm__("GHC_Internal_Data_Tuple_fst$1");
int64_t ghc_tuple_fst_1(int64_t p) { return kk_fst(p); }

int64_t ghc_tuple_snd_0(void) __asm__("GHC_Internal_Data_Tuple_snd$0");
int64_t ghc_tuple_snd_0(void) { return make_closure0(&snd_code); }
static int64_t snd_code(int64_t clos, int64_t p) { (void)clos; return kk_snd(p); }

int64_t ghc_string_fromString_1(int64_t s) __asm__("GHC_Internal_Data_String_fromString$1");
int64_t ghc_string_fromString_1(int64_t s) {
    if (kk_is_string(s)) return s;
    if (kk_is_nil(s)) return kk_string_empty();
    /* Walk [Char] cons-list (raw codepoints) and build a kk_string.
     * Each element is a raw i64 codepoint (not boxed C#). */
    /* First pass: count UTF-8 bytes needed */
    int64_t total = 0;
    int64_t cur = s;
    while (!kk_is_nil(cur) && kk_is_heap_ptr(cur) && kk_tag(cur) == 46589) {
        int64_t cp = kk_field(cur, 0);
        if (cp < 0x80) total += 1;
        else if (cp < 0x800) total += 2;
        else if (cp < 0x10000) total += 3;
        else total += 4;
        cur = kk_field(cur, 1);
    }
    if (total == 0) return kk_string_empty();
    /* Second pass: encode */
    char* buf = (char*)malloc(total + 1);
    int64_t pos = 0;
    cur = s;
    while (!kk_is_nil(cur) && kk_is_heap_ptr(cur) && kk_tag(cur) == 46589) {
        int64_t cp = kk_field(cur, 0);
        if (cp < 0x80) { buf[pos++] = (char)cp; }
        else if (cp < 0x800) {
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
        cur = kk_field(cur, 1);
    }
    buf[pos] = '\0';
    return kk_str_alloc_leaf_owned(buf, pos);
}

/* ================================================================== */
/*  GHC.Internal.Data.IORef / IORef / IO                                */
/* ================================================================== */

int64_t ghc_ioref_new_1(int64_t val) __asm__("GHC_Internal_IORef_newIORef$1");
int64_t ghc_ioref_new_1(int64_t val) { return kk_ref_new(val); }

int64_t ghc_ioref_read_1(int64_t ref) __asm__("GHC_Internal_IORef_readIORef$1");
int64_t ghc_ioref_read_1(int64_t ref) { return kk_ref_get(ref); }

int64_t ghc_ioref_modify_2(int64_t ref, int64_t f) __asm__("GHC_Internal_Data_IORef_modifyIORef$2");
int64_t ghc_ioref_modify_2(int64_t ref, int64_t f) {
    int64_t old = kk_ref_get(ref);
    int64_t new_val = call1(f, old);
    kk_ref_set(ref, new_val);
    return 0;
}

int64_t ghc_sysio_writeFile_2(int64_t path, int64_t content) __asm__("GHC_Internal_System_IO_writeFile$2");
int64_t ghc_sysio_writeFile_2(int64_t path, int64_t content) {
    return kk_write_file(path, content);
}

/* ================================================================== */
/*  GHC.Internal.Unicode                                                */
/* ================================================================== */

/* Chars arrive boxed (C# tag=30786, codepoint in field 0). Unbox before comparing. */
#define CHAR_BOX_TAG_U 30786
static int64_t unbox_char_u(int64_t c) {
    if (kk_is_heap_ptr(c) && kk_tag(c) == CHAR_BOX_TAG_U)
        return kk_field(c, 0);
    return c;
}

static int64_t isDigit_code(int64_t clos, int64_t c) { (void)clos; int64_t v = unbox_char_u(c); return (v >= '0' && v <= '9') ? 1 : 0; }
static int64_t isSpace_code(int64_t clos, int64_t c) {
    (void)clos;
    int64_t v = unbox_char_u(c);
    return (v == ' ' || v == '\t' || v == '\n' || v == '\r' || v == '\f' || v == '\v') ? 1 : 0;
}

int64_t ghc_unicode_isDigit_0(void) __asm__("GHC_Internal_Unicode_isDigit$0");
int64_t ghc_unicode_isDigit_0(void) { return make_closure0(&isDigit_code); }
int64_t ghc_unicode_isDigit_1(int64_t c) __asm__("GHC_Internal_Unicode_isDigit$1");
int64_t ghc_unicode_isDigit_1(int64_t c) { int64_t v = unbox_char_u(c); return (v >= '0' && v <= '9') ? 1 : 0; }
int64_t ghc_unicode_isSpace_0(void) __asm__("GHC_Internal_Unicode_isSpace$0");
int64_t ghc_unicode_isSpace_0(void) { return make_closure0(&isSpace_code); }

/* ================================================================== */
/*  GHC.Internal.Text.Read                                              */
/* ================================================================== */

/* Convert a Haskell String ([Char] cons-list) to a C string.
 * Also works if the argument is already a kk_string. */
static char* haskell_string_to_cstr(int64_t s) {
    if (kk_is_string(s)) {
        return kk_str_dup_cstr(s);
    }
    /* Walk [Char] cons-list, extract codepoints */
    int64_t cap = 256, len = 0;
    char* buf = (char*)malloc((size_t)cap);
    if (!buf) return NULL;
    int64_t cur = s;
    while (kk_is_heap_ptr(cur) && kk_tag(cur) == KK_CONS_TAG) {
        int64_t ch = kk_field(cur, 0);
        int64_t cp = unbox_char_u(ch);
        if (cp > 0 && cp < 128) {
            if (len + 1 >= cap) { cap *= 2; buf = (char*)realloc(buf, (size_t)cap); }
            buf[len++] = (char)cp;
        }
        cur = kk_field(cur, 1);
    }
    buf[len] = '\0';
    return buf;
}

/* Convert result "rest" string back to [Char] cons-list if input was [Char] */
static int64_t cstr_to_haskell_string(const char* cstr) {
    int64_t len = (int64_t)strlen(cstr);
    int64_t result = kk_nil();
    for (int64_t j = len - 1; j >= 0; j--) {
        int64_t boxed = kk_alloc_con(CHAR_BOX_TAG_U, 1);
        kk_set_field(boxed, 0, (int64_t)(unsigned char)cstr[j]);
        result = kk_cons(boxed, result);
    }
    return result;
}

int64_t ghc_read_read_1(int64_t s) __asm__("GHC_Internal_Text_Read_read$1");
int64_t ghc_read_read_1(int64_t s) {
    char *cstr = haskell_string_to_cstr(s);
    int64_t val = strtol(cstr, NULL, 10);
    free(cstr);
    return val;
}

int64_t ghc_read_readMaybe_1(int64_t s) __asm__("GHC_Internal_Text_Read_readMaybe$1");
int64_t ghc_read_readMaybe_1(int64_t s) {
    char *cstr = haskell_string_to_cstr(s);
    char *end;
    int64_t val = strtol(cstr, &end, 10);
    int64_t result = (end != cstr && *end == '\0') ? kk_just(val) : kk_nothing();
    free(cstr);
    return result;
}

/* Make a Haskell String from a C string suffix.
 * If is_charlist, returns [Char] cons-list; otherwise returns kk_string. */
static int64_t make_rest_string(const char* s, int is_charlist) {
    if (is_charlist) {
        return cstr_to_haskell_string(s);
    }
    int64_t len = (int64_t)strlen(s);
    char *buf = (char*)malloc((size_t)len + 1);
    memcpy(buf, s, (size_t)len + 1);
    return kk_str_alloc_leaf_owned(buf, len);
}

int64_t ghc_read_reads_1(int64_t s) __asm__("GHC_Internal_Text_Read_reads$1");
int64_t ghc_read_reads_1(int64_t s) {
    int is_charlist = !kk_is_string(s);
    char *cstr = haskell_string_to_cstr(s);
    char *end;
    /* Try integer parse first */
    int64_t ival = strtol(cstr, &end, 10);
    if (end != cstr && (*end == '\0' || *end == ' ' || *end == ')' || *end == ']')) {
        /* Successful integer parse — check for float indicators */
        char *fend;
        double dval = strtod(cstr, &fend);
        if (fend > end) {
            int64_t rest = make_rest_string(fend, is_charlist);
            union { double d; int64_t i; } u;
            u.d = dval;
            free(cstr);
            return kk_cons(kk_pair(u.i, rest), kk_nil());
        }
        int64_t rest = make_rest_string(end, is_charlist);
        free(cstr);
        return kk_cons(kk_pair(ival, rest), kk_nil());
    }
    /* Try float parse (handles "3.14e2" etc.) */
    double dval = strtod(cstr, &end);
    if (end != cstr) {
        int64_t rest = make_rest_string(end, is_charlist);
        union { double d; int64_t i; } u;
        u.d = dval;
        free(cstr);
        return kk_cons(kk_pair(u.i, rest), kk_nil());
    }
    free(cstr);
    return kk_nil();
}
