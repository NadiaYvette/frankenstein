/* GHC List, Foldable, Traversable, OldList, Maybe, Either, Functor,
 * Tuple, Data.String shims.
 *
 * All values are int64_t.  Lists: cons (tag 1, 2 fields), nil (tag 0).
 * Closures: field 0 = fptr, fields 1..n = captures.
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "../runtime/kk_runtime.h"

typedef int64_t (*fn1_t)(int64_t, int64_t);
typedef int64_t (*fn2_t)(int64_t, int64_t, int64_t);

static int64_t call1(int64_t clos, int64_t a) {
    return ((fn1_t)(intptr_t)kk_field(clos, 0))(clos, a);
}
static int64_t call2(int64_t clos, int64_t a, int64_t b) {
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
    kk_set_field(c, 1, c1);
    return c;
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

/* ++ (2-char → ___) */
static int64_t list_append(int64_t xs, int64_t ys) {
    int64_t *arr; int64_t n = list_to_array(xs, &arr);
    int64_t result = ys;
    for (int64_t i = n - 1; i >= 0; i--) result = kk_cons(arr[i], result);
    free(arr);
    return result;
}

int64_t ghc_list_op_2(int64_t a, int64_t b) __asm__("GHC_Internal_List___$2");
int64_t ghc_list_op_2(int64_t a, int64_t b) { return list_append(a, b); }
int64_t ghc_list_op_3(int64_t d, int64_t a, int64_t b) __asm__("GHC_Internal_List___$3");
int64_t ghc_list_op_3(int64_t d, int64_t a, int64_t b) { (void)d; return list_append(a, b); }

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
        if (call1(p, arr[i])) arr[count++] = arr[i];
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
        if (!call1(p, kk_list_head(xs))) return 0;
        xs = kk_list_tail(xs);
    }
    return 1;
}

int64_t ghc_foldable_any_1(int64_t p) __asm__("GHC_Internal_Data_Foldable_any$1");
int64_t ghc_foldable_any_1(int64_t p) { return make_closure1(&any_apply, p); }
int64_t ghc_foldable_any_2(int64_t p, int64_t xs) __asm__("GHC_Internal_Data_Foldable_any$2");
int64_t ghc_foldable_any_2(int64_t p, int64_t xs) {
    while (!kk_is_nil(xs)) {
        if (call1(p, kk_list_head(xs))) return 1;
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

int64_t ghc_foldable_find_1(int64_t p) __asm__("GHC_Internal_Data_Foldable_find$1");
int64_t ghc_foldable_find_1(int64_t p) { return make_closure1(&find_apply, p); }
int64_t ghc_foldable_find_2(int64_t p, int64_t xs) __asm__("GHC_Internal_Data_Foldable_find$2");
int64_t ghc_foldable_find_2(int64_t p, int64_t xs) {
    while (!kk_is_nil(xs)) {
        int64_t h = kk_list_head(xs);
        if (call1(p, h)) return kk_just(h);
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
    if (kk_is_nil(xs)) return z;
    return call2(f, kk_list_head(xs), ghc_foldable_foldr_3(f, z, kk_list_tail(xs)));
}

int64_t ghc_foldable_forM_1(int64_t xs) __asm__("GHC_Internal_Data_Foldable_forM_$1");
int64_t ghc_foldable_forM_1(int64_t xs) { return make_closure1(&forM_apply, xs); }
static int64_t forM_apply(int64_t clos, int64_t f) {
    int64_t xs = kk_field(clos, 1);
    while (!kk_is_nil(xs)) { call1(f, kk_list_head(xs)); xs = kk_list_tail(xs); }
    return 0;
}

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

int64_t ghc_traversable_mapM_2(int64_t f, int64_t xs) __asm__("GHC_Internal_Data_Traversable_mapM$2");
int64_t ghc_traversable_mapM_2(int64_t f, int64_t xs) {
    int64_t *arr; int64_t n = list_to_array(xs, &arr);
    for (int64_t i = 0; i < n; i++) arr[i] = call1(f, arr[i]);
    int64_t result = array_to_list(arr, n);
    free(arr);
    return result;
}
int64_t ghc_traversable_mapM_3(int64_t d, int64_t f, int64_t xs) __asm__("GHC_Internal_Data_Traversable_mapM$3");
int64_t ghc_traversable_mapM_3(int64_t d, int64_t f, int64_t xs) {
    (void)d; return ghc_traversable_mapM_2(f, xs);
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
        if (call1(p, h)) {
            if (yes_n >= cap) { cap *= 2; yes_arr = realloc(yes_arr, (size_t)cap * sizeof(int64_t)); no_arr = realloc(no_arr, (size_t)cap * sizeof(int64_t)); }
            yes_arr[yes_n++] = h;
        } else {
            if (no_n >= cap) { cap *= 2; yes_arr = realloc(yes_arr, (size_t)cap * sizeof(int64_t)); no_arr = realloc(no_arr, (size_t)cap * sizeof(int64_t)); }
            no_arr[no_n++] = h;
        }
        xs = kk_list_tail(xs);
    }
    int64_t result = kk_pair(array_to_list(yes_arr, yes_n), array_to_list(no_arr, no_n));
    free(yes_arr); free(no_arr);
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

int64_t ghc_maybe_fEqMaybe_0(void) __asm__("GHC_Internal_Maybe__fEqMaybe$0");
int64_t ghc_maybe_fEqMaybe_0(void) { return 0; }

int64_t ghc_either_fApplicative_0(void) __asm__("GHC_Internal_Data_Either__fApplicativeEither$0");
int64_t ghc_either_fApplicative_0(void) { return 0; }
int64_t ghc_either_fFunctor_0(void) __asm__("GHC_Internal_Data_Either__fFunctorEither$0");
int64_t ghc_either_fFunctor_0(void) { return 0; }
int64_t ghc_either_fMonad_0(void) __asm__("GHC_Internal_Data_Either__fMonadEither$0");
int64_t ghc_either_fMonad_0(void) { return 0; }

/* <$> = fmap for lists (3-char → ___) */
int64_t ghc_functor_fmap_2(int64_t f, int64_t xs) __asm__("GHC_Internal_Data_Functor____$2");
int64_t ghc_functor_fmap_2(int64_t f, int64_t xs) {
    int64_t *arr; int64_t n = list_to_array(xs, &arr);
    for (int64_t i = 0; i < n; i++) arr[i] = call1(f, arr[i]);
    int64_t result = array_to_list(arr, n);
    free(arr);
    return result;
}
int64_t ghc_functor_fmap_3(int64_t d, int64_t f, int64_t xs) __asm__("GHC_Internal_Data_Functor____$3");
int64_t ghc_functor_fmap_3(int64_t d, int64_t f, int64_t xs) { (void)d; return ghc_functor_fmap_2(f, xs); }

int64_t ghc_tuple_fst_0(void) __asm__("GHC_Internal_Data_Tuple_fst$0");
int64_t ghc_tuple_fst_0(void) { return make_closure0(&fst_code); }
static int64_t fst_code(int64_t clos, int64_t p) { (void)clos; return kk_fst(p); }
int64_t ghc_tuple_fst_1(int64_t p) __asm__("GHC_Internal_Data_Tuple_fst$1");
int64_t ghc_tuple_fst_1(int64_t p) { return kk_fst(p); }

int64_t ghc_tuple_snd_0(void) __asm__("GHC_Internal_Data_Tuple_snd$0");
int64_t ghc_tuple_snd_0(void) { return make_closure0(&snd_code); }
static int64_t snd_code(int64_t clos, int64_t p) { (void)clos; return kk_snd(p); }

int64_t ghc_string_fromString_1(int64_t s) __asm__("GHC_Internal_Data_String_fromString$1");
int64_t ghc_string_fromString_1(int64_t s) { return s; }

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

static int64_t isDigit_code(int64_t clos, int64_t c) { (void)clos; return (c >= '0' && c <= '9') ? 1 : 0; }
static int64_t isSpace_code(int64_t clos, int64_t c) {
    (void)clos;
    return (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v') ? 1 : 0;
}

int64_t ghc_unicode_isDigit_0(void) __asm__("GHC_Internal_Unicode_isDigit$0");
int64_t ghc_unicode_isDigit_0(void) { return make_closure0(&isDigit_code); }
int64_t ghc_unicode_isDigit_1(int64_t c) __asm__("GHC_Internal_Unicode_isDigit$1");
int64_t ghc_unicode_isDigit_1(int64_t c) { return (c >= '0' && c <= '9') ? 1 : 0; }
int64_t ghc_unicode_isSpace_0(void) __asm__("GHC_Internal_Unicode_isSpace$0");
int64_t ghc_unicode_isSpace_0(void) { return make_closure0(&isSpace_code); }

/* ================================================================== */
/*  GHC.Internal.Text.Read                                              */
/* ================================================================== */

int64_t ghc_read_read_1(int64_t s) __asm__("GHC_Internal_Text_Read_read$1");
int64_t ghc_read_read_1(int64_t s) {
    char *cstr = kk_str_dup_cstr(s);
    int64_t val = strtol(cstr, NULL, 10);
    free(cstr);
    return val;
}

int64_t ghc_read_readMaybe_1(int64_t s) __asm__("GHC_Internal_Text_Read_readMaybe$1");
int64_t ghc_read_readMaybe_1(int64_t s) {
    char *cstr = kk_str_dup_cstr(s);
    char *end;
    int64_t val = strtol(cstr, &end, 10);
    int64_t result = (end != cstr && *end == '\0') ? kk_just(val) : kk_nothing();
    free(cstr);
    return result;
}

int64_t ghc_read_reads_1(int64_t s) __asm__("GHC_Internal_Text_Read_reads$1");
int64_t ghc_read_reads_1(int64_t s) {
    char *cstr = kk_str_dup_cstr(s);
    char *end;
    int64_t val = strtol(cstr, &end, 10);
    if (end == cstr) { free(cstr); return kk_nil(); }
    int64_t rest = kk_str_alloc_leaf_owned(end, (int64_t)strlen(end));
    free(cstr);
    return kk_cons(kk_pair(val, rest), kk_nil());
}
