/* Data.Map shims for Frankenstein self-hosted binary.
 *
 * Implements Data.Map as a sorted association list using the kk_runtime.
 * Map = Nil | Node(key, value, left, right, size)
 *   - tag 0, 0 fields = empty (Tip)
 *   - tag 1, 5 fields = Bin(size, key, value, left, right)
 *
 * Comparison uses kk_compare (structural/string ordering).
 *
 * Required symbols (from nm -u):
 *   Data_Map_Internal_elems$1
 *   Data_Map_Internal_empty$0
 *   Data_Map_Internal_findWithDefault$3
 *   Data_Map_Internal_lookup$2
 *   Data_Map_Internal_member$2
 *   Data_Map_Internal_toList$1
 *   Data_Map_Internal_union$2
 *   Data_Map_Strict_Internal_fromList$1
 *   Data_Map_Strict_Internal_fromListWith$2
 *   Data_Map_Strict_Internal_insert$3
 *   Data_Map_Strict_Internal_singleton$2
 *   Data_Map_Strict_Internal_unionsWith$2
 *   Data_Map_Strict_Internal_unionWith$3
 */

#include <stdint.h>
#include "../runtime/kk_runtime.h"

/* --- Map representation using BST nodes --- */

#define MAP_TIP_TAG  0
#define MAP_BIN_TAG  1
/* Bin fields: [0]=size, [1]=key, [2]=value, [3]=left, [4]=right */

static int64_t map_tip(void) {
    return kk_alloc_con(MAP_TIP_TAG, 0);
}

static int64_t map_bin(int64_t sz, int64_t key, int64_t val,
                        int64_t left, int64_t right) {
    int64_t n = kk_alloc_con(MAP_BIN_TAG, 5);
    kk_set_field(n, 0, sz);
    kk_set_field(n, 1, key);
    kk_set_field(n, 2, val);
    kk_set_field(n, 3, left);
    kk_set_field(n, 4, right);
    return n;
}

static int map_is_tip(int64_t m) {
    return !kk_is_heap_ptr(m) || kk_tag(m) == MAP_TIP_TAG;
}

static int64_t map_size(int64_t m) {
    if (map_is_tip(m)) return 0;
    return kk_field(m, 0);
}

static int64_t map_key(int64_t m)   { return kk_field(m, 1); }
static int64_t map_val(int64_t m)   { return kk_field(m, 2); }
static int64_t map_left(int64_t m)  { return kk_field(m, 3); }
static int64_t map_right(int64_t m) { return kk_field(m, 4); }

/* Simple unbalanced BST insert (sufficient for self-hosting demo) */
static int64_t map_insert(int64_t k, int64_t v, int64_t m) {
    if (map_is_tip(m))
        return map_bin(1, k, v, map_tip(), map_tip());
    int64_t cmp = kk_compare(k, map_key(m));
    if (cmp < 0) {
        int64_t new_left = map_insert(k, v, map_left(m));
        return map_bin(map_size(new_left) + map_size(map_right(m)) + 1,
                       map_key(m), map_val(m), new_left, map_right(m));
    } else if (cmp > 0) {
        int64_t new_right = map_insert(k, v, map_right(m));
        return map_bin(map_size(map_left(m)) + map_size(new_right) + 1,
                       map_key(m), map_val(m), map_left(m), new_right);
    } else {
        /* Key exists: update value (strict) */
        return map_bin(map_size(m), k, v, map_left(m), map_right(m));
    }
}

/* Lookup: returns Maybe (Nothing = tag 0; Just x = tag 1, 1 field) */
static int64_t map_lookup(int64_t k, int64_t m) {
    while (!map_is_tip(m)) {
        int64_t cmp = kk_compare(k, map_key(m));
        if (cmp < 0) m = map_left(m);
        else if (cmp > 0) m = map_right(m);
        else return kk_just(map_val(m));
    }
    return kk_nothing();
}

static int64_t map_member(int64_t k, int64_t m) {
    while (!map_is_tip(m)) {
        int64_t cmp = kk_compare(k, map_key(m));
        if (cmp < 0) m = map_left(m);
        else if (cmp > 0) m = map_right(m);
        else return 1;
    }
    return 0;
}

static int64_t map_find_with_default(int64_t def, int64_t k, int64_t m) {
    while (!map_is_tip(m)) {
        int64_t cmp = kk_compare(k, map_key(m));
        if (cmp < 0) m = map_left(m);
        else if (cmp > 0) m = map_right(m);
        else return map_val(m);
    }
    return def;
}

/* In-order traversal to list of (k,v) pairs */
static int64_t map_to_list_go(int64_t m, int64_t acc) {
    if (map_is_tip(m)) return acc;
    acc = map_to_list_go(map_right(m), acc);
    int64_t pair = kk_pair(map_key(m), map_val(m));
    acc = kk_cons(pair, acc);
    acc = map_to_list_go(map_left(m), acc);
    return acc;
}

static int64_t map_to_list(int64_t m) {
    return map_to_list_go(m, kk_nil());
}

/* In-order values only */
static int64_t map_elems_go(int64_t m, int64_t acc) {
    if (map_is_tip(m)) return acc;
    acc = map_elems_go(map_right(m), acc);
    acc = kk_cons(map_val(m), acc);
    acc = map_elems_go(map_left(m), acc);
    return acc;
}

static int64_t map_elems(int64_t m) {
    return map_elems_go(m, kk_nil());
}

/* Union: left-biased */
static int64_t map_union(int64_t m1, int64_t m2) {
    if (map_is_tip(m1)) return m2;
    if (map_is_tip(m2)) return m1;
    /* Insert all of m2 into m1 (simple but O(n*m)) */
    int64_t result = m1;
    int64_t list = map_to_list(m2);
    while (!kk_is_nil(list)) {
        int64_t pair = kk_list_head(list);
        int64_t k = kk_fst(pair), v = kk_snd(pair);
        /* Left-biased: don't overwrite existing keys in m1 */
        if (!map_member(k, result))
            result = map_insert(k, v, result);
        list = kk_list_tail(list);
    }
    return result;
}

/* Union with combining function */
static int64_t map_union_with(int64_t f, int64_t m1, int64_t m2) {
    if (map_is_tip(m1)) return m2;
    if (map_is_tip(m2)) return m1;
    int64_t result = m1;
    int64_t list = map_to_list(m2);
    while (!kk_is_nil(list)) {
        int64_t pair = kk_list_head(list);
        int64_t k = kk_fst(pair), v2 = kk_snd(pair);
        int64_t existing = map_lookup(k, result);
        if (kk_tag(existing) == KK_JUST_TAG) {
            /* Combine: f(existing_val, v2) */
            int64_t v1 = kk_field(existing, 0);
            /* f is a closure: field 0 = fn ptr, field 1+ = captures */
            int64_t fn_ptr = kk_field(f, 0);
            typedef int64_t (*fn2_t)(int64_t, int64_t, int64_t);
            int64_t combined = ((fn2_t)fn_ptr)(f, v1, v2);
            result = map_insert(k, combined, result);
        } else {
            result = map_insert(k, v2, result);
        }
        list = kk_list_tail(list);
    }
    return result;
}

/* fromList: insert each pair */
static int64_t map_from_list(int64_t list) {
    int64_t result = map_tip();
    while (!kk_is_nil(list)) {
        int64_t pair = kk_list_head(list);
        result = map_insert(kk_fst(pair), kk_snd(pair), result);
        list = kk_list_tail(list);
    }
    return result;
}

/* fromListWith: insert with combining function */
static int64_t map_from_list_with(int64_t f, int64_t list) {
    int64_t result = map_tip();
    while (!kk_is_nil(list)) {
        int64_t pair = kk_list_head(list);
        int64_t k = kk_fst(pair), v = kk_snd(pair);
        int64_t existing = map_lookup(k, result);
        if (kk_tag(existing) == KK_JUST_TAG) {
            int64_t v1 = kk_field(existing, 0);
            int64_t fn_ptr = kk_field(f, 0);
            typedef int64_t (*fn2_t)(int64_t, int64_t, int64_t);
            int64_t combined = ((fn2_t)fn_ptr)(f, v1, v);
            result = map_insert(k, combined, result);
        } else {
            result = map_insert(k, v, result);
        }
        list = kk_list_tail(list);
    }
    return result;
}

/* unionsWith: fold unionWith over a list of maps */
static int64_t map_unions_with(int64_t f, int64_t maps) {
    int64_t result = map_tip();
    while (!kk_is_nil(maps)) {
        result = map_union_with(f, result, kk_list_head(maps));
        maps = kk_list_tail(maps);
    }
    return result;
}

/* =================================================================== */
/*  Exported symbols with asm labels                                   */
/* =================================================================== */

/* Data_Map_Internal_empty$0 — returns empty map (function reference) */
int64_t map_empty_0(void) __asm__("Data_Map_Internal_empty$0");
int64_t map_empty_0(void) { return map_tip(); }

/* Data_Map_Strict_Internal_singleton$2(key, value) */
int64_t map_singleton_2(int64_t k, int64_t v)
    __asm__("Data_Map_Strict_Internal_singleton$2");
int64_t map_singleton_2(int64_t k, int64_t v) {
    return map_bin(1, k, v, map_tip(), map_tip());
}

/* Data_Map_Strict_Internal_insert$3(key, value, map) */
int64_t map_insert_3(int64_t k, int64_t v, int64_t m)
    __asm__("Data_Map_Strict_Internal_insert$3");
int64_t map_insert_3(int64_t k, int64_t v, int64_t m) {
    return map_insert(k, v, m);
}

/* Data_Map_Internal_lookup$2(key, map) */
int64_t map_lookup_2(int64_t k, int64_t m)
    __asm__("Data_Map_Internal_lookup$2");
int64_t map_lookup_2(int64_t k, int64_t m) {
    return map_lookup(k, m);
}

/* Data_Map_Internal_member$2(key, map) */
int64_t map_member_2(int64_t k, int64_t m)
    __asm__("Data_Map_Internal_member$2");
int64_t map_member_2(int64_t k, int64_t m) {
    return map_member(k, m);
}

/* Data_Map_Internal_findWithDefault$2(default, key) — partial */
int64_t map_fwd_2(int64_t def, int64_t k)
    __asm__("Data_Map_Internal_findWithDefault$2");
int64_t map_fwd_2(int64_t def, int64_t k) {
    (void)def; (void)k;
    /* Partial application not commonly needed — return 0 as stub */
    return 0;
}

/* Data_Map_Internal_findWithDefault$3(default, key, map) */
int64_t map_fwd_3(int64_t def, int64_t k, int64_t m)
    __asm__("Data_Map_Internal_findWithDefault$3");
int64_t map_fwd_3(int64_t def, int64_t k, int64_t m) {
    return map_find_with_default(def, k, m);
}

/* Data_Map_Internal_union$2(map1, map2) */
int64_t map_union_2(int64_t m1, int64_t m2)
    __asm__("Data_Map_Internal_union$2");
int64_t map_union_2(int64_t m1, int64_t m2) {
    return map_union(m1, m2);
}

/* Data_Map_Strict_Internal_unionWith$3(f, map1, map2) */
int64_t map_unionWith_3(int64_t f, int64_t m1, int64_t m2)
    __asm__("Data_Map_Strict_Internal_unionWith$3");
int64_t map_unionWith_3(int64_t f, int64_t m1, int64_t m2) {
    return map_union_with(f, m1, m2);
}

/* Data_Map_Strict_Internal_unionsWith$2(f, [map]) */
int64_t map_unionsWith_2(int64_t f, int64_t maps)
    __asm__("Data_Map_Strict_Internal_unionsWith$2");
int64_t map_unionsWith_2(int64_t f, int64_t maps) {
    return map_unions_with(f, maps);
}

/* Data_Map_Strict_Internal_fromList$1([(k,v)]) */
int64_t map_fromList_1(int64_t list)
    __asm__("Data_Map_Strict_Internal_fromList$1");
int64_t map_fromList_1(int64_t list) {
    return map_from_list(list);
}

/* Data_Map_Strict_Internal_fromListWith$2(f, [(k,v)]) */
int64_t map_fromListWith_2(int64_t f, int64_t list)
    __asm__("Data_Map_Strict_Internal_fromListWith$2");
int64_t map_fromListWith_2(int64_t f, int64_t list) {
    return map_from_list_with(f, list);
}

/* Data_Map_Internal_elems$1(map) */
int64_t map_elems_1(int64_t m)
    __asm__("Data_Map_Internal_elems$1");
int64_t map_elems_1(int64_t m) {
    return map_elems(m);
}

/* Data_Map_Internal_toList$1(map) */
int64_t map_toList_1(int64_t m)
    __asm__("Data_Map_Internal_toList$1");
int64_t map_toList_1(int64_t m) {
    return map_to_list(m);
}

/* Data_Map_Internal__fEqMap$0 — Eq dictionary stub */
int64_t map_fEqMap_0(void)
    __asm__("Data_Map_Internal__fEqMap$0");
int64_t map_fEqMap_0(void) { return 0; }
