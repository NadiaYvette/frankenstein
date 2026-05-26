/* Data.Set shims for Frankenstein self-hosted binary.
 *
 * Implements Data.Set as an unbalanced BST using the kk_runtime.
 * Set = Tip | Bin(size, elem, left, right)
 *   - tag 0, 0 fields = Tip (empty)
 *   - tag 1, 4 fields = Bin
 *
 * Required symbols (from nm -u):
 *   Data_Set_Internal_delete$0
 *   Data_Set_Internal_difference$2
 *   Data_Set_Internal_empty$0
 *   Data_Set_Internal_fromList$0
 *   Data_Set_Internal_fromList$1
 *   Data_Set_Internal_insert$0
 *   Data_Set_Internal_insert$2
 *   Data_Set_Internal_member$2
 *   Data_Set_Internal_notMember$0
 *   Data_Set_Internal_null$1
 *   Data_Set_Internal_singleton$1
 *   Data_Set_Internal_toAscList$1
 *   Data_Set_Internal_toList$0
 *   Data_Set_Internal_toList$1
 *   Data_Set_Internal_union$2
 *   Data_Set_Internal_union$3
 *   Data_Set_Internal_unions$1
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "../runtime/kk_runtime.h"

/* --- Set representation using BST nodes --- */

#define SET_TIP_TAG 0
#define SET_BIN_TAG 1
/* Bin fields: [0]=size, [1]=elem, [2]=left, [3]=right */

static int64_t set_tip(void) {
    return kk_alloc_con(SET_TIP_TAG, 0);
}

static int64_t set_bin(int64_t sz, int64_t elem,
                        int64_t left, int64_t right) {
    int64_t n = kk_alloc_con(SET_BIN_TAG, 4);
    kk_set_field(n, 0, sz);
    kk_set_field(n, 1, elem);
    kk_set_field(n, 2, left);
    kk_set_field(n, 3, right);
    return n;
}

static int set_is_tip(int64_t s) {
    return !kk_is_heap_ptr(s) || kk_tag(s) == SET_TIP_TAG;
}

/* Force a Set argument that may be a LAZY thunk wrapping the real BST.
 * Frankenstein's CAF emitter (e.g. `externalRuntimeFns`) returns
 * `kk_thunk_create(...)` for CAF results; downstream Set.union /
 * Set.toList walk the cell as a BST without forcing it first,
 * reading the thunk's closure/eval-flag fields as "tree" pointers
 * and emitting garbage elements.  Force any LAZY-tagged Set arg here
 * so set ops always see the real BST. */
#define KK_THUNK_TAG_S 0x4C415A59LL
extern int64_t kk_thunk_force(int64_t);
static inline int64_t set_force(int64_t s) {
    if (!kk_is_heap_ptr(s)) return s;
    if (kk_tag(s) == KK_THUNK_TAG_S) return kk_thunk_force(s);
    return s;
}

static int64_t set_size(int64_t s) {
    if (set_is_tip(s)) return 0;
    return kk_field(s, 0);
}

static int64_t set_elem(int64_t s)  { return kk_field(s, 1); }
static int64_t set_left(int64_t s)  { return kk_field(s, 2); }
static int64_t set_right(int64_t s) { return kk_field(s, 3); }

/* Weight-balanced tree balancing (matches GHC's Data.Set).
 * delta = 3, ratio = 2. */
#define SET_WB_DELTA 3
#define SET_WB_RATIO 2

static int64_t set_balanceL(int64_t e, int64_t l, int64_t r) {
    int64_t rs = set_size(r);
    int64_t ls = set_size(l);
    if (ls + rs <= 1)
        return set_bin(ls + rs + 1, e, l, r);
    if (ls > SET_WB_DELTA * rs + 1) {
        int64_t ll = set_left(l);  kk_retain(ll);
        int64_t lr = set_right(l); kk_retain(lr);
        int64_t le = set_elem(l);  kk_retain(le);
        if (set_size(lr) < SET_WB_RATIO * set_size(ll)) {
            int64_t newr = set_bin(set_size(lr) + rs + 1, e, lr, r);
            return set_bin(set_size(ll) + set_size(newr) + 1, le, ll, newr);
        } else {
            int64_t lrl = set_left(lr);  kk_retain(lrl);
            int64_t lrr = set_right(lr); kk_retain(lrr);
            int64_t lre = set_elem(lr);  kk_retain(lre);
            int64_t newl = set_bin(set_size(ll) + set_size(lrl) + 1, le, ll, lrl);
            int64_t newr = set_bin(rs + set_size(lrr) + 1, e, lrr, r);
            return set_bin(set_size(newl) + set_size(newr) + 1, lre, newl, newr);
        }
    }
    return set_bin(ls + rs + 1, e, l, r);
}

static int64_t set_balanceR(int64_t e, int64_t l, int64_t r) {
    int64_t ls = set_size(l);
    int64_t rs = set_size(r);
    if (ls + rs <= 1)
        return set_bin(ls + rs + 1, e, l, r);
    if (rs > SET_WB_DELTA * ls + 1) {
        int64_t rl = set_left(r);  kk_retain(rl);
        int64_t rr = set_right(r); kk_retain(rr);
        int64_t re = set_elem(r);  kk_retain(re);
        if (set_size(rl) < SET_WB_RATIO * set_size(rr)) {
            int64_t newl = set_bin(ls + set_size(rl) + 1, e, l, rl);
            return set_bin(set_size(newl) + set_size(rr) + 1, re, newl, rr);
        } else {
            int64_t rll = set_left(rl);  kk_retain(rll);
            int64_t rlr = set_right(rl); kk_retain(rlr);
            int64_t rle = set_elem(rl);  kk_retain(rle);
            int64_t newl = set_bin(ls + set_size(rll) + 1, e, l, rll);
            int64_t newr = set_bin(set_size(rlr) + set_size(rr) + 1, re, rlr, rr);
            return set_bin(set_size(newl) + set_size(newr) + 1, rle, newl, newr);
        }
    }
    return set_bin(ls + rs + 1, e, l, r);
}

/* Balanced BST insert using weight-balanced tree rotations.
 * Retain shared fields from old node when creating new nodes. */
static int64_t set_insert(int64_t x, int64_t s) {
    if (getenv("KK_SET_INSERT_TRACE")) {
        int64_t magic = (kk_is_heap_ptr(x)) ? *(int64_t*)x : 0;
        if (magic != 0x4B4B535452494E47LL) {
            int64_t f0 = (kk_is_heap_ptr(x)) ? *(int64_t*)(x+8) : 0;
            int64_t f1 = (kk_is_heap_ptr(x)) ? *(int64_t*)(x+16) : 0;
            int64_t nf = (kk_is_heap_ptr(x)) ? kk_nfields(x) : 0;
            fprintf(stderr, "[set_insert] non-Text x=%p magic=%#lx nf=%ld f0=%#lx f1=%#lx\n",
                    (void*)x, (long)magic, (long)nf, (long)f0, (long)f1);
        }
    }
    if (set_is_tip(s))
        return set_bin(1, x, set_tip(), set_tip());
    int64_t cmp = kk_compare(x, set_elem(s));
    if (cmp < 0) {
        int64_t nl = set_insert(x, set_left(s));
        int64_t oe = set_elem(s);  kk_retain(oe);
        int64_t or_ = set_right(s); kk_retain(or_);
        return set_balanceL(oe, nl, or_);
    } else if (cmp > 0) {
        int64_t nr = set_insert(x, set_right(s));
        int64_t oe = set_elem(s);  kk_retain(oe);
        int64_t ol = set_left(s);  kk_retain(ol);
        return set_balanceR(oe, ol, nr);
    }
    kk_retain(s);
    return s; /* already present */
}

static int64_t set_member(int64_t x, int64_t s) {
    while (!set_is_tip(s)) {
        int64_t cmp = kk_compare(x, set_elem(s));
        if (cmp < 0) s = set_left(s);
        else if (cmp > 0) s = set_right(s);
        else return 1;
    }
    return 0;
}

static int64_t set_delete(int64_t x, int64_t s);

/* Find minimum element — caller is responsible for retaining the result */
static int64_t set_find_min(int64_t s) {
    while (!set_is_tip(set_left(s)))
        s = set_left(s);
    return set_elem(s);
}
/* Note: set_find_min returns a borrowed ref — callers (set_delete) already retain it */

/* Delete minimum — retain shared fields */
static int64_t set_delete_min(int64_t s) {
    if (set_is_tip(set_left(s))) {
        int64_t r = set_right(s); kk_retain(r);
        return r;
    }
    int64_t nl = set_delete_min(set_left(s));
    int64_t oe = set_elem(s);   kk_retain(oe);
    int64_t or_ = set_right(s); kk_retain(or_);
    return set_bin(set_size(nl) + set_size(or_) + 1, oe, nl, or_);
}

static int64_t set_delete(int64_t x, int64_t s) {
    if (set_is_tip(s)) return s;
    int64_t cmp = kk_compare(x, set_elem(s));
    if (cmp < 0) {
        int64_t nl = set_delete(x, set_left(s));
        int64_t oe = set_elem(s);   kk_retain(oe);
        int64_t or_ = set_right(s); kk_retain(or_);
        return set_bin(set_size(nl) + set_size(or_) + 1, oe, nl, or_);
    } else if (cmp > 0) {
        int64_t nr = set_delete(x, set_right(s));
        int64_t oe = set_elem(s);  kk_retain(oe);
        int64_t ol = set_left(s);  kk_retain(ol);
        return set_bin(set_size(ol) + set_size(nr) + 1, oe, ol, nr);
    } else {
        /* Found — delete this node */
        if (set_is_tip(set_left(s))) {
            int64_t r = set_right(s); kk_retain(r); return r;
        }
        if (set_is_tip(set_right(s))) {
            int64_t l = set_left(s); kk_retain(l); return l;
        }
        int64_t succ = set_find_min(set_right(s)); kk_retain(succ);
        int64_t nr = set_delete_min(set_right(s));
        int64_t ol = set_left(s); kk_retain(ol);
        return set_bin(set_size(ol) + set_size(nr) + 1, succ, ol, nr);
    }
}

/* In-order traversal to sorted list — retain elements shared from set nodes */
static int64_t set_to_list_go(int64_t s, int64_t acc) {
    if (set_is_tip(s)) return acc;
    acc = set_to_list_go(set_right(s), acc);
    int64_t e = set_elem(s); kk_retain(e);
    if (getenv("KK_SET_TOLIST_TRACE")) {
        int64_t magic = (kk_is_heap_ptr(e)) ? *(int64_t*)e : 0;
        fprintf(stderr, "[set_to_list] elem=%p magic=%#lx\n", (void*)e, (long)magic);
    }
    acc = kk_cons(e, acc);
    acc = set_to_list_go(set_left(s), acc);
    return acc;
}

static int64_t set_to_asc_list(int64_t s) {
    return set_to_list_go(s, kk_nil());
}

static int64_t set_union(int64_t s1, int64_t s2) {
    if (set_is_tip(s1)) { kk_retain(s2); return s2; }
    if (set_is_tip(s2)) { kk_retain(s1); return s1; }
    /* Insert all of s2 into s1 */
    int64_t list = set_to_asc_list(s2);
    kk_retain(s1);
    int64_t result = s1;
    while (!kk_is_nil(list)) {
        int64_t h = kk_list_head(list); kk_retain(h);
        result = set_insert(h, result);
        list = kk_list_tail(list);
    }
    return result;
}

static int64_t set_difference(int64_t s1, int64_t s2) {
    if (set_is_tip(s1) || set_is_tip(s2)) { kk_retain(s1); return s1; }
    int64_t list = set_to_asc_list(s2);
    kk_retain(s1);
    int64_t result = s1;
    while (!kk_is_nil(list)) {
        int64_t h = kk_list_head(list); kk_retain(h);
        result = set_delete(h, result);
        list = kk_list_tail(list);
    }
    return result;
}

static int64_t set_from_list(int64_t list) {
    int64_t result = set_tip();
    while (!kk_is_nil(list)) {
        int64_t h = kk_list_head(list); kk_retain(h);
        result = set_insert(h, result);
        list = kk_list_tail(list);
    }
    return result;
}

static int64_t set_unions(int64_t sets) {
    int64_t result = set_tip();
    while (!kk_is_nil(sets)) {
        int64_t h = kk_list_head(sets); kk_retain(h);
        result = set_union(result, h);
        sets = kk_list_tail(sets);
    }
    return result;
}

/* =================================================================== */
/*  Exported symbols with asm labels                                   */
/* =================================================================== */

/* empty$0 — function reference returning empty set */
int64_t set_empty_0(void) __asm__("Data_Set_Internal_empty$0");
int64_t set_empty_0(void) { return set_tip(); }

/* singleton$1(elem) */
int64_t set_singleton_1(int64_t x)
    __asm__("Data_Set_Internal_singleton$1");
int64_t set_singleton_1(int64_t x) {
    return set_bin(1, x, set_tip(), set_tip());
}

/* insert$2(elem, set) */
int64_t set_insert_2(int64_t x, int64_t s)
    __asm__("Data_Set_Internal_insert$2");
int64_t set_insert_2(int64_t x, int64_t s) {
    return set_insert(x, set_force(s));
}

/* Closure trampolines for set operations */
#define CLOS_TAG_S 0x434C4F53
static int64_t tram_set_insert(int64_t clos, int64_t x, int64_t s) { (void)clos; return set_insert(x, s); }
static int64_t tram_set_member(int64_t clos, int64_t x, int64_t s) { (void)clos; return set_member(x, s); }
static int64_t tram_set_delete(int64_t clos, int64_t x, int64_t s) { (void)clos; return set_delete(x, s); }
static int64_t tram_set_fromList(int64_t clos, int64_t l) { (void)clos; return set_from_list(l); }
static int64_t tram_set_toList(int64_t clos, int64_t s)   { (void)clos; return set_to_asc_list(s); }

/* insert$0 — function reference */
int64_t set_insert_0(void) __asm__("Data_Set_Internal_insert$0");
int64_t set_insert_0(void) {
    int64_t c = kk_alloc_con(CLOS_TAG_S, 1);
    kk_set_field(c, 0, (int64_t)&tram_set_insert);
    return c;
}

/* member$2(elem, set) */
int64_t set_member_2(int64_t x, int64_t s)
    __asm__("Data_Set_Internal_member$2");
int64_t set_member_2(int64_t x, int64_t s) {
    return set_member(x, set_force(s));
}

/* notMember$0 — function reference */
int64_t set_notMember_0(void) __asm__("Data_Set_Internal_notMember$0");
int64_t set_notMember_0(void) {
    int64_t c = kk_alloc_con(CLOS_TAG_S, 1);
    kk_set_field(c, 0, (int64_t)&tram_set_member); /* caller negates */
    return c;
}

/* delete$0 — function reference */
int64_t set_delete_0(void) __asm__("Data_Set_Internal_delete$0");
int64_t set_delete_0(void) {
    int64_t c = kk_alloc_con(CLOS_TAG_S, 1);
    kk_set_field(c, 0, (int64_t)&tram_set_delete);
    return c;
}

/* null$1(set) */
int64_t set_null_1(int64_t s)
    __asm__("Data_Set_Internal_null$1");
int64_t set_null_1(int64_t s) {
    return set_is_tip(set_force(s)) ? 1 : 0;
}

/* union$2(s1, s2) */
int64_t set_union_2(int64_t s1, int64_t s2)
    __asm__("Data_Set_Internal_union$2");
int64_t set_union_2(int64_t s1, int64_t s2) {
    return set_union(set_force(s1), set_force(s2));
}

/* union$3 — 3-arg variant (first arg is Ord dict, ignored) */
int64_t set_union_3(int64_t _dict, int64_t s1, int64_t s2)
    __asm__("Data_Set_Internal_union$3");
int64_t set_union_3(int64_t _dict, int64_t s1, int64_t s2) {
    (void)_dict;
    return set_union(set_force(s1), set_force(s2));
}

/* difference$2(s1, s2) */
int64_t set_difference_2(int64_t s1, int64_t s2)
    __asm__("Data_Set_Internal_difference$2");
int64_t set_difference_2(int64_t s1, int64_t s2) {
    return set_difference(set_force(s1), set_force(s2));
}

/* fromList$1(list) */
int64_t set_fromList_1(int64_t list)
    __asm__("Data_Set_Internal_fromList$1");
int64_t set_fromList_1(int64_t list) {
    return set_from_list(list);
}

/* fromList$0 — function reference */
int64_t set_fromList_0(void) __asm__("Data_Set_Internal_fromList$0");
int64_t set_fromList_0(void) {
    int64_t c = kk_alloc_con(CLOS_TAG_S, 1);
    kk_set_field(c, 0, (int64_t)&tram_set_fromList);
    return c;
}

/* toList$1(set) */
int64_t set_toList_1(int64_t s)
    __asm__("Data_Set_Internal_toList$1");
int64_t set_toList_1(int64_t s) {
    return set_to_asc_list(set_force(s));
}

/* toList$0 — function reference */
int64_t set_toList_0(void) __asm__("Data_Set_Internal_toList$0");
int64_t set_toList_0(void) {
    int64_t c = kk_alloc_con(CLOS_TAG_S, 1);
    kk_set_field(c, 0, (int64_t)&tram_set_toList);
    return c;
}

/* toAscList$1(set) */
int64_t set_toAscList_1(int64_t s)
    __asm__("Data_Set_Internal_toAscList$1");
int64_t set_toAscList_1(int64_t s) {
    return set_to_asc_list(set_force(s));
}

/* unions$1([set]) */
int64_t set_unions_1(int64_t sets)
    __asm__("Data_Set_Internal_unions$1");
int64_t set_unions_1(int64_t sets) {
    return set_unions(sets);
}
