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

static int64_t set_size(int64_t s) {
    if (set_is_tip(s)) return 0;
    return kk_field(s, 0);
}

static int64_t set_elem(int64_t s)  { return kk_field(s, 1); }
static int64_t set_left(int64_t s)  { return kk_field(s, 2); }
static int64_t set_right(int64_t s) { return kk_field(s, 3); }

static int64_t set_insert(int64_t x, int64_t s) {
    if (set_is_tip(s))
        return set_bin(1, x, set_tip(), set_tip());
    int64_t cmp = kk_compare(x, set_elem(s));
    if (cmp < 0) {
        int64_t nl = set_insert(x, set_left(s));
        return set_bin(set_size(nl) + set_size(set_right(s)) + 1,
                       set_elem(s), nl, set_right(s));
    } else if (cmp > 0) {
        int64_t nr = set_insert(x, set_right(s));
        return set_bin(set_size(set_left(s)) + set_size(nr) + 1,
                       set_elem(s), set_left(s), nr);
    }
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

/* Find minimum element */
static int64_t set_find_min(int64_t s) {
    while (!set_is_tip(set_left(s)))
        s = set_left(s);
    return set_elem(s);
}

/* Delete minimum */
static int64_t set_delete_min(int64_t s) {
    if (set_is_tip(set_left(s)))
        return set_right(s);
    int64_t nl = set_delete_min(set_left(s));
    return set_bin(set_size(nl) + set_size(set_right(s)) + 1,
                   set_elem(s), nl, set_right(s));
}

static int64_t set_delete(int64_t x, int64_t s) {
    if (set_is_tip(s)) return s;
    int64_t cmp = kk_compare(x, set_elem(s));
    if (cmp < 0) {
        int64_t nl = set_delete(x, set_left(s));
        return set_bin(set_size(nl) + set_size(set_right(s)) + 1,
                       set_elem(s), nl, set_right(s));
    } else if (cmp > 0) {
        int64_t nr = set_delete(x, set_right(s));
        return set_bin(set_size(set_left(s)) + set_size(nr) + 1,
                       set_elem(s), set_left(s), nr);
    } else {
        /* Found — delete this node */
        if (set_is_tip(set_left(s))) return set_right(s);
        if (set_is_tip(set_right(s))) return set_left(s);
        int64_t succ = set_find_min(set_right(s));
        int64_t nr = set_delete_min(set_right(s));
        return set_bin(set_size(set_left(s)) + set_size(nr) + 1,
                       succ, set_left(s), nr);
    }
}

/* In-order traversal to sorted list */
static int64_t set_to_list_go(int64_t s, int64_t acc) {
    if (set_is_tip(s)) return acc;
    acc = set_to_list_go(set_right(s), acc);
    acc = kk_cons(set_elem(s), acc);
    acc = set_to_list_go(set_left(s), acc);
    return acc;
}

static int64_t set_to_asc_list(int64_t s) {
    return set_to_list_go(s, kk_nil());
}

static int64_t set_union(int64_t s1, int64_t s2) {
    if (set_is_tip(s1)) return s2;
    if (set_is_tip(s2)) return s1;
    /* Insert all of s2 into s1 */
    int64_t list = set_to_asc_list(s2);
    int64_t result = s1;
    while (!kk_is_nil(list)) {
        result = set_insert(kk_list_head(list), result);
        list = kk_list_tail(list);
    }
    return result;
}

static int64_t set_difference(int64_t s1, int64_t s2) {
    if (set_is_tip(s1) || set_is_tip(s2)) return s1;
    int64_t list = set_to_asc_list(s2);
    int64_t result = s1;
    while (!kk_is_nil(list)) {
        result = set_delete(kk_list_head(list), result);
        list = kk_list_tail(list);
    }
    return result;
}

static int64_t set_from_list(int64_t list) {
    int64_t result = set_tip();
    while (!kk_is_nil(list)) {
        result = set_insert(kk_list_head(list), result);
        list = kk_list_tail(list);
    }
    return result;
}

static int64_t set_unions(int64_t sets) {
    int64_t result = set_tip();
    while (!kk_is_nil(sets)) {
        result = set_union(result, kk_list_head(sets));
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
    return set_insert(x, s);
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
    return set_member(x, s);
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
    return set_is_tip(s) ? 1 : 0;
}

/* union$2(s1, s2) */
int64_t set_union_2(int64_t s1, int64_t s2)
    __asm__("Data_Set_Internal_union$2");
int64_t set_union_2(int64_t s1, int64_t s2) {
    return set_union(s1, s2);
}

/* union$3 — 3-arg variant (first arg is Ord dict, ignored) */
int64_t set_union_3(int64_t _dict, int64_t s1, int64_t s2)
    __asm__("Data_Set_Internal_union$3");
int64_t set_union_3(int64_t _dict, int64_t s1, int64_t s2) {
    (void)_dict;
    return set_union(s1, s2);
}

/* difference$2(s1, s2) */
int64_t set_difference_2(int64_t s1, int64_t s2)
    __asm__("Data_Set_Internal_difference$2");
int64_t set_difference_2(int64_t s1, int64_t s2) {
    return set_difference(s1, s2);
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
    return set_to_asc_list(s);
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
    return set_to_asc_list(s);
}

/* unions$1([set]) */
int64_t set_unions_1(int64_t sets)
    __asm__("Data_Set_Internal_unions$1");
int64_t set_unions_1(int64_t sets) {
    return set_unions(sets);
}
