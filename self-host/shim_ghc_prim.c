/* GHC primitive operations shims — Base, Classes, Num, Real, Enum, Show,
 * Err, Stack, String, State monad.
 *
 * Convention: all values are int64_t.  Closures have field 0 = fptr,
 * fields 1..n = captures.  Booleans: 0 = False, nonzero = True.
 * Strings: kk_string ropes.  Lists: cons (tag 1, 2 fields), nil (tag 0).
 *
 * Operator mangling: sanitizeName maps +*-/=<>!@#$%^&|~.,()[]{}'"\ to _.
 * This causes collisions (++ == >> == <> → ___).  We handle the most
 * common semantics for each collision.
 */

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../runtime/kk_runtime.h"

/* ------------------------------------------------------------------ */
/*  Closure calling helpers                                             */
/* ------------------------------------------------------------------ */

typedef int64_t (*fn0_t)(int64_t);
typedef int64_t (*fn1_t)(int64_t, int64_t);
typedef int64_t (*fn2_t)(int64_t, int64_t, int64_t);

static int64_t call0(int64_t clos) {
    int64_t fp = kk_field(clos, 0);
    return ((fn0_t)(intptr_t)fp)(clos);
}

static int64_t call1(int64_t clos, int64_t a) {
    int64_t fp = kk_field(clos, 0);
    return ((fn1_t)(intptr_t)fp)(clos, a);
}

static int64_t call2(int64_t clos, int64_t a, int64_t b) {
    int64_t fp = kk_field(clos, 0);
    return ((fn2_t)(intptr_t)fp)(clos, a, b);
}

#define CLOS_TAG 0x434C4F53 /* 'CLOS' */

static int64_t make_closure0(void* fptr) {
    int64_t c = kk_alloc_con(CLOS_TAG, 1);
    kk_set_field(c, 0, (int64_t)(intptr_t)fptr);
    return c;
}

static int64_t make_closure1(void* fptr, int64_t cap1) {
    int64_t c = kk_alloc_con(CLOS_TAG, 2);
    kk_set_field(c, 0, (int64_t)(intptr_t)fptr);
    kk_set_field(c, 1, cap1);
    return c;
}

static int64_t make_closure2(void* fptr, int64_t cap1, int64_t cap2) {
    int64_t c = kk_alloc_con(CLOS_TAG, 3);
    kk_set_field(c, 0, (int64_t)(intptr_t)fptr);
    kk_set_field(c, 1, cap1);
    kk_set_field(c, 2, cap2);
    return c;
}

/* ------------------------------------------------------------------ */
/*  Forward declarations for closure code pointers                      */
/* ------------------------------------------------------------------ */

static int64_t dollar_apply_code(int64_t clos, int64_t x);
static int64_t dollar_code(int64_t clos, int64_t f);
static int64_t append_2_code(int64_t clos, int64_t b);
static int64_t append_1_code(int64_t clos, int64_t a);
static int64_t bind_runner(int64_t clos, int64_t s);
static int64_t cons_closure_code(int64_t clos, int64_t h, int64_t t);
static int64_t flip_code(int64_t clos, int64_t a);
static int64_t flip_apply(int64_t clos, int64_t b);
static int64_t fmap_apply(int64_t clos, int64_t xs);
static int64_t id_code(int64_t clos, int64_t x);
static int64_t pure_runner(int64_t clos, int64_t s);
static int64_t pure_partial(int64_t clos, int64_t a);
static int64_t op2_code(int64_t clos, int64_t a);
static int64_t op2_apply(int64_t clos, int64_t b);
static int64_t not_code(int64_t clos, int64_t x);
static int64_t max_code(int64_t clos, int64_t a);
static int64_t max_apply(int64_t clos, int64_t b);
static int64_t num_op_code(int64_t clos, int64_t a);
static int64_t num_op_apply(int64_t clos, int64_t b);
static int64_t show_code(int64_t clos, int64_t x);
static int64_t try_code(int64_t clos, int64_t action);

/* ------------------------------------------------------------------ */
/*  List helpers                                                        */
/* ------------------------------------------------------------------ */

static int64_t list_append(int64_t xs, int64_t ys) {
    if (kk_is_nil(xs)) return ys;
    int64_t rev = kk_nil();
    int64_t cur = xs;
    while (!kk_is_nil(cur)) {
        rev = kk_cons(kk_list_head(cur), rev);
        cur = kk_list_tail(cur);
    }
    int64_t result = ys;
    while (!kk_is_nil(rev)) {
        result = kk_cons(kk_list_head(rev), result);
        rev = kk_list_tail(rev);
    }
    return result;
}

/* ================================================================== */
/*  GHC.Internal.Base — operators                                       */
/* ================================================================== */

/* --- $ (1-char, collides with .) --- */

static int64_t dollar_apply_code(int64_t clos, int64_t x) {
    int64_t f = kk_field(clos, 1);
    return call1(f, x);
}
static int64_t dollar_code(int64_t clos, int64_t f) {
    (void)clos;
    return make_closure1(&dollar_apply_code, f);
}

int64_t ghc_base_dollar_0(void)   __asm__("GHC_Internal_Base__$0");
int64_t ghc_base_dollar_0(void)   { return make_closure0(&dollar_code); }

int64_t ghc_base_dollar_1(int64_t f)        __asm__("GHC_Internal_Base__$1");
int64_t ghc_base_dollar_1(int64_t f)        { return f; }

int64_t ghc_base_dollar_2(int64_t f, int64_t x) __asm__("GHC_Internal_Base__$2");
int64_t ghc_base_dollar_2(int64_t f, int64_t x) { return call1(f, x); }

/* --- <> / ++ / >> (2-char, all collide → ___) --- */

static int64_t append_2_code(int64_t clos, int64_t b) {
    int64_t a = kk_field(clos, 1);
    if (kk_is_string(a) && kk_is_string(b))
        return kk_str_concat(a, b);
    if (kk_is_nil(a) || (!kk_is_string(a) && kk_is_heap_ptr(a) && kk_tag(a) == KK_CONS_TAG))
        return list_append(a, b);
    if (!kk_is_heap_ptr(a))
        return list_append(a, b);  /* nil = non-heap 0 */
    return kk_str_concat(a, b);
}
static int64_t append_1_code(int64_t clos, int64_t a) {
    (void)clos;
    return make_closure1(&append_2_code, a);
}

int64_t ghc_base_append_0(void)   __asm__("GHC_Internal_Base___$0");
int64_t ghc_base_append_0(void)   { return make_closure0(&append_1_code); }

int64_t ghc_base_append_1(int64_t a)        __asm__("GHC_Internal_Base___$1");
int64_t ghc_base_append_1(int64_t a)        { return make_closure1(&append_2_code, a); }

int64_t ghc_base_append_2(int64_t a, int64_t b) __asm__("GHC_Internal_Base___$2");
int64_t ghc_base_append_2(int64_t a, int64_t b) {
    if (kk_is_string(a) && kk_is_string(b))
        return kk_str_concat(a, b);
    if (kk_is_nil(a)) return b;
    if (!kk_is_string(a) && kk_is_heap_ptr(a) && kk_tag(a) == KK_CONS_TAG)
        return list_append(a, b);
    return kk_str_concat(a, b);
}

/* --- >>= (3-char → ____) --- State monad bind */

static int64_t bind_runner(int64_t clos, int64_t s) {
    int64_t m = kk_field(clos, 1);
    int64_t f = kk_field(clos, 2);
    int64_t result = call1(m, s);
    int64_t a  = kk_fst(result);
    int64_t s2 = kk_snd(result);
    int64_t g = call1(f, a);
    return call1(g, s2);
}

int64_t ghc_base_bind_2(int64_t m, int64_t f) __asm__("GHC_Internal_Base____$2");
int64_t ghc_base_bind_2(int64_t m, int64_t f) {
    return make_closure2(&bind_runner, m, f);
}

int64_t ghc_base_bind_3(int64_t dict, int64_t m, int64_t f) __asm__("GHC_Internal_Base____$3");
int64_t ghc_base_bind_3(int64_t dict, int64_t m, int64_t f) {
    (void)dict;
    return make_closure2(&bind_runner, m, f);
}

/* ================================================================== */
/*  GHC.Internal.Base — named functions                                 */
/* ================================================================== */

static int64_t cons_closure_code(int64_t clos, int64_t h, int64_t t) {
    (void)clos;
    return kk_cons(h, t);
}

int64_t ghc_base_build_1(int64_t g) __asm__("GHC_Internal_Base_build$1");
int64_t ghc_base_build_1(int64_t g) {
    int64_t cc = make_closure0((void*)&cons_closure_code);
    return call2(g, cc, kk_nil());
}

static int64_t flip_apply(int64_t clos, int64_t b) {
    int64_t f = kk_field(clos, 1);
    int64_t a = kk_field(clos, 2);
    return call2(f, b, a);
}
static int64_t flip_code(int64_t clos, int64_t a) {
    int64_t f = kk_field(clos, 1);
    return make_closure2(&flip_apply, f, a);
}

int64_t ghc_base_flip_1(int64_t f) __asm__("GHC_Internal_Base_flip$1");
int64_t ghc_base_flip_1(int64_t f) { return make_closure1(&flip_code, f); }

/* fmap / map: list map */
static int64_t fmap_apply(int64_t clos, int64_t xs) {
    int64_t f = kk_field(clos, 1);
    int64_t result = kk_nil();
    int64_t *stack = NULL;
    int64_t count = 0, cap2 = 0;
    int64_t cur = xs;
    while (!kk_is_nil(cur)) {
        int64_t mapped = call1(f, kk_list_head(cur));
        if (count >= cap2) { cap2 = cap2 ? cap2 * 2 : 16; stack = realloc(stack, (size_t)cap2 * sizeof(int64_t)); }
        stack[count++] = mapped;
        cur = kk_list_tail(cur);
    }
    for (int64_t i = count - 1; i >= 0; i--)
        result = kk_cons(stack[i], result);
    free(stack);
    return result;
}

int64_t ghc_base_fmap_1(int64_t f) __asm__("GHC_Internal_Base_fmap$1");
int64_t ghc_base_fmap_1(int64_t f) { return make_closure1(&fmap_apply, f); }

int64_t ghc_base_fmap_2(int64_t f, int64_t xs) __asm__("GHC_Internal_Base_fmap$2");
int64_t ghc_base_fmap_2(int64_t f, int64_t xs) {
    int64_t clos = make_closure1(&fmap_apply, f);
    return fmap_apply(clos, xs);
}

int64_t ghc_base_foldr_3(int64_t f, int64_t z, int64_t xs) __asm__("GHC_Internal_Base_foldr$3");
int64_t ghc_base_foldr_3(int64_t f, int64_t z, int64_t xs) {
    if (kk_is_nil(xs)) return z;
    return call2(f, kk_list_head(xs), ghc_base_foldr_3(f, z, kk_list_tail(xs)));
}

static int64_t id_code(int64_t clos, int64_t x) { (void)clos; return x; }

int64_t ghc_base_id_0(void) __asm__("GHC_Internal_Base_id$0");
int64_t ghc_base_id_0(void) { return make_closure0(&id_code); }

int64_t ghc_base_map_1(int64_t f) __asm__("GHC_Internal_Base_map$1");
int64_t ghc_base_map_1(int64_t f) { return make_closure1(&fmap_apply, f); }

int64_t ghc_base_map_2(int64_t f, int64_t xs) __asm__("GHC_Internal_Base_map$2");
int64_t ghc_base_map_2(int64_t f, int64_t xs) { return ghc_base_fmap_2(f, xs); }

/* pure: State monad — pure a = \s -> (a, s) */
static int64_t pure_runner(int64_t clos, int64_t s) {
    return kk_pair(kk_field(clos, 1), s);
}
static int64_t pure_partial(int64_t clos, int64_t a) {
    (void)clos;
    return make_closure1(&pure_runner, a);
}

int64_t ghc_base_pure_0(void) __asm__("GHC_Internal_Base_pure$0");
int64_t ghc_base_pure_0(void) { return make_closure0(&pure_partial); }

int64_t ghc_base_pure_1(int64_t a) __asm__("GHC_Internal_Base_pure$1");
int64_t ghc_base_pure_1(int64_t a) { return make_closure1(&pure_runner, a); }

int64_t ghc_base_pure_2(int64_t d, int64_t a) __asm__("GHC_Internal_Base_pure$2");
int64_t ghc_base_pure_2(int64_t d, int64_t a) { (void)d; return make_closure1(&pure_runner, a); }

int64_t ghc_base_p1Monad_0(void) __asm__("GHC_Internal_Base__p1Monad$0");
int64_t ghc_base_p1Monad_0(void) { return 0; }

/* ================================================================== */
/*  GHC.Internal.Classes                                                */
/* ================================================================== */

static int64_t op2_apply(int64_t clos, int64_t b) {
    int64_t a = kk_field(clos, 1);
    if (kk_is_string(a) && kk_is_string(b)) return kk_str_eq(a, b);
    return (a || b) ? 1 : 0;
}
static int64_t op2_code(int64_t clos, int64_t a) {
    (void)clos;
    return make_closure1(&op2_apply, a);
}

int64_t ghc_classes_op2_0(void)          __asm__("GHC_Internal_Classes___$0");
int64_t ghc_classes_op2_0(void)          { return make_closure0(&op2_code); }

int64_t ghc_classes_op2_2(int64_t a, int64_t b) __asm__("GHC_Internal_Classes___$2");
int64_t ghc_classes_op2_2(int64_t a, int64_t b) {
    if (kk_is_string(a) && kk_is_string(b)) return kk_str_eq(a, b);
    return (a || b) ? 1 : 0;
}

int64_t ghc_classes_cmp_2(int64_t a, int64_t b) __asm__("GHC_Internal_Classes__$2");
int64_t ghc_classes_cmp_2(int64_t a, int64_t b) {
    if (kk_is_string(a) && kk_is_string(b)) return kk_str_compare(a, b) < 0 ? 1 : 0;
    return (a < b) ? 1 : 0;
}

static int64_t not_code(int64_t clos, int64_t x) { (void)clos; return x ? 0 : 1; }

int64_t ghc_classes_not_0(void)   __asm__("GHC_Internal_Classes_not$0");
int64_t ghc_classes_not_0(void)   { return make_closure0(&not_code); }
int64_t ghc_classes_not_1(int64_t x) __asm__("GHC_Internal_Classes_not$1");
int64_t ghc_classes_not_1(int64_t x) { return x ? 0 : 1; }

int64_t ghc_classes_fEqList_0(void) __asm__("GHC_Internal_Classes__fEqList$0");
int64_t ghc_classes_fEqList_0(void) { return 0; }

int64_t ghc_classes_fOrdTuple2_0(void) __asm__("GHC_Internal_Classes__fOrdTuple2$0");
int64_t ghc_classes_fOrdTuple2_0(void) { return 0; }

int64_t ghc_classes_ip_1(int64_t x) __asm__("GHC_Internal_Classes_ip$1");
int64_t ghc_classes_ip_1(int64_t x) { return x; }

static int64_t max_apply(int64_t clos, int64_t b) { return kk_field(clos,1) > b ? kk_field(clos,1) : b; }
static int64_t max_code(int64_t clos, int64_t a) { (void)clos; return make_closure1(&max_apply, a); }

int64_t ghc_classes_max_0(void) __asm__("GHC_Internal_Classes_max$0");
int64_t ghc_classes_max_0(void) { return make_closure0(&max_code); }
int64_t ghc_classes_max_2(int64_t a, int64_t b) __asm__("GHC_Internal_Classes_max$2");
int64_t ghc_classes_max_2(int64_t a, int64_t b) { return a > b ? a : b; }

/* ================================================================== */
/*  GHC.Internal.Num / Real / Enum / Show                               */
/* ================================================================== */

static int64_t num_op_apply(int64_t clos, int64_t b) { return kk_field(clos,1) + b; }
static int64_t num_op_code(int64_t clos, int64_t a)  { (void)clos; return make_closure1(&num_op_apply, a); }

int64_t ghc_num_op_0(void) __asm__("GHC_Internal_Num__$0");
int64_t ghc_num_op_0(void) { return make_closure0(&num_op_code); }
int64_t ghc_num_op_2(int64_t a, int64_t b) __asm__("GHC_Internal_Num__$2");
int64_t ghc_num_op_2(int64_t a, int64_t b) { return a + b; }
int64_t ghc_num_fromInteger_1(int64_t n) __asm__("GHC_Internal_Num_fromInteger$1");
int64_t ghc_num_fromInteger_1(int64_t n) { return n; }
int64_t ghc_num_negate_1(int64_t n) __asm__("GHC_Internal_Num_negate$1");
int64_t ghc_num_negate_1(int64_t n) { return -n; }

int64_t ghc_real_fromIntegral_1(int64_t n) __asm__("GHC_Internal_Real_fromIntegral$1");
int64_t ghc_real_fromIntegral_1(int64_t n) { return n; }
int64_t ghc_real_fromRational_1(int64_t n) __asm__("GHC_Internal_Real_fromRational$1");
int64_t ghc_real_fromRational_1(int64_t n) { return n; }
int64_t ghc_real_toInteger_1(int64_t n) __asm__("GHC_Internal_Real_toInteger$1");
int64_t ghc_real_toInteger_1(int64_t n) { return n; }

int64_t ghc_enum_fromEnum_1(int64_t n) __asm__("GHC_Internal_Enum_fromEnum$1");
int64_t ghc_enum_fromEnum_1(int64_t n) { return n; }
int64_t ghc_enum_toEnum_1(int64_t n) __asm__("GHC_Internal_Enum_toEnum$1");
int64_t ghc_enum_toEnum_1(int64_t n) { return n; }

int64_t ghc_enum_enumFrom_1(int64_t n) __asm__("GHC_Internal_Enum_enumFrom$1");
int64_t ghc_enum_enumFrom_1(int64_t n) {
    int64_t limit = n + 10000;
    int64_t result = kk_nil();
    for (int64_t i = limit; i >= n; i--) result = kk_cons(i, result);
    return result;
}

int64_t ghc_enum_enumFromTo_2(int64_t lo, int64_t hi) __asm__("GHC_Internal_Enum_enumFromTo$2");
int64_t ghc_enum_enumFromTo_2(int64_t lo, int64_t hi) {
    int64_t result = kk_nil();
    for (int64_t i = hi; i >= lo; i--) result = kk_cons(i, result);
    return result;
}

static int64_t show_code(int64_t clos, int64_t x) {
    (void)clos;
    if (kk_is_string(x)) return x;
    return kk_str_show_int(x);
}

int64_t ghc_show_show_0(void) __asm__("GHC_Internal_Show_show$0");
int64_t ghc_show_show_0(void) { return make_closure0(&show_code); }
int64_t ghc_show_show_1(int64_t x) __asm__("GHC_Internal_Show_show$1");
int64_t ghc_show_show_1(int64_t x) { return kk_is_string(x) ? x : kk_str_show_int(x); }

/* ================================================================== */
/*  GHC.Internal.Err / Exception / IO / Stack                           */
/* ================================================================== */

int64_t ghc_err_error_1(int64_t msg) __asm__("GHC_Internal_Err_error$1");
int64_t ghc_err_error_1(int64_t msg) {
    fprintf(stderr, "error: ");
    if (kk_is_string(msg)) kk_print_str(msg);
    fprintf(stderr, "\n");
    abort();
    return 0;
}

int64_t ghc_err_undefined_0(void) __asm__("GHC_Internal_Err_undefined$0");
int64_t ghc_err_undefined_0(void) { fprintf(stderr, "Prelude.undefined\n"); abort(); return 0; }

int64_t ghc_patError_1(int64_t msg) __asm__("GHC_Internal_Control_Exception_Base_patError$1");
int64_t ghc_patError_1(int64_t msg) {
    fprintf(stderr, "Pattern match failure: ");
    if (kk_is_string(msg)) kk_print_str(msg);
    fprintf(stderr, "\n");
    abort();
    return 0;
}

static int64_t try_code(int64_t clos, int64_t action) {
    (void)clos;
    int64_t result = call0(action);
    int64_t right = kk_alloc_con(1, 1);
    kk_set_field(right, 0, result);
    return right;
}
int64_t ghc_try_0(void) __asm__("GHC_Internal_Control_Exception_Base_try$0");
int64_t ghc_try_0(void) { return make_closure0(&try_code); }

int64_t ghc_io_catch_2(int64_t action, int64_t handler) __asm__("GHC_Internal_IO_catch$2");
int64_t ghc_io_catch_2(int64_t action, int64_t handler) { (void)handler; return call0(action); }

int64_t ghc_emptyCallStack_0(void) __asm__("GHC_Internal_Stack_Types_emptyCallStack$0");
int64_t ghc_emptyCallStack_0(void) { return 0; }
int64_t ghc_pushCallStack_2(int64_t info, int64_t cs) __asm__("GHC_Internal_Stack_Types_pushCallStack$2");
int64_t ghc_pushCallStack_2(int64_t info, int64_t cs) { (void)info; return cs; }

/* ================================================================== */
/*  Control.Monad.State                                                 */
/* ================================================================== */

static int64_t state_get_code(int64_t clos, int64_t s)    { (void)clos; return kk_pair(s, s); }
static int64_t state_gets_code(int64_t clos, int64_t s)   { return kk_pair(call1(kk_field(clos,1), s), s); }
static int64_t state_modify_code(int64_t clos, int64_t s) { return kk_pair(0, call1(kk_field(clos,1), s)); }
static int64_t state_put_code(int64_t clos, int64_t s)    { (void)s; return kk_pair(0, kk_field(clos,1)); }

int64_t state_get_0(void) __asm__("Control_Monad_State_Class_get$0");
int64_t state_get_0(void) { return make_closure0(&state_get_code); }

int64_t state_gets_1(int64_t f) __asm__("Control_Monad_State_Class_gets$1");
int64_t state_gets_1(int64_t f) { return make_closure1(&state_gets_code, f); }

int64_t state_modify_1(int64_t f) __asm__("Control_Monad_State_Class_modify$1");
int64_t state_modify_1(int64_t f) { return make_closure1(&state_modify_code, f); }

int64_t state_put_1(int64_t s2) __asm__("Control_Monad_State_Class_put$1");
int64_t state_put_1(int64_t s2) { return make_closure1(&state_put_code, s2); }

int64_t state_runState_2(int64_t m, int64_t s) __asm__("Control_Monad_Trans_State_Lazy_runState$2");
int64_t state_runState_2(int64_t m, int64_t s) { return call1(m, s); }
