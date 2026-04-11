/* Self-hosted Frankenstein driver
 *
 * Constructs Frankenstein Core IR values using the kk_* runtime,
 * then calls self-hosted record selectors from Types.o — compiled
 * through Frankenstein's own pipeline.
 */

#include <stdio.h>
#include <string.h>
#include "../runtime/kk_runtime.h"

/* Self-hosted selectors from Types.o — module-qualified symbol names */
#define T(name) Frankenstein_Core_Types_##name
extern int64_t T(nameText)(int64_t);
extern int64_t T(nameUnique)(int64_t);
extern int64_t T(qnameName)(int64_t);
extern int64_t T(qnameModule)(int64_t);
extern int64_t T(defName)(int64_t);
extern int64_t T(defExpr)(int64_t);
extern int64_t T(defVisibility)(int64_t);
extern int64_t T(progName)(int64_t);
extern int64_t T(progDefs)(int64_t);
extern int64_t T(effectName)(int64_t);
extern int64_t T(conName)(int64_t);
extern int64_t T(dataName)(int64_t);
#define nameText      T(nameText)
#define nameUnique    T(nameUnique)
#define qnameName     T(qnameName)
#define qnameModule   T(qnameModule)
#define defName       T(defName)
#define defExpr       T(defExpr)
#define defVisibility T(defVisibility)
#define progName      T(progName)
#define progDefs      T(progDefs)
#define effectName    T(effectName)
#define conName       T(conName)
#define dataName      T(dataName)

static int pass = 0, fail = 0;
#define CHECK(desc, cond) do { \
    if (cond) { printf("  PASS: %s\n", desc); pass++; } \
    else      { printf("  FAIL: %s\n", desc); fail++; } \
} while(0)

static int64_t s(const char *c) {
    return kk_string_from_literal((int64_t)c, (int64_t)strlen(c));
}
static int64_t ph(void) { return kk_alloc_con(0, 0); }
static int64_t nil(void) { return kk_alloc_con(0, 0); }
static int64_t cons(int64_t h, int64_t t) {
    int64_t c = kk_alloc_con(1, 2);
    kk_set_field(c, 0, h);
    kk_set_field(c, 1, t);
    return c;
}

static int64_t mk_name(const char *text, int64_t u) {
    int64_t n = kk_alloc_con(0, 2);
    kk_set_field(n, 0, s(text));
    kk_set_field(n, 1, u);
    return n;
}

static int64_t mk_qname(const char *mod, const char *name, int64_t u) {
    int64_t qn = kk_alloc_con(0, 2);
    kk_set_field(qn, 0, s(mod));
    kk_set_field(qn, 1, mk_name(name, u));
    return qn;
}

int main(void) {
    printf("=== Frankenstein Self-Hosted Binary ===\n");
    printf("Testing self-hosted record selectors from Types.o\n\n");

    /* --- Name --- */
    printf("[Name]\n");
    int64_t n1 = mk_name("factorial", 42);
    kk_retain(n1);
    CHECK("nameText(\"factorial\", 42) == \"factorial\"",
          kk_str_eq(nameText(n1), s("factorial")));
    CHECK("nameUnique(\"factorial\", 42) == 42",
          nameUnique(n1) == 42);

    int64_t n2 = mk_name("", 0);
    kk_retain(n2);
    CHECK("nameText(\"\", 0) == \"\"",
          kk_str_eq(nameText(n2), s("")));
    CHECK("nameUnique(\"\", 0) == 0",
          nameUnique(n2) == 0);

    int64_t n3 = mk_name("x", 999);
    CHECK("nameUnique(\"x\", 999) == 999",
          nameUnique(n3) == 999);

    /* --- QName --- */
    printf("\n[QName]\n");
    int64_t qn1 = mk_qname("demo", "main", 0);
    kk_retain(qn1);
    CHECK("qnameModule == \"demo\"",
          kk_str_eq(qnameModule(qn1), s("demo")));

    int64_t qn2 = mk_qname("demo", "main", 0);
    int64_t inner = qnameName(qn2);
    kk_retain(inner);
    CHECK("qnameName.nameText == \"main\"",
          kk_str_eq(nameText(inner), s("main")));
    CHECK("qnameName.nameUnique == 0",
          nameUnique(inner) == 0);

    int64_t qn3 = mk_qname("mercury", "exn", 7);
    kk_retain(qn3);
    CHECK("qnameModule(\"mercury\", \"exn\") == \"mercury\"",
          kk_str_eq(qnameModule(qn3), s("mercury")));

    /* --- Def --- */
    printf("\n[Def]\n");
    int64_t def = kk_alloc_con(0, 5);
    kk_set_field(def, 0, mk_qname("demo", "fac", 1));
    kk_set_field(def, 1, ph());   /* type */
    kk_set_field(def, 2, ph());   /* expr (ELit placeholder) */
    kk_set_field(def, 3, ph());   /* DefFun */
    kk_set_field(def, 4, ph());   /* Public */
    kk_retain(def);
    kk_retain(def);

    int64_t dn = defName(def);
    CHECK("defName.nameText == \"fac\"",
          kk_str_eq(nameText(qnameName(dn)), s("fac")));

    int64_t de = defExpr(def);
    CHECK("defExpr returns a value (tag 0)", kk_tag(de) == 0);

    defVisibility(def);  /* just check it doesn't crash */
    CHECK("defVisibility doesn't crash", 1);

    /* --- Program --- */
    printf("\n[Program]\n");
    int64_t prog = kk_alloc_con(0, 4);
    kk_set_field(prog, 0, mk_qname("", "selftest", 0));
    kk_set_field(prog, 1, nil());
    kk_set_field(prog, 2, nil());
    kk_set_field(prog, 3, nil());
    kk_retain(prog);

    int64_t pn = progName(prog);
    CHECK("progName.nameText == \"selftest\"",
          kk_str_eq(nameText(qnameName(pn)), s("selftest")));
    CHECK("progDefs is empty (tag 0)", kk_tag(progDefs(prog)) == 0);

    /* --- EffectDecl --- */
    printf("\n[EffectDecl]\n");
    int64_t eff = kk_alloc_con(0, 3);
    kk_set_field(eff, 0, mk_qname("mercury", "exn", 0));
    kk_set_field(eff, 1, nil());
    int64_t op = kk_alloc_con(0, 2);
    kk_set_field(op, 0, mk_qname("mercury", "raise", 0));
    kk_set_field(op, 1, ph());
    kk_set_field(eff, 2, cons(op, nil()));
    kk_retain(eff);

    CHECK("effectName.nameText == \"exn\"",
          kk_str_eq(nameText(qnameName(effectName(eff))), s("exn")));

    /* --- DataDecl / ConDecl --- */
    printf("\n[DataDecl]\n");
    int64_t cd = kk_alloc_con(0, 3);
    kk_set_field(cd, 0, mk_qname("", "Just", 0));
    kk_set_field(cd, 1, nil());
    kk_set_field(cd, 2, ph());
    kk_retain(cd);
    CHECK("conName.nameText == \"Just\"",
          kk_str_eq(nameText(qnameName(conName(cd))), s("Just")));

    int64_t dd = kk_alloc_con(0, 4);
    kk_set_field(dd, 0, mk_qname("", "Maybe", 0));
    kk_set_field(dd, 1, nil());
    kk_set_field(dd, 2, cons(cd, nil()));
    kk_set_field(dd, 3, ph());
    CHECK("dataName.nameText == \"Maybe\"",
          kk_str_eq(nameText(qnameName(dataName(dd))), s("Maybe")));

    /* --- Summary --- */
    printf("\n=== Results: %d passed, %d failed ===\n", pass, fail);

    if (fail == 0) {
        printf("\nFrankenstein has bootstrapped its own type system!\n");
        printf("Pipeline: Types.hs -> GHC bridge -> Core IR -> Perceus -> MLIR -> LLVM -> ELF -> executed\n");
        printf("Binary size: ");
        fflush(stdout);
    }

    return fail > 0 ? 1 : 0;
}
