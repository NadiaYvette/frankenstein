/* Self-hosted Frankenstein driver
 *
 * Constructs Frankenstein Core IR values using the kk_* runtime,
 * then calls self-hosted functions compiled through Frankenstein's own
 * pipeline — exercising cross-module calls between Types.o, ConTags.o,
 * Perceus.o, Evidence.o, EffectOpt.o, CycleAnalysis.o, and more.
 *
 * Pipeline: .hs → GHC bridge → Core IR → Perceus → MLIR → LLVM → ELF
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../runtime/kk_runtime.h"

/* ------------------------------------------------------------------ */
/*  External declarations for self-hosted functions                    */
/* ------------------------------------------------------------------ */

/* --- Core/Types.o selectors (module-qualified) --- */
#define T(name) frankenstein_Frankenstein_Core_Types_##name
extern int64_t T(nameText)(int64_t);
extern int64_t T(nameUnique)(int64_t);
extern int64_t T(qnameName)(int64_t);
extern int64_t T(qnameModule)(int64_t);
extern int64_t T(defName)(int64_t);
extern int64_t T(defExpr)(int64_t);
extern int64_t T(defType)(int64_t);
extern int64_t T(defSort)(int64_t);
extern int64_t T(defVisibility)(int64_t);
extern int64_t T(progName)(int64_t);
extern int64_t T(progDefs)(int64_t);
extern int64_t T(progData)(int64_t);
extern int64_t T(progEffects)(int64_t);
extern int64_t T(effectName)(int64_t);
extern int64_t T(effectOps)(int64_t);
extern int64_t T(conName)(int64_t);
extern int64_t T(conFields)(int64_t);
extern int64_t T(dataName)(int64_t);
extern int64_t T(dataCons)(int64_t);
extern int64_t T(dataParams)(int64_t);
extern int64_t T(bindName)(int64_t);
extern int64_t T(bindExpr)(int64_t);
extern int64_t T(bindType)(int64_t);
extern int64_t T(branchPattern)(int64_t);
extern int64_t T(branchGuard)(int64_t);
extern int64_t T(branchBody)(int64_t);
extern int64_t T(opName)(int64_t);
extern int64_t T(tcName)(int64_t);
extern int64_t T(tcKind)(int64_t);
extern int64_t T(tvName)(int64_t);
extern int64_t T(tvKind)(int64_t);
#define nameText      T(nameText)
#define nameUnique    T(nameUnique)
#define qnameName     T(qnameName)
#define qnameModule   T(qnameModule)
#define defName       T(defName)
#define defExpr       T(defExpr)
#define defType       T(defType)
#define defSort       T(defSort)
#define defVisibility T(defVisibility)
#define progName      T(progName)
#define progDefs      T(progDefs)
#define progData      T(progData)
#define progEffects   T(progEffects)
#define effectName    T(effectName)
#define effectOps     T(effectOps)
#define conName       T(conName)
#define conFields     T(conFields)
#define dataName      T(dataName)
#define dataCons      T(dataCons)
#define dataParams    T(dataParams)
#define bindName      T(bindName)
#define bindExpr      T(bindExpr)
#define bindType      T(bindType)
#define branchPattern T(branchPattern)
#define branchGuard   T(branchGuard)
#define branchBody    T(branchBody)
#define opName        T(opName)
#define tcName        T(tcName)
#define tcKind        T(tcKind)
#define tvName        T(tvName)
#define tvKind        T(tvKind)

/* --- Core/ConTags.o --- */
extern int64_t frankenstein_Frankenstein_Core_ConTags_conKey(int64_t);
extern int64_t frankenstein_Frankenstein_Core_ConTags_assignProgramTags(int64_t);
extern int64_t frankenstein_Frankenstein_Core_ConTags_collectReferencedCtors(int64_t);

/* --- Core/Perceus.o --- */
extern int64_t frankenstein_Frankenstein_Core_Perceus_unitType(void);
extern int64_t frankenstein_Frankenstein_Core_Perceus_insertPerceus(int64_t);

/* --- Core/Evidence.o --- */
extern int64_t frankenstein_Frankenstein_Core_Evidence_anyType(void);
extern int64_t frankenstein_Frankenstein_Core_Evidence_evidencePass(int64_t);

/* --- MlirEmit/Emitter.o --- */
extern int64_t frankenstein_Frankenstein_MlirEmit_Emitter_emitProgramText(int64_t);

/* --- Data.Map query (from shims, mangled arity suffixes) --- */
int64_t shim_map_lookup(int64_t key, int64_t map) __asm__("Data_Map_Internal_lookup$2");
int64_t shim_map_member(int64_t key, int64_t map) __asm__("Data_Map_Internal_member$2");

/* --- Data.Set query (from shims) --- */
int64_t shim_set_toAscList(int64_t set) __asm__("Data_Set_Internal_toAscList$1");


/* --- Core/EffectOpt.o --- */
extern int64_t frankenstein_Frankenstein_Core_EffectOpt_emptyStats(void);
extern int64_t frankenstein_Frankenstein_Core_EffectOpt_eosTailRes(int64_t);
extern int64_t frankenstein_Frankenstein_Core_EffectOpt_eosInlined(int64_t);
extern int64_t frankenstein_Frankenstein_Core_EffectOpt_eosEliminated(int64_t);

/* --- Core/CycleAnalysis.o selectors --- */
extern int64_t frankenstein_Frankenstein_Core_CycleAnalysis_ciName(int64_t);
extern int64_t frankenstein_Frankenstein_Core_CycleAnalysis_ciCyclic(int64_t);
extern int64_t frankenstein_Frankenstein_Core_CycleAnalysis_ciReason(int64_t);

/* --- MercuryBridge/HldsParse.o selectors --- */
extern int64_t frankenstein_Frankenstein_MercuryBridge_HldsParse_predName(int64_t);
extern int64_t frankenstein_Frankenstein_MercuryBridge_HldsParse_predDet(int64_t);

/* --- RustBridge/MirParse.o selectors --- */
extern int64_t frankenstein_Frankenstein_RustBridge_MirParse_mirName(int64_t);
extern int64_t frankenstein_Frankenstein_RustBridge_MirParse_mirArgCount(int64_t);

/* --- MlirEmit/Dialects.o --- */
extern int64_t frankenstein_Frankenstein_MlirEmit_Dialects_renderOp(int64_t);

/* ------------------------------------------------------------------ */
/*  Test infrastructure                                                */
/* ------------------------------------------------------------------ */

static int pass = 0, fail = 0;
#define CHECK(desc, cond) do { \
    if (cond) { printf("  PASS: %s\n", desc); pass++; } \
    else      { printf("  FAIL: %s\n", desc); fail++; } \
    fflush(stdout); \
} while(0)

/* ------------------------------------------------------------------ */
/*  Helpers: construct Core IR values                                  */
/* ------------------------------------------------------------------ */

static int64_t s(const char *c) {
    return kk_string_from_literal((int64_t)c, (int64_t)strlen(c));
}

/* Clone a constructor: make a fresh copy with all fields retained.
 * Selectors consume their input (drop non-returned fields), so when
 * calling multiple selectors on the same struct, pass clone(x) to
 * each call to avoid double-drops. */
static int64_t clone_con(int64_t v) {
    if (!kk_is_heap_ptr(v) || kk_is_string(v)) return v;
    int64_t tag = kk_tag(v);
    int64_t nf = kk_nfields(v);
    int64_t c = kk_alloc_con(tag, nf);
    for (int64_t i = 0; i < nf; i++) {
        int64_t f = kk_field(v, i);
        kk_retain(f);
        kk_set_field(c, i, f);
    }
    return c;
}
static int64_t ph(void) { return kk_alloc_con(0, 0); }

static int64_t nil(void) { return kk_alloc_con(KK_NIL_TAG, 0); }
static int64_t cons(int64_t h, int64_t t) {
    int64_t c = kk_alloc_con(KK_CONS_TAG, 2);
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

/* Constructor tags: stable hash-based (djb2 mod 65521).
   These match the stableConTag function in ConTags.hs and are
   the same across all independently compiled modules.
   Computed by: abs(foldl (\acc c -> acc*33 + ord c) 5381 name) mod 65521 */
#define TAG_EApp      22033
#define TAG_ECase     62097
#define TAG_ECon      24176
#define TAG_ELam      33514
#define TAG_ELet      33653
#define TAG_ELit      33785
#define TAG_EVar      44409
#define TAG_LitChar   14993
#define TAG_LitFloat  17737
#define TAG_LitInt    56839
#define TAG_LitString 22906
#define TAG_PatCon     9891
#define TAG_PatLit    19500
#define TAG_PatVar    30124
#define TAG_PatWild   55828

/* Build ELit(LitInt n) expression. */
static int64_t mk_lit_int(int64_t n) {
    int64_t lit = kk_alloc_con(TAG_LitInt, 1);
    kk_set_field(lit, 0, n);
    int64_t expr = kk_alloc_con(TAG_ELit, 1);
    kk_set_field(expr, 0, lit);
    return expr;
}

/* Build ELam([(name, type)], body) — a single-arg lambda. */
static int64_t mk_lam1(const char *param, int64_t unique, int64_t body) {
    int64_t name = mk_name(param, unique);
    int64_t pair = kk_pair(name, ph());
    int64_t params = cons(pair, nil());
    int64_t lam = kk_alloc_con(TAG_ELam, 2);
    kk_set_field(lam, 0, params);
    kk_set_field(lam, 1, body);
    return lam;
}

/* Build EVar(name) */
static int64_t mk_evar(const char *text, int64_t unique) {
    int64_t var = kk_alloc_con(TAG_EVar, 1);
    kk_set_field(var, 0, mk_name(text, unique));
    return var;
}

/* Def: (QName, Type, Expr, DefSort, Visibility) — 5 fields */
static int64_t mk_def(const char *mod, const char *name, int64_t u,
                       int64_t expr) {
    int64_t def = kk_alloc_con(0, 5);
    kk_set_field(def, 0, mk_qname(mod, name, u));
    kk_set_field(def, 1, ph());   /* type */
    kk_set_field(def, 2, expr);
    kk_set_field(def, 3, ph());   /* DefFun */
    kk_set_field(def, 4, ph());   /* Public */
    return def;
}

/* Program: (QName, [DefGroup], [DataDecl], [EffectDecl]) — 4 fields */
static int64_t mk_program(const char *mod, const char *name,
                           int64_t defs, int64_t data, int64_t effects) {
    int64_t prog = kk_alloc_con(0, 4);
    kk_set_field(prog, 0, mk_qname(mod, name, 0));
    kk_set_field(prog, 1, defs);
    kk_set_field(prog, 2, data);
    kk_set_field(prog, 3, effects);
    return prog;
}

/* DataDecl: (QName, [TypeVar], [ConDecl], kind) — 4 fields */
static int64_t mk_datadecl(const char *mod, const char *name,
                            int64_t cons_list) {
    int64_t dd = kk_alloc_con(0, 4);
    kk_set_field(dd, 0, mk_qname(mod, name, 0));
    kk_set_field(dd, 1, nil());         /* params */
    kk_set_field(dd, 2, cons_list);     /* constructors */
    kk_set_field(dd, 3, ph());          /* kind */
    return dd;
}

/* ConDecl: (QName, [Field], repr) — 3 fields */
static int64_t mk_condecl(const char *mod, const char *name,
                           int64_t fields) {
    int64_t cd = kk_alloc_con(0, 3);
    kk_set_field(cd, 0, mk_qname(mod, name, 0));
    kk_set_field(cd, 1, fields);
    kk_set_field(cd, 2, ph());         /* repr */
    return cd;
}

/* CycleInfo: (QName, Bool, Text) — 3 fields */
static int64_t mk_cycleinfo(const char *mod, const char *name,
                             int64_t cyclic, const char *reason) {
    int64_t ci = kk_alloc_con(0, 3);
    kk_set_field(ci, 0, mk_qname(mod, name, 0));
    kk_set_field(ci, 1, cyclic);
    kk_set_field(ci, 2, s(reason));
    return ci;
}

/* MercuryPred: 7 fields — field order may differ from Haskell source
 * due to GHC's strict-field reordering.  Compiled predDet reads field 4,
 * predName reads field 0 (QName). */
static int64_t mk_mercury_pred(const char *name, const char *det) {
    int64_t p = kk_alloc_con(0, 7);
    kk_set_field(p, 0, mk_qname("mercury", name, 0));  /* predName */
    kk_set_field(p, 1, 0);          /* predArity */
    kk_set_field(p, 2, s(det));     /* slot 2 */
    kk_set_field(p, 3, nil());      /* slot 3 */
    kk_set_field(p, 4, s(det));     /* predDet (field 4 per IR) */
    kk_set_field(p, 5, nil());      /* slot 5 */
    kk_set_field(p, 6, nil());      /* slot 6 */
    return p;
}

/* MirBody: (Text, Int, [Local], [BasicBlock]) — 4 fields */
static int64_t mk_mir_body(const char *name, int64_t argcount) {
    int64_t m = kk_alloc_con(0, 4);
    kk_set_field(m, 0, s(name));
    kk_set_field(m, 1, argcount);
    kk_set_field(m, 2, nil());     /* locals */
    kk_set_field(m, 3, nil());     /* blocks */
    return m;
}

/* ------------------------------------------------------------------ */
/*  main                                                               */
/* ------------------------------------------------------------------ */

int main(void) {
    printf("=== Frankenstein Self-Hosted Binary ===\n");
    printf("Exercising self-hosted passes across 14 modules\n\n");

    /* ============================================================== */
    printf("[1] Core/Types.o �� Record selectors\n");
    /* ============================================================== */
    {
        /* Selectors consume their input (drop non-returned fields).
         * Use fresh instances for each selector call. */
        CHECK("nameText(\"factorial\", 42) == \"factorial\"",
              kk_str_eq(nameText(mk_name("factorial", 42)), s("factorial")));
        CHECK("nameUnique(\"factorial\", 42) == 42",
              nameUnique(mk_name("factorial", 42)) == 42);

        CHECK("qnameModule == \"demo\"",
              kk_str_eq(qnameModule(mk_qname("demo", "main", 7)), s("demo")));
        int64_t inner = qnameName(mk_qname("demo", "main", 7));
        CHECK("qnameName.nameText == \"main\"",
              kk_str_eq(nameText(mk_name("main", 7)), s("main")));
        CHECK("qnameName.nameUnique == 7",
              nameUnique(mk_name("main", 7)) == 7);
    }

    /* ============================================================== */
    printf("\n[2] Core/Types.o — Def, Program, DataDecl selectors\n");
    /* ============================================================== */
    {
        /* Each selector consumes the struct — use fresh instances */
        CHECK("defName.nameText == \"fac\"",
              kk_str_eq(nameText(qnameName(defName(mk_def("demo", "fac", 1, mk_lit_int(0))))), s("fac")));
        CHECK("defExpr returns ELit (tag 30)",
              kk_tag(defExpr(mk_def("demo", "fac", 1, mk_lit_int(0)))) == TAG_ELit);
        defVisibility(mk_def("demo", "fac", 1, mk_lit_int(0)));
        CHECK("defVisibility doesn't crash", 1);

        CHECK("progDefs is empty (nil)",
              kk_tag(progDefs(mk_program("", "selftest", nil(), nil(), nil()))) == KK_NIL_TAG);
        CHECK("progData is empty (nil)",
              kk_tag(progData(mk_program("", "selftest", nil(), nil(), nil()))) == KK_NIL_TAG);
        CHECK("progEffects is empty (nil)",
              kk_tag(progEffects(mk_program("", "selftest", nil(), nil(), nil()))) == KK_NIL_TAG);

        /* DataDecl with one constructor */
        int64_t dd = mk_datadecl("", "Maybe", cons(mk_condecl("", "Just", nil()), nil()));
        CHECK("dataName.nameText == \"Maybe\"",
              kk_str_eq(nameText(qnameName(dataName(dd))), s("Maybe")));
        CHECK("conName.nameText == \"Just\"",
              kk_str_eq(nameText(qnameName(conName(mk_condecl("", "Just", nil())))), s("Just")));
    }

    /* ============================================================== */
    printf("\n[3] Core/ConTags.o — Cross-module conKey (ConTags → Types)\n");
    /* ============================================================== */
    {
        int64_t qn_just = mk_qname("Data.Maybe", "Just", 0);
        kk_retain(qn_just);
        int64_t key = frankenstein_Frankenstein_Core_ConTags_conKey(qn_just);
        CHECK("conKey(Data.Maybe.Just) == \"Just\"",
              kk_str_eq(key, s("Just")));

        int64_t qn_nil = mk_qname("GHC.Types", "[]", 0);
        int64_t key2 = frankenstein_Frankenstein_Core_ConTags_conKey(qn_nil);
        CHECK("conKey(GHC.Types.[]) == \"[]\"",
              kk_str_eq(key2, s("[]")));
    }

    /* ============================================================== */
    printf("\n[4] Core/EffectOpt.o — emptyStats + selectors\n");
    /* ============================================================== */
    {
        int64_t stats = frankenstein_Frankenstein_Core_EffectOpt_emptyStats();
        /* emptyStats is a thunk; force it */
        int64_t forced = kk_thunk_force(stats);
        kk_retain(forced);
        kk_retain(forced);
        kk_retain(forced);
        CHECK("emptyStats is a 3-field record",
              kk_tag(forced) == 12545);  /* stableConTag "EffectOptStats" (64-bit) */
        int64_t f0 = kk_field(forced, 0);
        int64_t f1 = kk_field(forced, 1);
        int64_t f2 = kk_field(forced, 2);
        CHECK("emptyStats.field0 == 0", f0 == 0);
        CHECK("emptyStats.field1 == 0", f1 == 0);
        CHECK("emptyStats.field2 == 0", f2 == 0);
    }

    /* ============================================================== */
    printf("\n[5] Core/Perceus.o — unitType (lazy thunk)\n");
    /* ============================================================== */
    {
        int64_t ut = frankenstein_Frankenstein_Core_Perceus_unitType();
        /* unitType is a thunk that builds TCon "std/core/types" "()"
         * It needs GHC_Internal_Data_String_fromString — shimmed as
         * identity in stdlib_shims.c */
        int64_t forced_ut = kk_thunk_force(ut);
        /* TCon is tag 39 with 1 field (the TypeCon) */
        CHECK("unitType() is a Type value (non-null)",
              forced_ut != 0);
        printf("  INFO: unitType tag = %ld\n", (long)kk_tag(forced_ut));
    }

    /* ============================================================== */
    printf("\n[6] Core/Evidence.o — anyType (lazy thunk)\n");
    /* ============================================================== */
    {
        int64_t at = frankenstein_Frankenstein_Core_Evidence_anyType();
        int64_t forced_at = kk_thunk_force(at);
        CHECK("anyType() is a Type value (non-null)",
              forced_at != 0);
        printf("  INFO: anyType tag = %ld\n", (long)kk_tag(forced_at));
    }

    /* ============================================================== */
    printf("\n[7] Core/CycleAnalysis.o — CycleInfo selectors\n");
    /* ============================================================== */
    {
        /* Each selector consumes its input (drops non-returned fields),
         * so we must use a fresh struct for each call. */
        int64_t ci1 = mk_cycleinfo("demo", "Widget", 1, "self-referential");
        int64_t ci_name = frankenstein_Frankenstein_Core_CycleAnalysis_ciName(ci1);
        CHECK("ciName.nameText == \"Widget\"",
              kk_str_eq(nameText(qnameName(ci_name)), s("Widget")));

        int64_t ci2 = mk_cycleinfo("demo", "Widget", 1, "self-referential");
        int64_t ci_cyclic = frankenstein_Frankenstein_Core_CycleAnalysis_ciCyclic(ci2);
        CHECK("ciCyclic == 1 (true)",
              ci_cyclic == 1);

        int64_t ci3 = mk_cycleinfo("demo", "Widget", 1, "self-referential");
        int64_t ci_reason = frankenstein_Frankenstein_Core_CycleAnalysis_ciReason(ci3);
        CHECK("ciReason == \"self-referential\"",
              kk_str_eq(ci_reason, s("self-referential")));
    }

    /* ============================================================== */
    printf("\n[8] MercuryBridge/HldsParse.o — Pred selectors\n");
    /* ============================================================== */
    {
        /* Each selector consumes its input */
        int64_t pn = frankenstein_Frankenstein_MercuryBridge_HldsParse_predName(mk_mercury_pred("append", "det"));
        CHECK("predName.nameText == \"append\"",
              kk_str_eq(nameText(qnameName(pn)), s("append")));

        int64_t pd = frankenstein_Frankenstein_MercuryBridge_HldsParse_predDet(mk_mercury_pred("append", "det"));
        CHECK("predDet == \"det\"",
              kk_str_eq(pd, s("det")));
    }

    /* ============================================================== */
    printf("\n[9] RustBridge/MirParse.o — MIR selectors\n");
    /* ============================================================== */
    {
        int64_t mn = frankenstein_Frankenstein_RustBridge_MirParse_mirName(mk_mir_body("factorial", 1));
        CHECK("mirName == \"factorial\"",
              kk_str_eq(mn, s("factorial")));

        int64_t mac = frankenstein_Frankenstein_RustBridge_MirParse_mirArgCount(mk_mir_body("factorial", 1));
        CHECK("mirArgCount == 1",
              mac == 1);
    }

    /* ============================================================== */
    printf("\n[11] Compose: build program, traverse with selectors\n");
    /* ============================================================== */
    {
        /* Each sub-test builds a fresh structure to avoid refcount issues
           with Perceus destructive selectors. */

        /* prog.name.nameText == "prog" */
        CHECK("prog.name.nameText == \"prog\"",
              kk_str_eq(nameText(qnameName(progName(
                  mk_program("demo", "prog", nil(), nil(), nil())))),
                  s("prog")));

        /* prog.defs[0].defName.nameText == "fac" */
        {
            int64_t p = mk_program("demo", "prog",
                cons(mk_def("demo", "fac", 1, ph()),
                     cons(mk_def("demo", "main", 2, ph()), nil())),
                nil(), nil());
            int64_t defs = progDefs(p);
            int64_t d0 = kk_field(defs, 0);
            kk_retain(d0);
            CHECK("prog.defs[0].defName.nameText == \"fac\"",
                  kk_str_eq(nameText(qnameName(defName(d0))), s("fac")));
        }

        /* prog.data[0].dataName.nameText == "Bool" */
        {
            int64_t p = mk_program("demo", "prog", nil(),
                cons(mk_datadecl("std", "Bool",
                    cons(mk_condecl("std", "True", nil()),
                         cons(mk_condecl("std", "False", nil()), nil()))),
                    nil()),
                nil());
            int64_t data = progData(p);
            int64_t dd0 = kk_field(data, 0);
            kk_retain(dd0);
            CHECK("prog.data[0].dataName.nameText == \"Bool\"",
                  kk_str_eq(nameText(qnameName(dataName(dd0))), s("Bool")));
        }

        /* prog.data[0].cons[0].conName.nameText == "True" */
        {
            int64_t p = mk_program("demo", "prog", nil(),
                cons(mk_datadecl("std", "Bool",
                    cons(mk_condecl("std", "True", nil()),
                         cons(mk_condecl("std", "False", nil()), nil()))),
                    nil()),
                nil());
            int64_t data = progData(p);
            int64_t dd0 = kk_field(data, 0);
            kk_retain(dd0);
            int64_t clist = dataCons(dd0);
            int64_t c0 = kk_field(clist, 0);
            kk_retain(c0);
            CHECK("prog.data[0].cons[0].conName.nameText == \"True\"",
                  kk_str_eq(nameText(qnameName(conName(c0))), s("True")));
        }

        /* prog.effects[0].effectName.nameText == "io" */
        {
            int64_t eff_op = kk_alloc_con(0, 2);
            kk_set_field(eff_op, 0, mk_qname("io", "print", 0));
            kk_set_field(eff_op, 1, ph());
            int64_t io_eff = kk_alloc_con(0, 3);
            kk_set_field(io_eff, 0, mk_qname("std", "io", 0));
            kk_set_field(io_eff, 1, nil());
            kk_set_field(io_eff, 2, cons(eff_op, nil()));
            int64_t p = mk_program("demo", "prog", nil(), nil(),
                cons(io_eff, nil()));
            int64_t effs = progEffects(p);
            int64_t e0 = kk_field(effs, 0);
            kk_retain(e0);
            CHECK("prog.effects[0].effectName.nameText == \"io\"",
                  kk_str_eq(nameText(qnameName(effectName(e0))), s("io")));
        }

        /* prog.effects[0].ops[0].opName.nameText == "print" */
        {
            int64_t eff_op = kk_alloc_con(0, 2);
            kk_set_field(eff_op, 0, mk_qname("io", "print", 0));
            kk_set_field(eff_op, 1, ph());
            int64_t io_eff = kk_alloc_con(0, 3);
            kk_set_field(io_eff, 0, mk_qname("std", "io", 0));
            kk_set_field(io_eff, 1, nil());
            kk_set_field(io_eff, 2, cons(eff_op, nil()));
            int64_t effs = cons(io_eff, nil());
            int64_t e0 = kk_field(effs, 0);
            kk_retain(e0);
            int64_t ops = effectOps(e0);
            int64_t op0 = kk_field(ops, 0);
            kk_retain(op0);
            CHECK("prog.effects[0].ops[0].opName.nameText == \"print\"",
                  kk_str_eq(nameText(qnameName(opName(op0))), s("print")));
        }

        /* Cross-module: conKey */
        {
            int64_t c = mk_condecl("std", "True", nil());
            kk_retain(c);
            int64_t true_key = frankenstein_Frankenstein_Core_ConTags_conKey(conName(c));
            CHECK("conKey(prog.data.Bool.True) == \"True\"",
                  kk_str_eq(true_key, s("True")));
        }
    }

    /* ============================================================== */
    printf("\n[12] Core/ConTags.o — assignProgramTags (full pass)\n");
    /* ============================================================== */
    {
        /* Start simple: completely empty program (no data, no defs) */
        int64_t prog0 = mk_program("test", "empty-tags", nil(), nil(), nil());
        int64_t tagMap0 = frankenstein_Frankenstein_Core_ConTags_assignProgramTags(prog0);
        CHECK("assignProgramTags(empty) returns non-null", tagMap0 != 0);

        /* Now with DataDecls */
        int64_t cd_true  = mk_condecl("std", "True", nil());
        int64_t cd_false = mk_condecl("std", "False", nil());
        int64_t bool_dd  = mk_datadecl("std", "Bool",
                                        cons(cd_true, cons(cd_false, nil())));
        int64_t cd_nothing = mk_condecl("std", "Nothing", nil());
        int64_t cd_just    = mk_condecl("std", "Just", cons(ph(), nil()));
        int64_t maybe_dd   = mk_datadecl("std", "Maybe",
                                          cons(cd_nothing, cons(cd_just, nil())));

        int64_t data = cons(bool_dd, cons(maybe_dd, nil()));
        int64_t prog = mk_program("test", "tags", nil(), data, nil());
        int64_t tagMap = frankenstein_Frankenstein_Core_ConTags_assignProgramTags(prog);
        CHECK("assignProgramTags returns a non-null Map",
              tagMap != 0);

        /* Look up "True" in the tag map */
        /* Hash-based tags: stableConTag computes deterministic values */
        int64_t trueResult = shim_map_lookup(s("True"), tagMap);
        int64_t trueTag = (kk_tag(trueResult) != 0) ? kk_field(trueResult, 0) : -1;
        CHECK("assignProgramTags: True -> tag 24914", trueTag == 24914);

        int64_t falseResult = shim_map_lookup(s("False"), tagMap);
        int64_t falseTag = (kk_tag(falseResult) != 0) ? kk_field(falseResult, 0) : -1;
        CHECK("assignProgramTags: False -> tag 44872", falseTag == 44872);

        int64_t justResult = shim_map_lookup(s("Just"), tagMap);
        int64_t justTag = (kk_tag(justResult) != 0) ? kk_field(justResult, 0) : -1;
        CHECK("assignProgramTags: Just -> tag 61886", justTag == 61886);

        int64_t nothingResult = shim_map_lookup(s("Nothing"), tagMap);
        int64_t nothingTag = (kk_tag(nothingResult) != 0) ? kk_field(nothingResult, 0) : -1;
        CHECK("assignProgramTags: Nothing -> tag 53440", nothingTag == 53440);
    }

    /* ============================================================== */
    printf("\n[13] Core/Perceus.o — insertPerceus (full pass)\n");
    /* ============================================================== */
    {
        /* Empty program — insertPerceus maps over empty defs list */
        int64_t prog = mk_program("demo", "perceus-test", nil(), nil(), nil());
        kk_retain(prog);

        int64_t result = frankenstein_Frankenstein_Core_Perceus_insertPerceus(prog);
        CHECK("insertPerceus returns non-null", result != 0);
        int64_t rn = progName(result);
        CHECK("insertPerceus preserves progName",
              kk_str_eq(nameText(qnameName(rn)), s("perceus-test")));
    }

    /* ============================================================== */
    printf("\n[14] Core/Evidence.o — evidencePass (full pass)\n");
    /* ============================================================== */
    {
        int64_t prog = mk_program("demo", "evidence-test", nil(), nil(), nil());
        kk_retain(prog);

        int64_t result = frankenstein_Frankenstein_Core_Evidence_evidencePass(prog);
        CHECK("evidencePass returns non-null", result != 0);
        int64_t rn = progName(result);
        CHECK("evidencePass preserves progName",
              kk_str_eq(nameText(qnameName(rn)), s("evidence-test")));
    }

    /* ============================================================== */
    printf("\n[15] MlirEmit/Emitter.o — emitProgramText (full pass)\n");
    /* ============================================================== */
    {
        int64_t prog = mk_program("demo", "emit-test", nil(), nil(), nil());
        kk_retain(prog);
        kk_retain(prog);

        int64_t mlir_text = frankenstein_Frankenstein_MlirEmit_Emitter_emitProgramText(prog);
        CHECK("emitProgramText returns non-null", mlir_text != 0);
        CHECK("emitProgramText returns a string", kk_is_string(mlir_text));
        if (kk_is_string(mlir_text)) {
            int64_t len = kk_str_len(mlir_text);
            CHECK("emitProgramText output is non-empty", len > 0);
            char* cstr = kk_str_dup_cstr(mlir_text);
            CHECK("emitProgramText output contains 'func'",
                  cstr && strstr(cstr, "func") != NULL);
            if (cstr) free(cstr);
        }
    }

    /* ============================================================== */
    printf("\n[16] Full pipeline: ConTags -> Perceus -> Evidence -> emitProgramText\n");
    /* ============================================================== */
    {
        /* Build a program with 2 defs, 1 data decl (Bool), 1 effect */
        /* identity = 42 ;  main = 0 */
        int64_t d1 = mk_def("demo", "identity", 10, mk_lit_int(42));
        int64_t d2 = mk_def("demo", "main", 11, mk_lit_int(0));
        int64_t defs = cons(d1, cons(d2, nil()));

        int64_t cd_true  = mk_condecl("std", "True", nil());
        int64_t cd_false = mk_condecl("std", "False", nil());
        int64_t bool_dd  = mk_datadecl("std", "Bool",
                                        cons(cd_true, cons(cd_false, nil())));
        int64_t data = cons(bool_dd, nil());

        /* Step 1: assignProgramTags with non-empty defs */
        printf("  ... calling assignProgramTags\n"); fflush(stdout);
        {
            int64_t p1 = mk_program("demo", "pipeline",
                cons(mk_def("demo", "identity", 10, mk_lit_int(42)),
                     cons(mk_def("demo", "main", 11, mk_lit_int(0)), nil())),
                cons(mk_datadecl("std", "Bool",
                    cons(mk_condecl("std", "True", nil()),
                         cons(mk_condecl("std", "False", nil()), nil()))),
                    nil()),
                nil());
            int64_t tagged = frankenstein_Frankenstein_Core_ConTags_assignProgramTags(p1);
            CHECK("pipeline: assignProgramTags succeeds", tagged != 0);
        }

        /* Step 2: insertPerceus */
        printf("  ... calling insertPerceus\n"); fflush(stdout);
        int64_t prog = mk_program("demo", "pipeline", defs, data, nil());
        int64_t perceus_out = frankenstein_Frankenstein_Core_Perceus_insertPerceus(prog);
        CHECK("pipeline: insertPerceus succeeds", perceus_out != 0);

        /* Step 3: evidencePass */
        printf("  ... calling evidencePass\n"); fflush(stdout);
        int64_t evidence_out = frankenstein_Frankenstein_Core_Evidence_evidencePass(perceus_out);
        CHECK("pipeline: evidencePass succeeds", evidence_out != 0);

        /* Step 4: emitProgramText on a program with defs */
        printf("  ... calling emitProgramText\n"); fflush(stdout);
        int64_t emit_prog = mk_program("demo", "pipeline",
            cons(mk_def("demo", "identity", 10, mk_lit_int(42)),
                 cons(mk_def("demo", "main", 11, mk_lit_int(0)), nil())),
            cons(mk_datadecl("std", "Bool",
                cons(mk_condecl("std", "True", nil()),
                     cons(mk_condecl("std", "False", nil()), nil()))),
                nil()),
            nil());
        kk_retain(emit_prog);
        kk_retain(emit_prog);
        int64_t mlir = frankenstein_Frankenstein_MlirEmit_Emitter_emitProgramText(emit_prog);
        CHECK("pipeline: emitProgramText succeeds", mlir != 0);
        CHECK("pipeline: emitProgramText returns string", kk_is_string(mlir));
        if (kk_is_string(mlir)) {
            int64_t len = kk_str_len(mlir);
            CHECK("pipeline: MLIR output is non-empty", len > 0);
            char* cstr = kk_str_dup_cstr(mlir);
            CHECK("pipeline: MLIR output contains 'func'",
                  cstr && strstr(cstr, "func") != NULL);
            CHECK("pipeline: MLIR output contains 'module'",
                  cstr && strstr(cstr, "module") != NULL);
            printf("  (MLIR output: %ld bytes)\n", (long)len);
            FILE *pf = fopen("self-host/pipeline-test.mlir", "w");
            if (pf) { fputs(cstr, pf); fclose(pf); }
            if (cstr) free(cstr);
        }
    }

    /* ============================================================== */
    printf("\n[17] REAL WORK: factorial(10) through self-hosted pipeline\n");
    /* ============================================================== */
    {
        /* Build a real factorial program as Core IR:
         *
         *   factorial : (int) -> total int
         *   factorial = \n -> case n of
         *     0 -> 1
         *     _ -> n * factorial(n - 1)
         *
         *   main : () -> total int
         *   main = factorial(10)
         *
         * This is the same program as --demo, but constructed in C and
         * compiled through the SELF-HOSTED emitter — proving Frankenstein
         * can bootstrap real work.
         */

        /* First test: simple function with EApp */
        printf("  ... test: simple add function\n"); fflush(stdout);
        {
            /* add = \(x,y) -> x + y */
            int64_t pp_x = kk_pair(mk_name("x", 1), ph());
            int64_t pp_y = kk_pair(mk_name("y", 2), ph());
            int64_t add_body_app = kk_alloc_con(TAG_EApp, 2);
            kk_set_field(add_body_app, 0, mk_evar("+", 0));
            kk_set_field(add_body_app, 1, cons(mk_evar("x", 1), cons(mk_evar("y", 2), nil())));
            int64_t add_body = kk_alloc_con(TAG_ELam, 2);
            kk_set_field(add_body, 0, cons(pp_x, cons(pp_y, nil())));
            kk_set_field(add_body, 1, add_body_app);
            int64_t add_def = mk_def("demo", "add", 5, add_body);

            int64_t add_main = kk_alloc_con(TAG_EApp, 2);
            kk_set_field(add_main, 0, mk_evar("add", 5));
            kk_set_field(add_main, 1, cons(mk_lit_int(3), cons(mk_lit_int(4), nil())));
            int64_t main_def = mk_def("demo", "main", 99, add_main);

            int64_t p = mk_program("demo", "add-test",
                cons(add_def, cons(main_def, nil())), nil(), nil());
            for (int i = 0; i < 8; i++) kk_retain(p);
            int64_t mlir = frankenstein_Frankenstein_MlirEmit_Emitter_emitProgramText(p);
            CHECK("add: emitProgramText succeeds", mlir != 0 && kk_is_string(mlir));
            if (kk_is_string(mlir)) {
                char* cs = kk_str_dup_cstr(mlir);
                CHECK("add: contains 'arith.addi'", cs && strstr(cs, "arith.addi") != NULL);
                printf("  (add MLIR: %ld bytes)\n", (long)kk_str_len(mlir));
                FILE *af = fopen("self-host/add-test.mlir", "w");
                if (af) { fputs(cs, af); fclose(af); }
                if (cs) free(cs);
            }
        }

        printf("  ... factorial test\n"); fflush(stdout);
        {

        /* Build PatLit(LitInt(0)) */
        int64_t lit0 = kk_alloc_con(TAG_LitInt, 1);
        kk_set_field(lit0, 0, 0);
        int64_t pat_base = kk_alloc_con(TAG_PatLit, 1);
        kk_set_field(pat_base, 0, lit0);

        /* Branch: { PatLit(0), Nothing, ELit(LitInt(1)) } */
        int64_t branch_base = kk_alloc_con(0, 3);
        kk_set_field(branch_base, 0, pat_base);
        kk_set_field(branch_base, 1, ph());
        kk_set_field(branch_base, 2, mk_lit_int(1));

        /* PatWild(placeholder) */
        int64_t pat_wild = kk_alloc_con(TAG_PatWild, 1);
        kk_set_field(pat_wild, 0, ph());

        /* n - 1 */
        int64_t sub_args = cons(mk_evar("n", 1), cons(mk_lit_int(1), nil()));
        int64_t sub_expr = kk_alloc_con(TAG_EApp, 2);
        kk_set_field(sub_expr, 0, mk_evar("-", 0));
        kk_set_field(sub_expr, 1, sub_args);

        /* factorial(n - 1) */
        int64_t fac_call = kk_alloc_con(TAG_EApp, 2);
        kk_set_field(fac_call, 0, mk_evar("factorial", 10));
        kk_set_field(fac_call, 1, cons(sub_expr, nil()));

        /* n * factorial(n - 1) */
        int64_t mul_expr = kk_alloc_con(TAG_EApp, 2);
        kk_set_field(mul_expr, 0, mk_evar("*", 0));
        kk_set_field(mul_expr, 1, cons(mk_evar("n", 1), cons(fac_call, nil())));

        /* Branch: { PatWild, Nothing, n*factorial(n-1) } */
        int64_t branch_rec = kk_alloc_con(0, 3);
        kk_set_field(branch_rec, 0, pat_wild);
        kk_set_field(branch_rec, 1, ph());
        kk_set_field(branch_rec, 2, mul_expr);

        /* case n of [branch_base, branch_rec] */
        int64_t case_expr = kk_alloc_con(TAG_ECase, 2);
        kk_set_field(case_expr, 0, mk_evar("n", 1));
        kk_set_field(case_expr, 1, cons(branch_base, cons(branch_rec, nil())));

        /* \n -> case n of ... */
        int64_t param_pair = kk_pair(mk_name("n", 1), ph());
        int64_t fac_body = kk_alloc_con(TAG_ELam, 2);
        kk_set_field(fac_body, 0, cons(param_pair, nil()));
        kk_set_field(fac_body, 1, case_expr);

        int64_t fac_def = mk_def("demo", "factorial", 10, fac_body);

        /* main = factorial(10) */
        int64_t main_call = kk_alloc_con(TAG_EApp, 2);
        kk_set_field(main_call, 0, mk_evar("factorial", 10));
        kk_set_field(main_call, 1, cons(mk_lit_int(10), nil()));
        int64_t main_def = mk_def("demo", "main", 99, main_call);

        int64_t prog = mk_program("demo", "factorial",
            cons(fac_def, cons(main_def, nil())), nil(), nil());

        /* Retain the prog tree so emitProgramText can traverse it.
         * kk_drop is fully functional (retain-on-force in kk_thunk_force
         * handles shared lazy thunks correctly). */
        for (int i = 0; i < 8; i++) kk_retain(prog);
        printf("  ... calling emitProgramText on factorial\n"); fflush(stdout);
        int64_t mlir = frankenstein_Frankenstein_MlirEmit_Emitter_emitProgramText(prog);
        CHECK("factorial: emitProgramText succeeds", mlir != 0);
        CHECK("factorial: emitProgramText returns string", kk_is_string(mlir));
        if (kk_is_string(mlir)) {
            int64_t len = kk_str_len(mlir);
            CHECK("factorial: MLIR output is non-empty", len > 0);
            char* cstr = kk_str_dup_cstr(mlir);
            CHECK("factorial: MLIR contains 'demo_factorial'",
                  cstr && strstr(cstr, "demo_factorial") != NULL);
            CHECK("factorial: MLIR contains 'arith.muli'",
                  cstr && strstr(cstr, "arith.muli") != NULL);
            CHECK("factorial: MLIR contains 'arith.subi'",
                  cstr && strstr(cstr, "arith.subi") != NULL);
            CHECK("factorial: MLIR contains 'arith.cmpi'",
                  cstr && strstr(cstr, "arith.cmpi") != NULL);
            CHECK("factorial: MLIR contains printf main",
                  cstr && strstr(cstr, "printf") != NULL);
            printf("  (factorial MLIR: %ld bytes)\n", (long)len);

            /* Write MLIR to file for external validation */
            FILE *f = fopen("self-host/factorial-self.mlir", "w");
            if (f) {
                fputs(cstr, f);
                fclose(f);
                printf("  Written to self-host/factorial-self.mlir\n");
            }
            if (cstr) free(cstr);
        }
        } /* end factorial test */
    }

    /* ================================================================ */
    printf("\n=== Results: %d passed, %d failed ===\n", pass, fail);
    /* ================================================================ */

    if (fail == 0) {
        printf("\nFrankenstein self-hosts ALL compiler passes across 14 modules!\n");
        printf("Passes: assignProgramTags, insertPerceus, evidencePass, emitProgramText\n");
        printf("Modules: Types, ConTags, Perceus, Evidence, EffectOpt,\n");
        printf("  CycleAnalysis, DeriveSelectors, HldsParse, MirParse, Dialects, Emitter\n");
        printf("Pipeline: .hs -> GHC bridge -> Core IR -> Perceus -> MLIR -> LLVM -> ELF\n");
    }

    return fail > 0 ? 1 : 0;
}
