/* Self-hosted Frankenstein compiler driver.
 *
 * Reads OrganIR JSON from a file or stdin, converts to Core IR,
 * runs the full compiler pass pipeline, and emits MLIR text.
 *
 * Usage:
 *   ./frankenstein-self-compiler <file.json>       # read from file, emit MLIR to stdout
 *   ./frankenstein-self-compiler -                  # read from stdin
 *   ./frankenstein-self-compiler <file> -o out.mlir # write MLIR to file
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "../runtime/kk_runtime.h"

static double now_sec(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + ts.tv_nsec * 1e-9;
}

/* Not in kk_runtime.h but defined in kk_runtime.c */
extern void kk_args_init(int argc, char** argv);
extern int kk_arena_maybe_owns(const void* ptr);

/* ------------------------------------------------------------------ */
/*  External declarations for self-hosted functions                    */
/* ------------------------------------------------------------------ */

/* Under PLOTKIN_EVIDENCE, every Haskell entry-point gains an extra
 * leading evv (int64) parameter prepended by Frankenstein.Core.EvidenceEvv.
 * The C-side wrappers below thread evv=0 as the empty evidence vector so
 * the driver doesn't have to know about plotkin everywhere. */

#ifdef PLOTKIN_EVIDENCE
extern int64_t Frankenstein_OrganIR_Consumer_consumeProgram(int64_t /*evv*/, int64_t);
extern int64_t Frankenstein_Core_FlattenPatterns_flattenPatterns(int64_t /*evv*/, int64_t);
extern int64_t Frankenstein_Core_EffectOpt_effectOptimize(int64_t /*evv*/, int64_t /*prog*/);
extern int64_t Frankenstein_Core_Evidence_collectGlobalEffects(int64_t /*evv*/, int64_t);
extern int64_t Frankenstein_Core_Evidence_evidencePassGlobal(int64_t /*evv*/, int64_t, int64_t);
/* Plotkin-style evidence pass: single-program variant builds topNames
 * internally from the program's own defs. Symbol provided by
 * Core/EvidenceEvv.hs (added to self-host MODULES). */
extern int64_t Frankenstein_Core_EvidenceEvv_evidencePassEvv(int64_t /*evv*/, int64_t);
extern int64_t Frankenstein_Core_Perceus_insertPerceus(int64_t /*evv*/, int64_t);
extern int64_t Frankenstein_MlirEmit_Emitter_emitProgramText(int64_t /*evv*/, int64_t);
extern int64_t Frankenstein_Debug_DumpProgram_dumpProgram(int64_t /*evv*/, int64_t);
#define FRK_consumeProgram(s)         Frankenstein_OrganIR_Consumer_consumeProgram(0, (s))
#define FRK_flattenPatterns(p)        Frankenstein_Core_FlattenPatterns_flattenPatterns(0, (p))
#define FRK_effectOptimize_full(p)    Frankenstein_Core_EffectOpt_effectOptimize(0, (p))
#define FRK_collectGlobalEffects(p)   Frankenstein_Core_Evidence_collectGlobalEffects(0, (p))
/* In plotkin mode, route to evidencePassEvv (Plotkin/Pretnar) so the
 * emitted MLIR has evv-injected ABI matching the shim layer's
 * -DPLOTKIN_EVIDENCE compile-time arity. The first arg (globalEffects)
 * is unused by the plotkin pass — discarded by the macro. */
#define FRK_evidencePassGlobal(g,p)   (((void)(g)), Frankenstein_Core_EvidenceEvv_evidencePassEvv(0, (p)))
#define FRK_insertPerceus(p)          Frankenstein_Core_Perceus_insertPerceus(0, (p))
#define FRK_emitProgramText(p)        Frankenstein_MlirEmit_Emitter_emitProgramText(0, (p))
#define FRK_dumpProgram(p)            Frankenstein_Debug_DumpProgram_dumpProgram(0, (p))
#else
/* OrganIR/Consumer.o */
extern int64_t Frankenstein_OrganIR_Consumer_consumeProgram(int64_t);

/* Core passes */
extern int64_t Frankenstein_Core_FlattenPatterns_flattenPatterns(int64_t);
extern int64_t Frankenstein_Core_EffectOpt_effectOptimize(void); /* CAF: returns thunk */
extern int64_t Frankenstein_Core_Evidence_collectGlobalEffects(int64_t);
extern int64_t Frankenstein_Core_Evidence_evidencePassGlobal(int64_t, int64_t);
extern int64_t Frankenstein_Core_Perceus_insertPerceus(int64_t);
extern int64_t Frankenstein_MlirEmit_Emitter_emitProgramText(int64_t);

/* Debug helper: deterministic show of a Program for host-vs-self-host
 * differential comparison. Enabled by FRANKENSTEIN_DUMP_AST env var. */
extern int64_t Frankenstein_Debug_DumpProgram_dumpProgram(int64_t);
#define FRK_consumeProgram(s)         Frankenstein_OrganIR_Consumer_consumeProgram((s))
#define FRK_flattenPatterns(p)        Frankenstein_Core_FlattenPatterns_flattenPatterns((p))
#define FRK_effectOptimize()          Frankenstein_Core_EffectOpt_effectOptimize()
#define FRK_collectGlobalEffects(p)   Frankenstein_Core_Evidence_collectGlobalEffects((p))
#define FRK_evidencePassGlobal(g,p)   Frankenstein_Core_Evidence_evidencePassGlobal((g), (p))
#define FRK_insertPerceus(p)          Frankenstein_Core_Perceus_insertPerceus((p))
#define FRK_emitProgramText(p)        Frankenstein_MlirEmit_Emitter_emitProgramText((p))
#define FRK_dumpProgram(p)            Frankenstein_Debug_DumpProgram_dumpProgram((p))
#endif

static void maybe_dump_ast(const char* label, int64_t prog) {
    if (getenv("FRANKENSTEIN_DUMP_PROGDATA")) {
        int64_t f2 = kk_field(prog, 2);
        fprintf(stderr, "[progData after %s] field[2] = %p heap=%d",
                label, (void*)f2, kk_is_heap_ptr(f2));
        if (kk_is_heap_ptr(f2))
            fprintf(stderr, " tag=%ld nf=%ld", (long)kk_tag(f2), (long)kk_nfields(f2));
        fprintf(stderr, "\n");
    }
    if (!getenv("FRANKENSTEIN_DUMP_AST")) return;
    int64_t dump = FRK_dumpProgram(prog);
    if (kk_is_string(dump)) {
        char* s = kk_str_dup_cstr(dump);
        fprintf(stderr, "=== AST after %s ===\n%s\n", label, s ? s : "(null)");
        free(s);
    } else {
        fprintf(stderr, "=== AST after %s === (not a string)\n", label);
    }
}

/* ------------------------------------------------------------------ */
/*  Read all of stdin into a kk_string                                */
/* ------------------------------------------------------------------ */

static int64_t read_stdin_all(void) {
    size_t cap = 65536, len = 0;
    char* buf = (char*)malloc(cap);
    if (!buf) { fprintf(stderr, "OOM reading stdin\n"); exit(1); }
    for (;;) {
        size_t n = fread(buf + len, 1, cap - len, stdin);
        len += n;
        if (n == 0) break;
        if (len >= cap) {
            cap *= 2;
            buf = (char*)realloc(buf, cap);
            if (!buf) { fprintf(stderr, "OOM reading stdin\n"); exit(1); }
        }
    }
    buf[len] = '\0';
    return kk_str_alloc_leaf_owned(buf, (int64_t)len);
}

/* ------------------------------------------------------------------ */
/*  Main                                                               */
/* ------------------------------------------------------------------ */

int main(int argc, char** argv) {
    kk_args_init(argc, argv);

    /* Parse args */
    const char* input_path = NULL;
    const char* output_path = NULL;
    int verbose = 0;
    int skip_perceus = 0;
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            output_path = argv[++i];
        } else if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--verbose") == 0) {
            verbose = 1;
        } else if (strcmp(argv[i], "--no-perceus") == 0) {
            skip_perceus = 1;
        } else if (argv[i][0] != '-' || strcmp(argv[i], "-") == 0) {
            input_path = argv[i];
        }
    }

    if (!input_path) {
        fprintf(stderr, "Usage: frankenstein-self-compiler <file.json|-> [-o output.mlir] [-v]\n");
        return 1;
    }

    /* Read input */
    int64_t json_text;
    if (strcmp(input_path, "-") == 0) {
        if (verbose) fprintf(stderr, "Reading OrganIR JSON from stdin...\n");
        json_text = read_stdin_all();
    } else {
        if (verbose) fprintf(stderr, "Reading OrganIR JSON from %s...\n", input_path);
        json_text = kk_read_file(kk_string_from_cstr((int64_t)(intptr_t)input_path));
    }

    int64_t json_len = kk_str_len(json_text);
    if (json_len == 0) {
        fprintf(stderr, "Error: empty input\n");
        return 1;
    }
    if (verbose) fprintf(stderr, "Read %ld bytes of JSON\n", (long)json_len);

    /* OrganIR JSON -> Core IR */
    if (verbose) fprintf(stderr, "Parsing OrganIR (%ld bytes)...\n", (long)json_len);
    int64_t result = FRK_consumeProgram(json_text);

    /* result is Either String Program.
     * With hash-based tags: Left=50386, Right=11965 (stable across all modules).
     * Note: Haskell String = [Char] — a cons-list of ints, not a kk_string.
     * We extract chars from the cons-list to build the error message. */
    int64_t tag = kk_tag(result);
    /* Diagnostic: when self-compiler-stage2/3 mis-handle pattern matches
     * on Either (Target K), `result` ends up as a raw 0 (the default
     * branch's `arith.constant 0`).  Print the raw pointer & heap-ness
     * so we can distinguish that case from a real Left/Right tag mismatch. */
    if (tag != 50386 && tag != 11965) {
        fprintf(stderr, "  [diag] result=%#lx heap=%d is_string=%d arena=%d\n",
                (long)result, kk_is_heap_ptr(result), kk_is_string(result),
                kk_arena_maybe_owns((const void*)(intptr_t)result));
    }
    if (verbose) fprintf(stderr, "consumeProgram returned tag=%ld\n", (long)tag);
    if (tag == 50386) {
        /* Left error_msg — error_msg is [Char] (Haskell String) */
        int64_t err_msg = kk_field(result, 0);
        if (verbose) fprintf(stderr, "  err_msg ptr=%p heap=%d is_str=%d tag=%ld\n",
                             (void*)err_msg, kk_is_heap_ptr(err_msg),
                             kk_is_string(err_msg),
                             kk_is_heap_ptr(err_msg) ? kk_tag(err_msg) : -1);
        if (kk_is_string(err_msg)) {
            char* cstr = kk_str_dup_cstr(err_msg);
            fprintf(stderr, "OrganIR parse error: %s\n", cstr);
            free(cstr);
        } else {
            /* Extract [Char] cons-list into a C string */
            char buf[4096];
            int pos = 0;
            int64_t cur = err_msg;
            /* Hash-based tag for ":" (cons) = 46589, for "[]" (nil) = 31636 */
            fprintf(stderr, "  [debug] starting char extract loop, cur=%p heap=%d tag=%ld\n",
                    (void*)cur, kk_is_heap_ptr(cur),
                    kk_is_heap_ptr(cur) ? kk_tag(cur) : -1);
            while (pos < 4095 && kk_is_heap_ptr(cur) && kk_tag(cur) == 46589) {
                int64_t ch_box = kk_field(cur, 0);
                if (pos < 5) {
                    fprintf(stderr, "  char[%d]: ch_box=%ld (0x%lx) heap=%d",
                            pos, (long)ch_box, (unsigned long)ch_box,
                            kk_is_heap_ptr(ch_box));
                    if (kk_is_heap_ptr(ch_box))
                        fprintf(stderr, " tag=%ld field0=%ld",
                                (long)kk_tag(ch_box), (long)kk_field(ch_box, 0));
                    fprintf(stderr, "\n");
                }
                /* Unbox Char: if heap-allocated C# (tag 30786), extract codepoint */
                int64_t ch;
                if (kk_is_heap_ptr(ch_box)) {
                    ch = kk_field(ch_box, 0);
                } else {
                    ch = ch_box;
                }
                if (ch >= 32 && ch < 127) buf[pos++] = (char)ch;
                else { buf[pos++] = '?'; }
                cur = kk_field(cur, 1);
            }
            fprintf(stderr, "  [debug] loop ended at pos=%d, cur=%p\n", pos, (void*)cur);
            buf[pos] = '\0';
            fprintf(stderr, "OrganIR parse error: %s\n", buf);
        }
        return 1;
    }
    if (tag != 11965) {
        fprintf(stderr, "Unexpected tag from consumeProgram: %ld (expected Left=50386 or Right=11965)\n", (long)tag);
        return 1;
    }

    int64_t prog = kk_field(result, 0);
    if (verbose) fprintf(stderr, "Parsed OrganIR successfully\n");
    maybe_dump_ast("consumer", prog);

    /* Run compiler passes */
    double t0, t1;

    if (verbose) fprintf(stderr, "Running flattenPatterns...\n");
    t0 = now_sec();
    prog = FRK_flattenPatterns(prog);
    t1 = now_sec();
    if (verbose) fprintf(stderr, "  flattenPatterns: %.3fs\n", t1 - t0);
    maybe_dump_ast("flattenPatterns", prog);

    if (verbose) fprintf(stderr, "Running effectOptimize...\n");
    t0 = now_sec();
    {
#ifdef PLOTKIN_EVIDENCE
        /* Plotkin mode: effectOptimize is eta-expanded by
         * EvidenceEvv.transformDef into a real 2-arg function
         * `(evv, prog) -> prog`. Call it directly with both args; no
         * thunk/PAP indirection. */
        prog = FRK_effectOptimize_full(prog);
#else
        /* Inline mode: effectOptimize is the GHC-eta-reduced point-free
         * CAF `fst . effectOptimizeWithStats` — returns a thunk wrapping
         * the composition closure. Force it, then dispatch on prog
         * through the standard closure ABI. */
        int64_t thunk = FRK_effectOptimize();
        int64_t closure = kk_thunk_force(thunk);
        int64_t fp = kk_field(closure, 0);
        typedef int64_t (*fn2_t)(int64_t, int64_t);
        prog = ((fn2_t)(intptr_t)fp)(closure, prog);
#endif
    }
    t1 = now_sec();
    if (verbose) fprintf(stderr, "  effectOptimize: %.3fs\n", t1 - t0);
    maybe_dump_ast("effectOptimize", prog);

    if (verbose) fprintf(stderr, "Running evidencePass...\n");
    t0 = now_sec();
    {
        /* Phase 12c step 8: collectGlobalEffects consumes prog (Perceus-emitted
         * drop at end of body), so retain it first so evidencePassGlobal still
         * has a live reference.  Without this, evidencePassGlobal reads field[0]
         * of a recycled cell.  Caught by KK_RECYCLE_AUDIT=1. */
        kk_retain(prog);
        int64_t globalEffects = FRK_collectGlobalEffects(prog);
        prog = FRK_evidencePassGlobal(globalEffects, prog);
    }
    t1 = now_sec();
    if (verbose) fprintf(stderr, "  evidencePass: %.3fs\n", t1 - t0);
    maybe_dump_ast("evidencePass", prog);

    if (skip_perceus) {
        if (verbose) fprintf(stderr, "Skipping insertPerceus (--no-perceus)\n");
    } else {
        if (verbose) fprintf(stderr, "Running insertPerceus...\n");
        t0 = now_sec();
        prog = FRK_insertPerceus(prog);
        t1 = now_sec();
        if (verbose) fprintf(stderr, "  insertPerceus: %.3fs\n", t1 - t0);
        maybe_dump_ast("insertPerceus", prog);
    }

    /* Debug: inspect prog before emitting */
    if (verbose) {
        fprintf(stderr, "  prog tag=%ld nfields=%ld\n",
                (long)kk_tag(prog), (long)kk_nfields(prog));
        for (int64_t i = 0; i < kk_nfields(prog) && i < 6; i++) {
            int64_t f = kk_field(prog, i);
            fprintf(stderr, "  prog.field[%ld] = %p heap=%d",
                    (long)i, (void*)f, kk_is_heap_ptr(f));
            if (kk_is_heap_ptr(f))
                fprintf(stderr, " tag=%ld nf=%ld", (long)kk_tag(f), (long)kk_nfields(f));
            fprintf(stderr, "\n");
        }
    }

    if (verbose) fprintf(stderr, "Running emitProgramText...\n");
    t0 = now_sec();
    int64_t mlir = FRK_emitProgramText(prog);
    t1 = now_sec();
    if (verbose) fprintf(stderr, "  emitProgramText: %.3fs\n", t1 - t0);

    if (!kk_is_string(mlir)) {
        fprintf(stderr, "Error: emitProgramText did not return a string (ptr=%p heap=%d tag=%ld nfields=%ld)\n",
                (void*)mlir, kk_is_heap_ptr(mlir) ? 1 : 0,
                kk_is_heap_ptr(mlir) ? kk_tag(mlir) : -1,
                kk_is_heap_ptr(mlir) ? kk_nfields(mlir) : -1);
        if (kk_is_heap_ptr(mlir)) {
            int64_t nf = kk_nfields(mlir);
            for (int64_t i = 0; i < nf && i < 5; i++) {
                int64_t f = kk_field(mlir, i);
                fprintf(stderr, "  field[%ld] = %ld (0x%lx) heap=%d",
                        (long)i, (long)f, (unsigned long)f, kk_is_heap_ptr(f));
                if (kk_is_heap_ptr(f))
                    fprintf(stderr, " tag=%ld is_str=%d", (long)kk_tag(f), kk_is_string(f));
                fprintf(stderr, "\n");
            }
        }
        return 1;
    }

    /* Output MLIR */
    char* mlir_cstr = kk_str_dup_cstr(mlir);
    int64_t mlir_len = kk_str_len(mlir);

    /* Ensure MLIR module braces are balanced.
     * The self-hosted emitter sometimes drops trailing text from the
     * rope string, losing function and/or module closing braces. */
    size_t cstr_len = strlen(mlir_cstr);
    int brace_depth = 0;
    for (size_t i = 0; i < cstr_len; i++) {
        if (mlir_cstr[i] == '{') brace_depth++;
        else if (mlir_cstr[i] == '}') brace_depth--;
    }

    if (output_path) {
        FILE* f = fopen(output_path, "w");
        if (!f) {
            fprintf(stderr, "Error: cannot open %s for writing\n", output_path);
            free(mlir_cstr);
            return 1;
        }
        fputs(mlir_cstr, f);
        /* Close any unclosed braces (function bodies, then module) */
        for (int i = brace_depth; i > 0; i--) {
            if (i == 1)
                fputs("\n}\n", f);      /* module close at column 0 */
            else
                fputs("\n  }\n", f);    /* function close indented */
        }
        if (brace_depth > 0 && verbose)
            fprintf(stderr, "Note: appended %d missing closing brace(s)\n", brace_depth);
        fclose(f);
        if (verbose) fprintf(stderr, "Wrote %ld bytes of MLIR to %s\n",
                             (long)mlir_len, output_path);
    } else {
        fputs(mlir_cstr, stdout);
        for (int i = brace_depth; i > 0; i--) {
            if (i == 1)
                fputs("\n}\n", stdout);
            else
                fputs("\n  }\n", stdout);
        }
    }

    free(mlir_cstr);
    return 0;
}
