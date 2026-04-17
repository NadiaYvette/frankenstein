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
#include "../runtime/kk_runtime.h"

/* Not in kk_runtime.h but defined in kk_runtime.c */
extern void kk_args_init(int argc, char** argv);

/* ------------------------------------------------------------------ */
/*  External declarations for self-hosted functions                    */
/* ------------------------------------------------------------------ */

/* OrganIR/Consumer.o */
extern int64_t Frankenstein_OrganIR_Consumer_consumeProgram(int64_t);

/* Core passes */
extern int64_t Frankenstein_Core_FlattenPatterns_flattenPatterns(int64_t);
extern int64_t Frankenstein_Core_EffectOpt_effectOptimize(void); /* CAF: returns thunk */
extern int64_t Frankenstein_Core_Evidence_collectGlobalEffects(int64_t);
extern int64_t Frankenstein_Core_Evidence_evidencePassGlobal(int64_t, int64_t);
extern int64_t Frankenstein_Core_Perceus_insertPerceus(int64_t);
extern int64_t Frankenstein_MlirEmit_Emitter_emitProgramText(int64_t);

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
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            output_path = argv[++i];
        } else if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--verbose") == 0) {
            verbose = 1;
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
    if (verbose) fprintf(stderr, "Parsing OrganIR...\n");
    int64_t result = Frankenstein_OrganIR_Consumer_consumeProgram(json_text);

    /* result is Either String Program.
     * With hash-based tags: Left=50386, Right=11965 (stable across all modules). */
    int64_t tag = kk_tag(result);
    if (tag == 50386) {
        /* Left error_msg */
        int64_t err_msg = kk_field(result, 0);
        if (kk_is_string(err_msg)) {
            char* cstr = kk_str_dup_cstr(err_msg);
            fprintf(stderr, "OrganIR parse error: %s\n", cstr);
            free(cstr);
        } else {
            fprintf(stderr, "OrganIR parse error (non-string error value)\n");
        }
        return 1;
    }

    int64_t prog = kk_field(result, 0);
    if (verbose) fprintf(stderr, "Parsed OrganIR successfully\n");

    /* Run compiler passes */
    if (verbose) fprintf(stderr, "Running flattenPatterns...\n");
    prog = Frankenstein_Core_FlattenPatterns_flattenPatterns(prog);

    if (verbose) fprintf(stderr, "Running effectOptimize...\n");
    {
        /* effectOptimize is a CAF (point-free def: fst . effectOptimizeWithStats).
         * It returns a thunk wrapping a closure (Program -> Program). */
        int64_t thunk = Frankenstein_Core_EffectOpt_effectOptimize();
        int64_t closure = kk_thunk_force(thunk);
        int64_t fp = kk_field(closure, 0);
        typedef int64_t (*fn2_t)(int64_t, int64_t);
        prog = ((fn2_t)(intptr_t)fp)(closure, prog);
    }

    /* Skip evidence pass — not needed for effect-free programs.
     * TODO: fix cross-module tag mismatch that causes infinite recursion
     * in the evidence traversal when processing cross-module Core IR. */

    if (verbose) fprintf(stderr, "Running insertPerceus...\n");
    prog = Frankenstein_Core_Perceus_insertPerceus(prog);

    if (verbose) fprintf(stderr, "Running emitProgramText...\n");
    int64_t mlir = Frankenstein_MlirEmit_Emitter_emitProgramText(prog);

    if (!kk_is_string(mlir)) {
        fprintf(stderr, "Error: emitProgramText did not return a string\n");
        return 1;
    }

    /* Output MLIR */
    char* mlir_cstr = kk_str_dup_cstr(mlir);
    int64_t mlir_len = kk_str_len(mlir);

    if (output_path) {
        FILE* f = fopen(output_path, "w");
        if (!f) {
            fprintf(stderr, "Error: cannot open %s for writing\n", output_path);
            free(mlir_cstr);
            return 1;
        }
        fputs(mlir_cstr, f);
        fclose(f);
        if (verbose) fprintf(stderr, "Wrote %ld bytes of MLIR to %s\n",
                             (long)mlir_len, output_path);
    } else {
        fputs(mlir_cstr, stdout);
    }

    free(mlir_cstr);
    return 0;
}
