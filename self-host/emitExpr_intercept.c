/* Diagnostic interceptor for Frankenstein_MlirEmit_Emitter_emitExpr$1.
 *
 * The stage 2 binary's compiled `emitExpr$1` returns 0 (NULL) for some
 * Expr constructor.  This wrapper:
 *   1. Logs the input Expr's kk_tag on entry
 *   2. Calls the real emitExpr
 *   3. Aborts with the input tag if the result is 0
 *
 * Gated on KK_EMITEXPR_TRACE env var so it stays cheap when not debugging.
 *
 * Linked into SHIM_OBJS so its definition wins via --allow-multiple-definition
 * (link order: shims before stage 2 .o files).
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "../runtime/kk_runtime.h"

extern int64_t Frankenstein_MlirEmit_Emitter_emitExpr(int64_t expr);

static const char* expr_tag_name(int64_t tag) {
    switch (tag) {
    case 44409: return "EVar";
    case 33785: return "ELit";
    case 24176: return "ECon";
    case 22033: return "EApp";
    case 33514: return "ELam";
    case 33653: return "ELet";
    case 62097: return "ECase";
    case 43241: return "ETypeApp";
    case 54722: return "ETypeLam";
    case 17712: return "EPerform";
    case 56674: return "EHandle";
    case 29665: return "EDelay";
    case 15575: return "EForce";
    case 63469: return "EFunRef";
    case 58529: return "EAddr";
    case 36865: return "EOp";
    default:    return "???";
    }
}

int64_t emitExpr_intercept(int64_t expr) __asm__("Frankenstein_MlirEmit_Emitter_emitExpr$1");
int64_t emitExpr_intercept(int64_t expr) {
    int trace = (getenv("KK_EMITEXPR_TRACE") != NULL);
    int64_t tag = 0;
    if (kk_is_heap_ptr(expr)) tag = kk_tag(expr);
    if (trace) {
        fprintf(stderr, "[emitExpr] tag=%ld (%s) heap=%d\n",
                (long)tag, expr_tag_name(tag), kk_is_heap_ptr(expr));
        fflush(stderr);
    }
    int64_t result = Frankenstein_MlirEmit_Emitter_emitExpr(expr);
    if (result == 0) {
        fprintf(stderr, "[emitExpr ***NULL RETURN***] tag=%ld (%s) expr=%p caller=%p\n",
                (long)tag, expr_tag_name(tag), (void*)expr, __builtin_return_address(0));
        /* Dump first few fields of the bogus expr arg */
        if (kk_is_heap_ptr(expr)) {
            int64_t nf = kk_nfields(expr);
            fprintf(stderr, "  expr nfields=%ld tag=0x%lx\n", (long)nf, (long)tag);
            for (int64_t i = 0; i < nf && i < 4; i++) {
                int64_t fi = kk_field(expr, i);
                fprintf(stderr, "  expr.field[%ld] = 0x%lx heap=%d\n",
                        (long)i, (long)fi, kk_is_heap_ptr(fi));
            }
        }
        fflush(stderr);
        abort();
    }
    if (trace) {
        fprintf(stderr, "[emitExpr] tag=%ld (%s) → result=%p\n",
                (long)tag, expr_tag_name(tag), (void*)result);
        fflush(stderr);
    }
    return result;
}
