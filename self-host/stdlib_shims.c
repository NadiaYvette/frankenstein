/* Minimal Haskell stdlib shims for the self-hosted binary.
 *
 * These are the absolute minimum needed to exercise self-hosted passes
 * that use string literals (unitType, anyType, etc.).  The emitter
 * compiles string literals as kk_string_from_literal() calls, then
 * wraps them in fromString() for OverloadedStrings.  Since the value
 * is already a kk_string, fromString is the identity function.
 *
 * More stdlib shims (Data.Map, Data.Set, etc.) will be needed to
 * exercise deeper passes like perceusExpr or deriveSelectors.
 */

#include <stdint.h>

/* GHC.Internal.Data.String.fromString :: String -> a
 * With OverloadedStrings, GHC inserts fromString around literals.
 * In our runtime, the literal is already a kk_string — pass through. */
int64_t ghc_fromString_1(int64_t s)
    __asm__("GHC_Internal_Data_String_fromString$1");
int64_t ghc_fromString_1(int64_t s) {
    return s;
}
