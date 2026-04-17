/* Data.Char shims for Frankenstein self-hosted binary.
 *
 * Symbols:
 *   GHC_Internal_Char_chr$1
 *   Data_Char_digitToInt$1
 *   GHC_Internal_Unicode_isHexDigit$0  (closure)
 */

#include <stdint.h>
#include "../runtime/kk_runtime.h"

/* chr :: Int -> Char  (identity — both are codepoints as Int) */
int64_t ghc_char_chr_1(int64_t cp) __asm__("GHC_Internal_Char_chr$1");
int64_t ghc_char_chr_1(int64_t cp) { return cp; }

/* digitToInt :: Char -> Int */
static int64_t digit_to_int(int64_t c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return 0;  /* error case — Haskell would throw */
}

int64_t data_char_digitToInt_1(int64_t c) __asm__("Data_Char_digitToInt$1");
int64_t data_char_digitToInt_1(int64_t c) { return digit_to_int(c); }

/* isHexDigit :: Char -> Bool */
static int64_t is_hex_digit(int64_t c) {
    return (c >= '0' && c <= '9') ||
           (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F') ? 1 : 0;
}

/* isHexDigit$0: closure version (used as a value, e.g. T.all isHexDigit) */
#define CLOS_TAG_CH 0x434C4F53
static int64_t tram_isHexDigit(int64_t clos, int64_t arg) {
    (void)clos;
    return is_hex_digit(arg);
}

int64_t ghc_isHexDigit_0(void) __asm__("GHC_Internal_Unicode_isHexDigit$0");
int64_t ghc_isHexDigit_0(void) {
    int64_t c = kk_alloc_con(CLOS_TAG_CH, 1);
    kk_set_field(c, 0, (int64_t)(intptr_t)tram_isHexDigit);
    return c;
}
