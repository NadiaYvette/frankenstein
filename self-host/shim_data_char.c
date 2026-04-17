/* Data.Char shims for Frankenstein self-hosted binary.
 *
 * Symbols:
 *   GHC_Internal_Char_chr$1
 *   Data_Char_digitToInt$1
 *   GHC_Internal_Unicode_isHexDigit$0  (closure)
 */

#include <stdint.h>
#include "../runtime/kk_runtime.h"

/* Unbox a Char value: if it's a heap-allocated C# box (tag 30786),
 * extract the codepoint from field 0. Otherwise treat as raw codepoint. */
#define CHAR_BOX_TAG 30786
static int64_t unbox_char(int64_t c) {
    if (kk_is_heap_ptr(c) && kk_tag(c) == CHAR_BOX_TAG)
        return kk_field(c, 0);
    return c;
}

static int64_t box_char(int64_t cp) {
    int64_t c = kk_alloc_con(CHAR_BOX_TAG, 1);
    kk_set_field(c, 0, cp);
    return c;
}

/* chr :: Int -> Char  (returns boxed Char) */
int64_t ghc_char_chr_1(int64_t cp) __asm__("GHC_Internal_Char_chr$1");
int64_t ghc_char_chr_1(int64_t cp) { return box_char(cp); }

/* digitToInt :: Char -> Int  (receives boxed Char) */
static int64_t digit_to_int(int64_t c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return 0;  /* error case — Haskell would throw */
}

int64_t data_char_digitToInt_1(int64_t c) __asm__("Data_Char_digitToInt$1");
int64_t data_char_digitToInt_1(int64_t c) { return digit_to_int(unbox_char(c)); }

/* isHexDigit :: Char -> Bool  (receives boxed Char) */
static int64_t is_hex_digit(int64_t c) {
    return (c >= '0' && c <= '9') ||
           (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F') ? 1 : 0;
}

/* isHexDigit$0: closure version (used as a value, e.g. T.all isHexDigit) */
#define CLOS_TAG_CH 0x434C4F53
static int64_t tram_isHexDigit(int64_t clos, int64_t arg) {
    (void)clos;
    return is_hex_digit(unbox_char(arg));
}

int64_t ghc_isHexDigit_0(void) __asm__("GHC_Internal_Unicode_isHexDigit$0");
int64_t ghc_isHexDigit_0(void) {
    int64_t c = kk_alloc_con(CLOS_TAG_CH, 1);
    kk_set_field(c, 0, (int64_t)(intptr_t)tram_isHexDigit);
    return c;
}
