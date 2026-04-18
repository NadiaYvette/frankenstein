/* kk_cycle.h — Bacon-Rajan synchronous cycle collector for Perceus RC
 *
 * Overview:
 *   Pure reference counting cannot collect cyclic garbage (refcount never
 *   reaches zero). The Bacon-Rajan algorithm detects and collects cycles
 *   using trial deletion:
 *
 *   1. When a decrement doesn't free (rc > 0), the object is a "possible root"
 *   2. Periodically, we trial-delete: for each candidate, temporarily subtract
 *      internal references. Objects that reach zero are in a cycle → free them.
 *
 * Colors (stored in high byte of refcount word):
 *   BLACK  (0) — in use, not a candidate
 *   PURPLE (1) — possible root (added to roots buffer on decrement)
 *   GRAY   (2) — being scanned (trial deletion in progress)
 *   WHITE  (3) — garbage (trial deletion found rc == 0)
 *
 * Integration with Perceus:
 *   - kk_drop() calls kk_cycle_candidate() when rc > 0 after decrement
 *   - kk_cycle_collect() runs the collector (called periodically or at exit)
 *   - Acyclic objects (detected by static analysis) skip cycle tracking
 *
 * Reference: Bacon & Rajan, "Concurrent Cycle Collection in Reference
 * Counted Systems", ECOOP 2001.
 */

#ifndef KK_CYCLE_H
#define KK_CYCLE_H

#include <stdint.h>

/* RC word layout (64 bits):
 *   bits 63-56: color      (8 bits)
 *   bits 55-48: nfields    (8 bits, max 255 — clamped)
 *   bits 47-0:  refcount   (48 bits)
 * nfields is packed into the RC word to avoid the O(n) linear-probe
 * side table that was a bottleneck at high allocation counts. */
#define KK_COLOR_MASK     0xFF00000000000000LL
#define KK_NFIELDS_MASK   0x00FF000000000000LL
#define KK_NFIELDS_SHIFT  48
#define KK_RC_MASK        0x0000FFFFFFFFFFFFLL
#define KK_COLOR_BLACK    0x0000000000000000LL
#define KK_COLOR_PURPLE   0x0100000000000000LL
#define KK_COLOR_GRAY     0x0200000000000000LL
#define KK_COLOR_WHITE    0x0300000000000000LL

/* Mark an object as a cycle candidate (called from kk_drop when rc > 0) */
void kk_cycle_candidate(int64_t ptr);

/* Run the cycle collector. Returns number of objects freed. */
int64_t kk_cycle_collect(void);

/* Check if a tag value represents a type that can participate in cycles.
 * Only constructor tags (not thunks/evv) with fields that could be pointers. */
int kk_can_be_cyclic(int64_t tag);

/* Field count registration (for scanning children during cycle collection) */
void    kk_register_nfields(int64_t ptr, int64_t nfields);
void    kk_unregister_nfields(int64_t ptr);
int64_t kk_nfields(int64_t ptr);

/* Statistics */
int64_t kk_cycle_roots_count(void);
int64_t kk_cycle_collected_count(void);

#endif /* KK_CYCLE_H */
