/* Frankenstein bump-arena allocator.
 *
 * A linked list of large slabs, each filled by a simple pointer-bump.
 * Used to back kk_alloc_con (and any other hot constructor-shaped
 * allocation) so that the common case avoids the per-cell malloc/free
 * pair entirely. Allocations are never individually reclaimed; the
 * whole arena is dropped on program exit (or via kk_arena_reset).
 *
 * Coexistence with libc malloc:
 *   kk_arena_owns(p) reports whether a pointer was handed out by the
 *   arena, so kk_drop / cycle-collector code paths can call
 *   kk_arena_free(p) instead of free(p) — it is a no-op for
 *   arena-owned pointers and a real free() for everything else.
 *   This lets transient buffers (string bytes, intermediate flatten
 *   results, …) keep using libc malloc unchanged.
 *
 * Disable at runtime with the environment variable KK_NO_ARENA=1
 * (handy for ASan / leak-checking runs).
 */

#ifndef KK_ARENA_H
#define KK_ARENA_H

#include <stdint.h>
#include <stddef.h>

/* Allocate `size` bytes from the arena. Returns NULL if the arena is
 * disabled (caller should fall back to malloc). The returned pointer
 * is 8-byte aligned. */
void* kk_arena_alloc(size_t size);

/* True iff `ptr` lies within any slab the arena has handed out. */
int kk_arena_owns(const void* ptr);

/* Fast O(1) check: true iff `ptr` is within the arena's global address range.
 * May return true for non-arena pointers that happen to be in the range
 * (rare: only if a malloc'd string buffer shares the same address space). */
int kk_arena_maybe_owns(const void* ptr);

/* Free helper: no-op when `ptr` is arena-owned, free(ptr) otherwise.
 * Safe to call with NULL. */
void kk_arena_free(void* ptr);

/* Drop every slab the arena currently owns. Mostly useful for tests
 * and benchmarks; not invoked automatically. */
void kk_arena_reset(void);

/* Total bytes the arena has handed out (sum of allocations). */
int64_t kk_arena_bytes_allocated(void);

/* Total bytes currently held in slabs (capacity, not utilisation). */
int64_t kk_arena_bytes_reserved(void);

/* ---- Checkpoint / rollback (for abort effect cleanup) ----
 *
 * A checkpoint captures the arena's current position (slab + offset).
 * kk_arena_rollback frees everything allocated after the checkpoint,
 * reclaiming memory that would otherwise leak when longjmp skips
 * Perceus drop operations in an abandoned continuation.
 *
 * Usage:
 *   kk_arena_checkpoint_t cp = kk_arena_checkpoint();
 *   ... allocate stuff ...
 *   kk_arena_rollback(cp);  // everything since cp is freed
 */
typedef struct {
    void*  slab;     /* opaque: the slab at checkpoint time */
    size_t used;     /* bump offset within that slab */
} kk_arena_checkpoint_t;

kk_arena_checkpoint_t kk_arena_checkpoint(void);
void kk_arena_rollback(kk_arena_checkpoint_t cp);

/* True iff `ptr` falls in the region that would be freed by rollback to `cp`. */
int kk_arena_in_rollback_region(const void* ptr, kk_arena_checkpoint_t cp);

#endif /* KK_ARENA_H */
