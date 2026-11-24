/*
 * organ_dso.h — Ingolstadt Dynamic Linking & DSO-as-Plugin ABI
 *
 * Makes dynamic plugins as smooth as C while preventing C++ ODR/allocator
 * traps and Perceus drop_fields use-after-dlclose crashes.
 */
#ifndef ORGAN_DSO_H
#define ORGAN_DSO_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define ORGAN_DSO_ABI_VERSION 1u
#define ORGAN_DSO_ERR_ABI (-22)   /* -EINVAL */
#define ORGAN_DSO_ERR_BUSY (-16)  /* -EBUSY: live plugin objects still exist */

#if defined(__GNUC__) || defined(__clang__)
#define ORGAN_EXPORT __attribute__((visibility("default")))
#else
#define ORGAN_EXPORT
#endif

/* Layout compatibility contract across the DSO seam (Swift Library Evolution split). */
typedef enum {
    ORGAN_DSO_LAYOUT_FROZEN    = 0, /* C-like direct struct layout verified by hash */
    ORGAN_DSO_LAYOUT_RESILIENT = 1  /* Polytropos RowLayoutDescriptor witness table */
} organ_dso_layout_t;

/* Host-injected runtime vtable ensuring unified heap allocator & io_uring reactor. */
typedef struct {
    uint32_t abi_version;
    void *(*host_alloc)(size_t bytes);
    void (*host_free)(void *ptr);
    void *host_uring_reactor; /* Optional organ_core_reactor_t* */
} organ_rt_host_table_t;

/* Single exported symbol entry inside a DSO's metadata descriptor. */
typedef struct {
    const char *symbol_name;
    uint32_t arity;
    uint32_t ev_params;
    uint32_t mem_profile;
} organ_dso_export_t;

/* Versioned read-only metadata returned by organ_dso_get_meta(). */
typedef struct {
    uint32_t abi_version;
    organ_dso_layout_t layout_policy;
    uint64_t interface_hash; /* 64-bit FNV-1a hash of canonical .organ-iface */
    const char *module_name;
    const char *source_lang;
    uint32_t export_count;
    const organ_dso_export_t *exports;
} organ_dso_meta_t;

/* Runtime handle tracking live objects pinned to a loaded DSO's .text segment. */
typedef struct {
    void *dl_handle;
    const organ_dso_meta_t *meta;
    uint64_t live_objects;
    bool pin_nodelete;
} organ_dso_handle_t;

typedef const organ_dso_meta_t *(*organ_dso_get_meta_fn)(void);
typedef int (*organ_dso_init_fn)(const organ_rt_host_table_t *host,
                                 organ_dso_handle_t *self_handle);

static inline void organ_dso_handle_init(organ_dso_handle_t *h,
                                         void *dl_handle,
                                         const organ_dso_meta_t *meta) {
    h->dl_handle = dl_handle;
    h->meta = meta;
    h->live_objects = 0u;
    h->pin_nodelete = false;
}

static inline void organ_dso_track_alloc(organ_dso_handle_t *h) {
    if (h != NULL) {
        h->live_objects += 1u;
    }
}

static inline void organ_dso_track_drop(organ_dso_handle_t *h) {
    if (h != NULL && h->live_objects > 0u) {
        h->live_objects -= 1u;
    }
}

/* Safe dlclose() precondition: true iff zero live objects reference this DSO's drop_fields. */
static inline bool organ_dso_can_unload(const organ_dso_handle_t *h) {
    if (h == NULL || h->pin_nodelete) {
        return false;
    }
    return h->live_objects == 0u;
}

static inline int organ_dso_verify_meta(const organ_dso_meta_t *meta,
                                        uint64_t expected_iface_hash) {
    if (meta == NULL || meta->abi_version != ORGAN_DSO_ABI_VERSION) {
        return ORGAN_DSO_ERR_ABI;
    }
    if (expected_iface_hash != 0u && meta->interface_hash != expected_iface_hash) {
        return ORGAN_DSO_ERR_ABI;
    }
    return 0;
}

#ifdef __cplusplus
}
#endif

#endif /* ORGAN_DSO_H */
