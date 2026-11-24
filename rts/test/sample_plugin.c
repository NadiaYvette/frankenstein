/*
 * sample_plugin.c — Sample Organ Dynamic Plugin
 */
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "organ_rt.h"
#include "organ_dso.h"

static const organ_rt_host_table_t *g_host = NULL;
static organ_dso_handle_t *g_handle = NULL;

static const organ_dso_export_t g_exports[] = {
    {"sample_create_obj", 0, 0, 1}
};

static const organ_dso_meta_t g_meta = {
    .abi_version = ORGAN_DSO_ABI_VERSION,
    .layout_policy = ORGAN_DSO_LAYOUT_FROZEN,
    .interface_hash = 0xCAFEF00DULL,
    .module_name = "SamplePlugin",
    .source_lang = "c11",
    .export_count = 1,
    .exports = g_exports
};

ORGAN_EXPORT const organ_dso_meta_t *organ_dso_get_meta(void) {
    return &g_meta;
}

ORGAN_EXPORT int organ_dso_init(const organ_rt_host_table_t *host, organ_dso_handle_t *handle) {
    if (!host || host->abi_version != ORGAN_DSO_ABI_VERSION) {
        return ORGAN_DSO_ERR_ABI;
    }
    g_host = host;
    g_handle = handle;
    return 0;
}

static void plugin_drop_fields(organ_obj_t *obj) {
    (void)obj;
    organ_dso_track_drop(g_handle);
}

ORGAN_EXPORT organ_obj_t *sample_create_obj(void) {
    if (!g_host || !g_host->host_alloc) {
        return NULL;
    }
    size_t sz = sizeof(organ_obj_t) + sizeof(int64_t);
    organ_obj_t *obj = (organ_obj_t *)g_host->host_alloc(sz);
    if (!obj) {
        return NULL;
    }
    obj->rc = 1;
    obj->kind = (uint16_t)ORGAN_KIND_CTOR;
    obj->arity = 1;
    obj->content_tag = 0x12345678ULL;
    obj->drop_fields = plugin_drop_fields;
    obj->fields[0] = 42;
    organ_dso_track_alloc(g_handle);
    return obj;
}
