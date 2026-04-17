/* System.Directory, System.FilePath.Posix, System.Process, Text.Printf
 * shims for Frankenstein self-hosted binary.
 *
 * These provide minimal filesystem and process operations.  Strings
 * are kk_string ropes (int64_t).  For operations that involve the real
 * filesystem, we convert to/from C strings via kk_str_dup_cstr().
 */

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <libgen.h>
#include "../runtime/kk_runtime.h"

/* ================================================================== */
/*  System.Directory                                                    */
/* ================================================================== */

int64_t sys_dir_copyFile_2(int64_t src, int64_t dst)
    __asm__("System_Directory_copyFile$2");
int64_t sys_dir_copyFile_2(int64_t src, int64_t dst) {
    char *csrc = kk_str_dup_cstr(src);
    char *cdst = kk_str_dup_cstr(dst);
    FILE *in = fopen(csrc, "rb");
    FILE *out = in ? fopen(cdst, "wb") : NULL;
    if (in && out) {
        char buf[4096];
        size_t n;
        while ((n = fread(buf, 1, sizeof(buf), in)) > 0) fwrite(buf, 1, n, out);
    }
    if (in) fclose(in);
    if (out) fclose(out);
    free(csrc); free(cdst);
    return 0;
}

int64_t sys_dir_createDirIfMissing_2(int64_t parents, int64_t path)
    __asm__("System_Directory_createDirectoryIfMissing$2");
int64_t sys_dir_createDirIfMissing_2(int64_t parents, int64_t path) {
    (void)parents;
    char *cpath = kk_str_dup_cstr(path);
    /* Simple mkdir -p: iterate through path components */
    if (parents) {
        char *tmp = strdup(cpath);
        for (char *p = tmp + 1; *p; p++) {
            if (*p == '/') { *p = '\0'; mkdir(tmp, 0755); *p = '/'; }
        }
        mkdir(tmp, 0755);
        free(tmp);
    } else {
        mkdir(cpath, 0755);
    }
    free(cpath);
    return 0;
}

int64_t sys_dir_doesDirExist_0(void)
    __asm__("System_Directory_doesDirectoryExist$0");
int64_t sys_dir_doesDirExist_0(void) { return 0; }

int64_t sys_dir_doesDirExist_1(int64_t path)
    __asm__("System_Directory_doesDirectoryExist$1");
int64_t sys_dir_doesDirExist_1(int64_t path) {
    char *cpath = kk_str_dup_cstr(path);
    struct stat st;
    int64_t result = (stat(cpath, &st) == 0 && S_ISDIR(st.st_mode)) ? 1 : 0;
    free(cpath);
    return result;
}

int64_t sys_dir_doesFileExist_1(int64_t path)
    __asm__("System_Directory_doesFileExist$1");
int64_t sys_dir_doesFileExist_1(int64_t path) {
    char *cpath = kk_str_dup_cstr(path);
    struct stat st;
    int64_t result = (stat(cpath, &st) == 0 && S_ISREG(st.st_mode)) ? 1 : 0;
    free(cpath);
    return result;
}

int64_t sys_dir_getCwd_0(void)
    __asm__("System_Directory_getCurrentDirectory$0");
int64_t sys_dir_getCwd_0(void) {
    char buf[4096];
    if (getcwd(buf, sizeof(buf)))
        return kk_str_alloc_leaf_owned(buf, (int64_t)strlen(buf));
    return kk_str_alloc_leaf_owned(".", 1);
}

int64_t sys_dir_getTmpDir_0(void)
    __asm__("System_Directory_getTemporaryDirectory$0");
int64_t sys_dir_getTmpDir_0(void) {
    const char *tmp = getenv("TMPDIR");
    if (!tmp) tmp = "/tmp";
    return kk_str_alloc_leaf_owned(tmp, (int64_t)strlen(tmp));
}

int64_t sys_dir_listDir_1(int64_t path)
    __asm__("System_Directory_listDirectory$1");
int64_t sys_dir_listDir_1(int64_t path) {
    char *cpath = kk_str_dup_cstr(path);
    DIR *d = opendir(cpath);
    free(cpath);
    if (!d) return kk_nil();
    int64_t result = kk_nil();
    int64_t *arr = NULL;
    int64_t n = 0, cap = 0;
    struct dirent *ent;
    while ((ent = readdir(d)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0) continue;
        if (n >= cap) { cap = cap ? cap * 2 : 16; arr = realloc(arr, (size_t)cap * sizeof(int64_t)); }
        arr[n++] = kk_str_alloc_leaf_owned(ent->d_name, (int64_t)strlen(ent->d_name));
    }
    closedir(d);
    for (int64_t i = n - 1; i >= 0; i--) result = kk_cons(arr[i], result);
    free(arr);
    return result;
}

int64_t sys_dir_makeAbsolute_1(int64_t path)
    __asm__("System_Directory_makeAbsolute$1");
int64_t sys_dir_makeAbsolute_1(int64_t path) {
    char *cpath = kk_str_dup_cstr(path);
    char *abs = realpath(cpath, NULL);
    int64_t result;
    if (abs) {
        result = kk_str_alloc_leaf_owned(abs, (int64_t)strlen(abs));
        free(abs);
    } else {
        /* realpath failed, return as-is */
        result = path;
    }
    free(cpath);
    return result;
}

int64_t sys_dir_removeDirRecursive_1(int64_t path)
    __asm__("System_Directory_removeDirectoryRecursive$1");
int64_t sys_dir_removeDirRecursive_1(int64_t path) {
    char *cpath = kk_str_dup_cstr(path);
    char cmd[4200];
    snprintf(cmd, sizeof(cmd), "rm -rf '%s'", cpath);
    system(cmd);
    free(cpath);
    return 0;
}

/* ================================================================== */
/*  System.FilePath.Posix                                               */
/* ================================================================== */

/* </> (path separator, collides as ___) */
int64_t sys_fp_combine_2(int64_t a, int64_t b)
    __asm__("System_FilePath_Posix____$2");
int64_t sys_fp_combine_2(int64_t a, int64_t b) {
    char *ca = kk_str_dup_cstr(a);
    char *cb = kk_str_dup_cstr(b);
    size_t la = strlen(ca), lb = strlen(cb);
    char *buf = malloc(la + 1 + lb + 1);
    memcpy(buf, ca, la);
    if (la > 0 && ca[la-1] != '/') buf[la++] = '/';
    memcpy(buf + la, cb, lb + 1);
    int64_t result = kk_str_alloc_leaf_owned(buf, (int64_t)(la + lb));
    free(ca); free(cb); free(buf);
    return result;
}

int64_t sys_fp_takeBaseName_1(int64_t path)
    __asm__("System_FilePath_Posix_takeBaseName$1");
int64_t sys_fp_takeBaseName_1(int64_t path) {
    char *cpath = kk_str_dup_cstr(path);
    char *base = basename(cpath);
    /* Strip extension */
    char *dot = strrchr(base, '.');
    int64_t len = dot ? (int64_t)(dot - base) : (int64_t)strlen(base);
    int64_t result = kk_str_alloc_leaf_owned(base, len);
    free(cpath);
    return result;
}

int64_t sys_fp_takeDirectory_1(int64_t path)
    __asm__("System_FilePath_Posix_takeDirectory$1");
int64_t sys_fp_takeDirectory_1(int64_t path) {
    char *cpath = kk_str_dup_cstr(path);
    char *dir = dirname(cpath);
    int64_t result = kk_str_alloc_leaf_owned(dir, (int64_t)strlen(dir));
    free(cpath);
    return result;
}

int64_t sys_fp_takeFileName_1(int64_t path)
    __asm__("System_FilePath_Posix_takeFileName$1");
int64_t sys_fp_takeFileName_1(int64_t path) {
    char *cpath = kk_str_dup_cstr(path);
    char *base = basename(cpath);
    int64_t result = kk_str_alloc_leaf_owned(base, (int64_t)strlen(base));
    free(cpath);
    return result;
}

/* ================================================================== */
/*  System.Process                                                      */
/* ================================================================== */

/* Helper: run command, capture stdout+stderr, return (exitcode, stdout, stderr) */
static int64_t run_cmd(const char *cmd) {
    FILE *fp = popen(cmd, "r");
    if (!fp) return kk_str_alloc_leaf_owned("", 0);
    char *buf = NULL;
    size_t len = 0, cap = 0;
    char line[4096];
    while (fgets(line, sizeof(line), fp)) {
        size_t ll = strlen(line);
        if (len + ll >= cap) { cap = (cap + ll) * 2 + 1; buf = realloc(buf, cap); }
        memcpy(buf + len, line, ll);
        len += ll;
    }
    int status = pclose(fp);
    int64_t out_str = buf ? kk_str_alloc_leaf_owned(buf, (int64_t)len) : kk_str_alloc_leaf_owned("", 0);
    free(buf);
    (void)status;
    return out_str;
}

int64_t sys_proc_proc_2(int64_t cmd, int64_t args)
    __asm__("System_Process_proc$2");
int64_t sys_proc_proc_2(int64_t cmd, int64_t args) {
    /* Return a pair (cmd, args) as a CreateProcess stand-in */
    return kk_pair(cmd, args);
}

int64_t sys_proc_readCreatePWEC_2(int64_t cp, int64_t stdin_str)
    __asm__("System_Process_readCreateProcessWithExitCode$2");
int64_t sys_proc_readCreatePWEC_2(int64_t cp, int64_t stdin_str) {
    (void)stdin_str;
    int64_t cmd = kk_fst(cp);
    int64_t args = kk_snd(cp);
    /* Build command string */
    char *ccmd = kk_str_dup_cstr(cmd);
    size_t total_len = strlen(ccmd) + 1;
    int64_t cur = args;
    while (!kk_is_nil(cur)) {
        char *a = kk_str_dup_cstr(kk_list_head(cur));
        total_len += strlen(a) + 3; /* space + quotes */
        free(a);
        cur = kk_list_tail(cur);
    }
    char *full = malloc(total_len + 64);
    strcpy(full, ccmd);
    cur = args;
    while (!kk_is_nil(cur)) {
        char *a = kk_str_dup_cstr(kk_list_head(cur));
        strcat(full, " '");
        strcat(full, a);
        strcat(full, "'");
        free(a);
        cur = kk_list_tail(cur);
    }
    int64_t out = run_cmd(full);
    free(ccmd); free(full);
    /* Return (ExitSuccess, stdout, stderr) — ExitSuccess = tag 0 */
    int64_t exitcode = kk_alloc_con(0, 0); /* ExitSuccess */
    int64_t triple = kk_alloc_con(0, 3);
    kk_set_field(triple, 0, exitcode);
    kk_set_field(triple, 1, out);
    kk_set_field(triple, 2, kk_str_alloc_leaf_owned("", 0));
    return triple;
}

int64_t sys_proc_readProcess_3(int64_t cmd, int64_t args, int64_t stdin_str)
    __asm__("System_Process_readProcess$3");
int64_t sys_proc_readProcess_3(int64_t cmd, int64_t args, int64_t stdin_str) {
    (void)stdin_str;
    char *ccmd = kk_str_dup_cstr(cmd);
    size_t total_len = strlen(ccmd) + 1;
    int64_t cur = args;
    while (!kk_is_nil(cur)) {
        char *a = kk_str_dup_cstr(kk_list_head(cur));
        total_len += strlen(a) + 3;
        free(a);
        cur = kk_list_tail(cur);
    }
    char *full = malloc(total_len + 64);
    strcpy(full, ccmd);
    cur = args;
    while (!kk_is_nil(cur)) {
        char *a = kk_str_dup_cstr(kk_list_head(cur));
        strcat(full, " '");
        strcat(full, a);
        strcat(full, "'");
        free(a);
        cur = kk_list_tail(cur);
    }
    int64_t out = run_cmd(full);
    free(ccmd); free(full);
    return out;
}

int64_t sys_proc_readPWEC_3(int64_t cmd, int64_t args, int64_t stdin_str)
    __asm__("System_Process_readProcessWithExitCode$3");
int64_t sys_proc_readPWEC_3(int64_t cmd, int64_t args, int64_t stdin_str) {
    int64_t out = sys_proc_readProcess_3(cmd, args, stdin_str);
    int64_t exitcode = kk_alloc_con(0, 0); /* ExitSuccess */
    int64_t triple = kk_alloc_con(0, 3);
    kk_set_field(triple, 0, exitcode);
    kk_set_field(triple, 1, out);
    kk_set_field(triple, 2, kk_str_alloc_leaf_owned("", 0));
    return triple;
}

/* ================================================================== */
/*  Text.Printf                                                         */
/* ================================================================== */

int64_t text_printf_2(int64_t fmt, int64_t arg)
    __asm__("Text_Printf_printf$2");
int64_t text_printf_2(int64_t fmt, int64_t arg) {
    char *cfmt = kk_str_dup_cstr(fmt);
    char buf[256];
    if (kk_is_string(arg)) {
        char *carg = kk_str_dup_cstr(arg);
        snprintf(buf, sizeof(buf), cfmt, carg);
        free(carg);
    } else {
        snprintf(buf, sizeof(buf), cfmt, (long)arg);
    }
    free(cfmt);
    /* kk_string_from_cstr doesn't own the buffer (owns_bytes=0),
     * so we must keep the malloc'd copy alive. Minor leak is OK. */
    size_t len = strlen(buf);
    char *copy = (char*)malloc(len + 1);
    memcpy(copy, buf, len + 1);
    return kk_string_from_cstr((int64_t)(intptr_t)copy);
}
