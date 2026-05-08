#!/usr/bin/env python3
"""Fix $0() references in stage2 MLIR.

The compiled emitter corrupts pattern binder Name objects, causing alias map
lookup failures. Variables that should reference local pattern fields become
unresolved 0-arity external calls: @frankenstein_X$0().

Two categories:
A) Pattern variables (names with z-encoded unique suffixes like bindTypezd...):
   Fix by matching trailing unique number to alias comments.
B) Function references (names like OrganIR_Parse_asArr):
   Fix by replacing with PAP closure construction.

Usage: python3 fix-dollar0-refs.py <stage2-file.mlir>
"""

import os
import re
import sys


# ─── Regex patterns ──────────────────────────────────────────────────────

DOLLAR0_RE = re.compile(
    r'(\s+)(%\w+)\s*=\s*func\.call\s+'
    r'@frankenstein_(\w+)\$0\(\)\s*:\s*\(\)\s*->\s*i64')

ALIAS_RE = re.compile(r'//\s*let\s+(\S+)\s*=\s*(%\w+)')

KK_FIELD_RE = re.compile(
    r'(%\w+)\s*=\s*func\.call\s+@kk_field\((%\w+),\s*(%\w+)\)')

CONST_RE = re.compile(r'(%\w+)\s*=\s*arith\.constant\s+(\d+)\s*:\s*i64')

FUNC_DECL_RE = re.compile(r'\s*func\.func\s+@(\S+)\(')

TRAILING_DIGITS_RE = re.compile(r'(\d{10,})$')

KNOWN_FUNCTIONS = {
    'OrganIR_Parse_asArr',
    'OrganIR_Parse_asStr',
    'OrganIR_Parse_asInt',
    'OrganIR_Parse_decodeName',
    'OrganIR_Parse_decodeQName',
    'OrganIR_Parse_decodeTy',
    'OrganIR_Parse_decodeTyVar',
    'OrganIR_Parse_decodePatBinder',
    'OrganIR_Parse_decodeLamParam',
    'OrganIR_Parse_decodeVisibility',
    'OrganIR_Parse_decodeSort',
    'OrganIR_Parse_decodeMetadata',
    'OrganIR_Parse_decodeConstructor',
    'Frankenstein_RustBridge_MirParse_jStr',
    'Frankenstein_RustBridge_MirParse_jInt',
    'Frankenstein_RustBridge_MirParse_jArr',
    'Frankenstein_RustBridge_MirParse_jBool',
    'Frankenstein_GhcBridge_CoreTranslate_isStateVar',
    'Frankenstein_MercuryBridge_CoreTranslate_extendBindingsFor',
    'Frankenstein_MercuryBridge_HldsParse_isComment',
    'Frankenstein_OrganIR_Consumer_consumeQName',
    'Frankenstein_MlirEmit_Dialects_valName',
    'Frankenstein_MlirEmit_Dialects_valType',
    'Frankenstein_Core_Types_bindExpr',
    'Frankenstein_Core_Types_defExpr',
}


# ─── Helpers ─────────────────────────────────────────────────────────────

def find_enclosing_func(lines, pos):
    for j in range(pos, -1, -1):
        fm = FUNC_DECL_RE.match(lines[j])
        if fm:
            return fm.group(1), j
    return None, None


def find_func_end(lines, start):
    depth = 0
    for j in range(start, len(lines)):
        for ch in lines[j]:
            if ch == '{':
                depth += 1
            elif ch == '}':
                depth -= 1
                if depth == 0 and j > start:
                    return j
    return len(lines) - 1


# ─── Scope checking ──────────────────────────────────────────────────────

def is_in_scope(lines, def_line, use_line):
    """Check if a variable defined at def_line is in scope at use_line.

    Walk from def_line to use_line counting braces. If we ever go negative
    (close more braces than we open), the definition's scope was exited
    before reaching the use site.
    """
    depth = 0
    for j in range(def_line + 1, use_line):
        for ch in lines[j]:
            if ch == '{':
                depth += 1
            elif ch == '}':
                depth -= 1
                if depth < 0:
                    return False  # exited the definition's scope
    return True


# ─── Strategy A: Match by trailing unique ────────────────────────────────

def fix_by_trailing_unique(var_name, lines, call_line, func_start):
    m = TRAILING_DIGITS_RE.search(var_name)
    if not m:
        return None
    unique = m.group(1)
    search_start = func_start if func_start is not None else max(0, call_line - 1000)
    for j in range(call_line - 1, search_start - 1, -1):
        am = ALIAS_RE.search(lines[j])
        if am:
            alias_name = am.group(1)
            if alias_name.endswith(unique) and not alias_name.startswith('_'):
                # Verify the alias variable is in scope at the call site
                if is_in_scope(lines, j, call_line):
                    return am.group(2)
    return None


# ─── Strategy B: Stage1 field index lookup ───────────────────────────────

def load_stage1_cache(stage1_dir):
    cache = {}
    if not stage1_dir or not os.path.isdir(stage1_dir):
        return cache
    for fn in sorted(os.listdir(stage1_dir)):
        if fn.endswith('.mlir'):
            path = os.path.join(stage1_dir, fn)
            if os.path.isfile(path):
                try:
                    with open(path) as f:
                        cache[fn] = f.read()
                except Exception:
                    pass
    return cache


def lookup_field_index_in_stage1(func_name, var_name, stage1_cache):
    candidates = [func_name]
    if func_name.startswith('frankenstein_'):
        candidates.append(func_name[len('frankenstein_'):])

    for lookup_name in candidates:
        for stage1_text in stage1_cache.values():
            func_re = re.compile(
                r'func\.func\s+@' + re.escape(lookup_name) +
                r'\([^)]*\)[^{]*\{(.*?)^\s*\}',
                re.MULTILINE | re.DOTALL)
            match = func_re.search(stage1_text)
            if not match:
                continue
            body = match.group(1)
            alias_re = re.compile(
                r'//\s*let\s+(' + re.escape(var_name) + r'\S*)\s*=\s*(%\w+)')
            am = alias_re.search(body)
            if not am:
                continue
            alias_var = am.group(2)
            field_re = re.compile(
                re.escape(alias_var) +
                r'\s*=\s*func\.call\s+@kk_field\((%\w+),\s*(%\w+)\)')
            fm = field_re.search(body)
            if not fm:
                continue
            idx_var = fm.group(2)
            const_re_p = re.compile(
                re.escape(idx_var) + r'\s*=\s*arith\.constant\s+(\d+)\s*:')
            cm = const_re_p.search(body)
            if not cm:
                continue
            return int(cm.group(1))
    return None


def fix_by_stage1_field_index(var_name, lines, call_line, func_name,
                              func_start, stage1_cache):
    if not stage1_cache:
        return None
    field_idx = lookup_field_index_in_stage1(
        func_name, var_name, stage1_cache)
    if field_idx is None:
        return None
    search_start = func_start if func_start is not None else max(0, call_line - 1000)
    consts = {}
    for j in range(search_start, call_line):
        cm = CONST_RE.search(lines[j])
        if cm:
            consts[cm.group(1)] = int(cm.group(2))
    for j in range(call_line - 1, search_start - 1, -1):
        fm = KK_FIELD_RE.search(lines[j])
        if fm:
            idx_var = fm.group(3)
            if idx_var in consts and consts[idx_var] == field_idx:
                if is_in_scope(lines, j, call_line):
                    return fm.group(1)
    return None


# ─── Strategy C: PAP closure for function references ─────────────────────

def find_pap_wrapper(var_name, lines):
    """Find the PAP wrapper function and its parameter count."""
    pap_re = re.compile(
        r'func\.func\s+@(\w*pap_frankenstein_' +
        re.escape(var_name) + r'_0)\(([^)]*)\)')
    for line in lines:
        m = pap_re.search(line)
        if m:
            pap_name = m.group(1)
            params = m.group(2)
            # Count i64 parameters
            nparams = params.count('i64')
            return pap_name, nparams
    return None, 0


# ─── Main: two-pass fix ─────────────────────────────────────────────────

def fix_dollar0_refs(path):
    with open(path) as f:
        text = f.read()

    if '$0()' not in text:
        return

    lines = text.split('\n')

    # Don't strip private $0 declarations yet — do it after we know which
    # $0 calls are fixed, so we don't remove decls for unfixed calls.

    stage2_dir = os.path.dirname(os.path.abspath(path))
    stage1_dir = os.path.dirname(stage2_dir)
    if not os.path.isdir(stage1_dir):
        stage1_dir = None
    stage1_cache = load_stage1_cache(stage1_dir)

    # ── Pass 1: collect all fixes ──

    # Each fix: (line_idx, result_var, correct_var, fix_type, pap_name)
    #   fix_type: 'rename' (replace result_var→correct_var in func)
    #             'pap' (replace line with PAP closure, rename result_var)
    fixes = []

    for i, line in enumerate(lines):
        m = DOLLAR0_RE.match(line)
        if not m:
            continue

        result_var = m.group(2)
        var_name = m.group(3)

        func_name, func_start = find_enclosing_func(lines, i)
        correct_var = None

        # Strategy A: trailing unique match
        correct_var = fix_by_trailing_unique(var_name, lines, i, func_start)
        if correct_var:
            fixes.append((i, result_var, correct_var, 'rename', var_name))
            continue

        # Strategy B: stage1 field index
        if var_name != 'fld':
            correct_var = fix_by_stage1_field_index(
                var_name, lines, i, func_name, func_start, stage1_cache)
            if correct_var:
                fixes.append((i, result_var, correct_var, 'rename', var_name))
                continue

        # Strategy C: PAP closure
        if var_name in KNOWN_FUNCTIONS:
            pap_name, pap_nparams = find_pap_wrapper(var_name, lines)
            if pap_name:
                fixes.append((i, result_var, None, 'pap',
                              (pap_name, pap_nparams)))
                continue

    if not fixes:
        with open(path, 'w') as f:
            f.write('\n'.join(lines))
        return

    # ── Pass 2: apply fixes ──

    # Group fixes by function (func_start, func_end)
    # Build per-function rename maps and line deletions

    # For each fix, determine its function range
    func_ranges = {}  # func_start -> func_end
    fix_func = {}     # fix_line -> func_start

    for line_idx, result_var, correct_var, fix_type, extra in fixes:
        _, fs = find_enclosing_func(lines, line_idx)
        if fs is not None and fs not in func_ranges:
            func_ranges[fs] = find_func_end(lines, fs)
        fix_func[line_idx] = fs

    # Build rename map per function: {func_start: {old_var: new_var, ...}}
    func_renames = {}  # func_start -> {old_var: new_var}
    delete_lines = set()
    pap_insertions = {}  # line_idx -> (indent, pap_lines, result_var, new_var)

    pap_counter = 0
    fix_unique = 0
    fix_stage1_count = 0
    fix_pap = 0

    for line_idx, result_var, correct_var, fix_type, extra in fixes:
        fs = fix_func[line_idx]
        if fs is None:
            continue

        if fix_type == 'rename':
            func_renames.setdefault(fs, {})[result_var] = correct_var
            delete_lines.add(line_idx)
            # Count by strategy (rough: if extra is a var name with digits → unique)
            dm = TRAILING_DIGITS_RE.search(extra)
            if dm:
                fix_unique += 1
            else:
                fix_stage1_count += 1

        elif fix_type == 'pap':
            pap_counter += 1
            pfx = f'_pap{pap_counter}'
            pap_clos_var = f'%{pfx}_clos'
            m = DOLLAR0_RE.match(lines[line_idx])
            indent = m.group(1)
            pap_name, pap_nparams = extra

            # Build the correct MLIR type for the PAP wrapper
            pap_type = ', '.join(['i64'] * pap_nparams)
            pap_mlir_type = f'({pap_type}) -> i64'

            pap_lines = [
                f'{indent}// FIXED: was $0() — PAP closure for {pap_name}',
                f'{indent}%{pfx}_tag = arith.constant 1129074515 : i64'
                f'  // KK_CLOSURE_TAG',
                f'{indent}%{pfx}_one = arith.constant 1 : i64',
                f'{indent}%{pfx}_clos = func.call @kk_alloc_con'
                f'(%{pfx}_tag, %{pfx}_one) : (i64, i64) -> i64',
                f'{indent}%{pfx}_fn = func.constant @{pap_name}'
                f' : {pap_mlir_type}',
                f'{indent}%{pfx}_ptr = builtin.unrealized_conversion_cast'
                f' %{pfx}_fn : {pap_mlir_type} to !llvm.ptr',
                f'{indent}%{pfx}_int = llvm.ptrtoint %{pfx}_ptr'
                f' : !llvm.ptr to i64',
                f'{indent}%{pfx}_zero = arith.constant 0 : i64',
                f'{indent}func.call @kk_set_field(%{pfx}_clos, %{pfx}_zero,'
                f' %{pfx}_int) : (i64, i64, i64) -> ()',
            ]
            pap_insertions[line_idx] = (pap_lines, result_var, pap_clos_var)
            func_renames.setdefault(fs, {})[result_var] = pap_clos_var
            fix_pap += 1

    # Collect set of fixed $0 var names (to strip their private decls)
    fixed_varnames = set()
    for line_idx, result_var, correct_var, fix_type, extra in fixes:
        m = DOLLAR0_RE.match(lines[line_idx])
        if m:
            fixed_varnames.add(m.group(3))

    # Apply: rebuild lines with renames, deletions, and PAP insertions
    out = []
    current_func_start = None
    current_func_end = -1
    current_renames = {}

    for i, line in enumerate(lines):
        # Strip private declarations ONLY for fixed $0 functions
        priv_m = re.match(
            r'\s*func\.func\s+private\s+@frankenstein_(\w+)\$0\(', line)
        if priv_m and priv_m.group(1) in fixed_varnames:
            continue

        # Check if we're entering a new function that has renames
        fm = FUNC_DECL_RE.match(line)
        if fm:
            for fs, fe in func_ranges.items():
                if fs == i and fs in func_renames:
                    current_func_start = fs
                    current_func_end = fe
                    current_renames = func_renames[fs]
                    break

        # Check if we passed the current function
        if i > current_func_end:
            current_func_start = None
            current_renames = {}

        # Handle deletions (rename-type fixes)
        if i in delete_lines:
            m = DOLLAR0_RE.match(line)
            if m:
                out.append(f'{m.group(1)}// FIXED by fix-dollar0-refs:'
                           f' was @frankenstein_{m.group(3)}$0()')
            continue

        # Handle PAP insertions
        if i in pap_insertions:
            pap_lines, _, _ = pap_insertions[i]
            out.extend(pap_lines)
            continue

        # Apply renames
        if current_renames:
            for old_var, new_var in current_renames.items():
                line = re.sub(
                    re.escape(old_var) + r'(?=[\s,):])',
                    new_var, line)
        out.append(line)

    # Report
    parts = []
    if fix_unique:
        parts.append(f'{fix_unique} by unique match')
    if fix_stage1_count:
        parts.append(f'{fix_stage1_count} by stage1 lookup')
    if fix_pap:
        parts.append(f'{fix_pap} by PAP closure')
    unfixed = sum(1 for l in out if re.search(
        r'func\.call\s+@frankenstein_\w+\$0\(\)', l))
    if unfixed:
        parts.append(f'{unfixed} unfixed')
    if parts:
        print(f'  fix-dollar0: {os.path.basename(path)}: '
              + ', '.join(parts), file=sys.stderr)

    with open(path, 'w') as f:
        f.write('\n'.join(out))


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('Usage: fix-dollar0-refs.py <file.mlir>', file=sys.stderr)
        sys.exit(1)
    fix_dollar0_refs(sys.argv[1])
