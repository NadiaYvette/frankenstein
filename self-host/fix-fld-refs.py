#!/usr/bin/env python3
"""Fix corrupted 'fld' pattern-variable references in stage2 MLIR.

The self-hosted compiler's sanitizeName (via T.concatMap) non-deterministically
corrupts the pattern variable name 'fld' to 'f0d' (or similar), causing the
emitter to generate '@frankenstein_fld$0()' external calls instead of yielding
the locally-bound field value from a pattern match.

Strategy: find each @frankenstein_fld$0() call, look for nearby alias comments
of the form '// let f0d0 = %vN' (the corrupted alias), and replace the fld$0
call + its yield with the correct field variable. Falls back to looking up the
function in stage1 MLIR for the correct field index.

Usage: python3 fix-fld-refs.py <file.mlir>
"""

import os
import re
import sys


# Pre-compiled patterns
FLD_CALL_RE = re.compile(
    r'(\s+)(%\w+)\s*=\s*func\.call\s+@frankenstein_fld\$0\(\)')
KK_FIELD_RE = re.compile(
    r'(%\w+)\s*=\s*func\.call\s+@kk_field\(%\w+,\s*(%\w+)\)')
CONST_RE = re.compile(
    r'(%\w+)\s*=\s*arith\.constant\s+(\d+)\s*:\s*i64')
ALIAS_RE = re.compile(r'//\s*let\s+(?!_)(\S+)\s*=\s*(%\w+)')
FUNC_DECL_RE = re.compile(r'\s*func\.func\s+@(\S+)\(')


def find_enclosing_func(lines, pos):
    """Find the enclosing func.func declaration, searching backwards."""
    for j in range(pos, -1, -1):
        fm = FUNC_DECL_RE.match(lines[j])
        if fm:
            return fm.group(1), j
    return None, None


def find_field_var_by_alias(lines, fld_line, func_start):
    """Strategy 1: find the correct field var from alias comments."""
    start = func_start or 0  # search entire function (not just 200 lines)
    correct_var = None
    for j in range(start, fld_line):
        am = ALIAS_RE.search(lines[j])
        if am:
            alias_name = am.group(1)
            if not alias_name.startswith('_') and alias_name != 'rec0':
                correct_var = am.group(2)
    return correct_var


def find_field_var_by_stage1(func_name, lines, fld_line, func_start,
                             stage1_cache):
    """Strategy 2: look up the correct field index from stage1 MLIR."""
    if not func_name or not stage1_cache:
        return None

    # Determine correct field index from stage1
    correct_idx = lookup_field_index(func_name, stage1_cache)
    if correct_idx is None:
        return None

    # Find the kk_field call at that index in stage2
    start = func_start or 0  # search entire function (not just 200 lines)
    constants = {}
    last_match = None
    for j in range(start, fld_line):
        cm = CONST_RE.search(lines[j])
        if cm:
            constants[cm.group(1)] = int(cm.group(2))
        fm = KK_FIELD_RE.search(lines[j])
        if fm:
            idx_v = fm.group(2)
            if idx_v in constants and constants[idx_v] == correct_idx:
                last_match = fm.group(1)  # Keep last (closest) match
    return last_match


def lookup_field_index(func_name, stage1_cache):
    """Find the field index a selector function returns in stage1 MLIR."""
    # Try both with and without frankenstein_ prefix
    candidates = [func_name]
    if func_name.startswith('frankenstein_'):
        candidates.append(func_name[len('frankenstein_'):])
    else:
        candidates.append('frankenstein_' + func_name)

    for lookup_name in candidates:
        for stage1_text in stage1_cache.values():
            func_re = re.compile(
                r'func\.func\s+@' + re.escape(lookup_name) +
                r'\(%(\w+):\s*i64\)\s*->\s*i64\s*\{(.*?)^\s*\}',
                re.MULTILINE | re.DOTALL)
            match = func_re.search(stage1_text)
            if not match:
                continue

            body = match.group(2)
            ret_m = re.search(r'func\.return\s+(%\w+)\s*:', body)
            if not ret_m:
                continue
            ret_var = ret_m.group(1)

            field_m = re.search(
                re.escape(ret_var) +
                r'\s*=\s*func\.call\s+@kk_field\(%\w+,\s*(%\w+)\)',
                body)
            if not field_m:
                continue
            idx_var = field_m.group(1)

            const_m = re.search(
                re.escape(idx_var) +
                r'\s*=\s*arith\.constant\s+(\d+)\s*:',
                body)
            if not const_m:
                continue

            return int(const_m.group(1))
    return None


def load_stage1_cache(obj_dir):
    """Load all stage1 MLIR files into memory for fast lookup."""
    cache = {}
    if not obj_dir:
        return cache
    for mlir_file in sorted(os.listdir(obj_dir)):
        if not mlir_file.endswith('.mlir'):
            continue
        # Skip stage2 subdirectory files if present
        mlir_path = os.path.join(obj_dir, mlir_file)
        if not os.path.isfile(mlir_path):
            continue
        try:
            with open(mlir_path) as f:
                cache[mlir_file] = f.read()
        except Exception:
            continue
    return cache


def fix_fld_refs(lines, obj_dir):
    """Fix @frankenstein_fld$0() calls by finding the correct field variable."""
    stage1_cache = load_stage1_cache(obj_dir)

    new_lines = []
    changes = 0
    i = 0
    while i < len(lines):
        m = FLD_CALL_RE.search(lines[i])
        if not m:
            new_lines.append(lines[i])
            i += 1
            continue

        fld_result_var = m.group(2)
        indent = m.group(1)

        # Find enclosing function
        func_name, func_start = find_enclosing_func(lines, i)

        # Strategy 1: look for corrupted alias comment
        correct_var = find_field_var_by_alias(lines, i, func_start)

        # Strategy 2: look up in stage1 MLIR
        if not correct_var:
            correct_var = find_field_var_by_stage1(
                func_name, lines, i, func_start, stage1_cache)

        if correct_var:
            # Check that the yield is on the next line
            if i + 1 < len(lines):
                yield_re = re.compile(
                    r'(\s*)scf\.yield\s+' +
                    re.escape(fld_result_var) + r'(?:\s*:\s*i64)?')
                ym = yield_re.search(lines[i + 1])
                if ym:
                    new_lines.append(
                        indent +
                        '// FIXED by fix-fld-refs.py: was '
                        '@frankenstein_fld$0()')
                    new_lines.append(
                        ym.group(1) + f'scf.yield {correct_var} : i64')
                    i += 2
                    changes += 1
                    continue

            # Also handle func.return on next line
            if i + 1 < len(lines):
                ret_re = re.compile(
                    r'(\s*)func\.return\s+' +
                    re.escape(fld_result_var) + r'\s*:\s*i64')
                rm = ret_re.search(lines[i + 1])
                if rm:
                    new_lines.append(
                        indent +
                        '// FIXED by fix-fld-refs.py: was '
                        '@frankenstein_fld$0()')
                    new_lines.append(
                        rm.group(1) + f'func.return {correct_var} : i64')
                    i += 2
                    changes += 1
                    continue

        # No fix found — keep original line
        new_lines.append(lines[i])
        i += 1

    return new_lines, changes


def main():
    if len(sys.argv) < 2:
        print("Usage: fix-fld-refs.py <file.mlir>", file=sys.stderr)
        sys.exit(1)

    path = sys.argv[1]
    with open(path) as f:
        text = f.read()

    # Check if there are any fld$0 references
    if 'frankenstein_fld$0' not in text:
        return

    lines = text.split('\n')

    # Remove private declaration for fld$0
    new_lines = []
    for line in lines:
        if re.match(
                r'\s*func\.func\s+private\s+@frankenstein_fld\$0\(',
                line):
            continue
        new_lines.append(line)
    lines = new_lines

    # Determine obj directory (stage1 MLIR location)
    stage2_dir = os.path.dirname(os.path.abspath(path))
    obj_dir = os.path.dirname(stage2_dir)
    if not os.path.isdir(obj_dir):
        obj_dir = None

    fixed_lines, n_changes = fix_fld_refs(lines, obj_dir)

    # Check for remaining unfixed fld$0 references
    remaining = sum(1 for l in fixed_lines
                    if 'func.call @frankenstein_fld$0()' in l)

    if n_changes > 0 or remaining > 0:
        print(f"  Fixed {n_changes} @frankenstein_fld$0 references"
              + (f" ({remaining} remaining)" if remaining else ""),
              file=sys.stderr)

    # Fix truncated 'func.return %' lines (missing SSA name).
    # The compiled emitter sometimes produces 'func.return %' instead of
    # 'func.return %vN : i64'.  Look for the preceding scf.if result var.
    trunc_fixes = 0
    out_lines = []
    for line in fixed_lines:
        m = re.match(r'^(\s*)func\.return\s+%\s*$', line)
        if m:
            indent = m.group(1)
            # Walk backwards to find the scf.if result variable
            for prev in reversed(out_lines):
                pm = re.match(r'\s*(%\w+)\s*=\s*scf\.if\b', prev)
                if pm:
                    out_lines.append(
                        f'{indent}func.return {pm.group(1)} : i64')
                    trunc_fixes += 1
                    break
            else:
                out_lines.append(line)
        else:
            out_lines.append(line)
    if trunc_fixes:
        print(f"  Fixed {trunc_fixes} truncated func.return line(s)",
              file=sys.stderr)
        fixed_lines = out_lines

    with open(path, 'w') as f:
        f.write('\n'.join(fixed_lines))


if __name__ == '__main__':
    main()
