#!/usr/bin/env python3
"""Fix scf.if blocks that return values but are missing an else branch,
and fix functions that are missing their func.return statement.

MLIR requires scf.if to have an else block when it defines values (-> i64).
The compiled emitter sometimes omits the else block for single-constructor
pattern matches. This script adds a default else branch that yields 0.

It also detects functions where the last operation is an scf.if defining
a value but no func.return follows, and adds the missing return.

Usage: python3 fix-missing-else.py <file.mlir>
"""

import re
import sys


SCF_IF_RE = re.compile(r'^(\s*)(%\w+)\s*=\s*scf\.if\s+%\w+\s*->\s*i64\s*\{')
FUNC_RE = re.compile(r'^(\s*)func\.func\s+@\S+\(.*\)\s*->\s*i64\s*\{')
TRUNCATED_SCF_RE = re.compile(r'^(\s*)scf\.\s*$')
SSA_DEF_RE = re.compile(r'^\s*(%\w+)\s*=')


def fix_truncated_scf(lines):
    """Fix truncated 'scf.' lines (should be 'scf.yield %vN : i64')."""
    fixes = 0
    for i, line in enumerate(lines):
        m = TRUNCATED_SCF_RE.match(line)
        if not m:
            continue
        indent = m.group(1)
        # Find the SSA value from the previous line
        prev_var = None
        for j in range(i - 1, max(i - 5, -1), -1):
            pm = SSA_DEF_RE.match(lines[j])
            if pm:
                prev_var = pm.group(1)
                break
        if prev_var:
            lines[i] = f'{indent}scf.yield {prev_var} : i64'
            fixes += 1
    return fixes


def fix_missing_else(path):
    with open(path) as f:
        text = f.read()

    lines = text.split('\n')
    fixes = 0

    # Find all scf.if -> i64 that are missing else blocks.
    # We process from end to start so line insertions don't shift indices.
    insertions = []  # list of (line_index_of_closing_brace, indent, result_var)

    for i, line in enumerate(lines):
        m = SCF_IF_RE.match(line)
        if not m:
            continue

        indent = m.group(1)
        result_var = m.group(2)

        # Find matching closing } of the then-block by tracking brace depth
        depth = 0
        then_close = None
        for j in range(i, len(lines)):
            for ch in lines[j]:
                if ch == '{':
                    depth += 1
                elif ch == '}':
                    depth -= 1
                    if depth == 0:
                        then_close = j
                        break
            if then_close is not None:
                break

        if then_close is None:
            continue

        # Check if this closing line contains 'else'
        close_line = lines[then_close].strip()
        if 'else' in close_line:
            # Already has else block, skip
            continue

        # This scf.if -> i64 is missing its else block. Record for insertion.
        insertions.append((then_close, indent, result_var))

    # Apply insertions from end to start
    for close_idx, indent, result_var in reversed(insertions):
        else_lines = [
            f'{indent}}} else {{',
            f'{indent}  %_else_default_{fixes} = arith.constant 0 : i64',
            f'{indent}  scf.yield %_else_default_{fixes} : i64',
            f'{indent}}}',
        ]
        # Replace the closing } line with } else { ... }
        lines[close_idx:close_idx + 1] = else_lines
        fixes += 1

    # Second pass: fix functions missing func.return after scf.if -> i64.
    # Walk through func.func -> i64 blocks and check if func.return is present.
    ret_fixes = 0
    out = lines[:]
    i = 0
    while i < len(out):
        fm = FUNC_RE.match(out[i])
        if not fm:
            i += 1
            continue

        func_indent = fm.group(1)
        func_start = i

        # Find the closing } of this function by brace-matching
        depth = 0
        func_end = None
        for j in range(i, len(out)):
            for ch in out[j]:
                if ch == '{':
                    depth += 1
                elif ch == '}':
                    depth -= 1
                    if depth == 0:
                        func_end = j
                        break
            if func_end is not None:
                break

        if func_end is None:
            i += 1
            continue

        # Check if there's a func.return in this function body
        has_return = False
        last_scf_if_var = None
        for j in range(func_start + 1, func_end):
            if 'func.return' in out[j]:
                has_return = True
                break
            sm = SCF_IF_RE.match(out[j])
            if sm:
                last_scf_if_var = sm.group(2)

        if not has_return and last_scf_if_var:
            # Insert func.return before the closing }
            ret_line = f'{func_indent}  func.return {last_scf_if_var} : i64'
            out.insert(func_end, ret_line)
            ret_fixes += 1

        i = func_end + 1

    # Third pass: fix truncated 'scf.' lines
    trunc_fixes = fix_truncated_scf(out)

    if fixes or ret_fixes or trunc_fixes:
        with open(path, 'w') as f:
            f.write('\n'.join(out))
        parts = []
        if fixes:
            parts.append(f"{fixes} scf.if block(s)")
        if ret_fixes:
            parts.append(f"{ret_fixes} func.return(s)")
        if trunc_fixes:
            parts.append(f"{trunc_fixes} truncated scf.yield(s)")
        print(f"fix-missing-else: {path}: fixed {', '.join(parts)}",
              file=sys.stderr)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: fix-missing-else.py <file.mlir>", file=sys.stderr)
        sys.exit(1)
    fix_missing_else(sys.argv[1])
