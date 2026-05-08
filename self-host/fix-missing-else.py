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
    # Pattern: func.func ... -> i64 { ... %r = scf.if ... -> i64 { ... } } (no func.return)
    ret_fixes = 0
    out = []
    for i, line in enumerate(lines):
        out.append(line)
        # Check: is this a lone `}` closing a func.func, and is the previous
        # non-empty/non-comment line a `}` closing an scf.if?
        stripped = line.strip()
        if stripped == '}':
            # Walk back to find the nearest scf.if result variable
            # The pattern is:  }  (closing scf.if else)  then  }  (closing func.func)
            prev_idx = len(out) - 2  # line before this }
            while prev_idx >= 0 and (not out[prev_idx].strip()
                                     or out[prev_idx].strip().startswith('//')):
                prev_idx -= 1
            if prev_idx >= 0 and out[prev_idx].strip() == '}':
                # The line before this } is another }. Check if it's closing an scf.if.
                # Walk backwards to find the matching scf.if
                for j in range(prev_idx - 1, max(prev_idx - 500, -1), -1):
                    sm = SCF_IF_RE.match(out[j])
                    if sm:
                        result_var = sm.group(2)
                        func_indent = sm.group(1)
                        # Insert func.return before the func closing }
                        out.pop()  # remove the func closing }
                        out.append(f'{func_indent}func.return {result_var} : i64')
                        out.append(line)  # re-add the func closing }
                        ret_fixes += 1
                        break
                    fm = FUNC_RE.match(out[j])
                    if fm:
                        break  # reached the func start, no scf.if found

    if fixes or ret_fixes:
        with open(path, 'w') as f:
            f.write('\n'.join(out))
        parts = []
        if fixes:
            parts.append(f"{fixes} scf.if block(s)")
        if ret_fixes:
            parts.append(f"{ret_fixes} func.return(s)")
        print(f"fix-missing-else: {path}: fixed {', '.join(parts)}",
              file=sys.stderr)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: fix-missing-else.py <file.mlir>", file=sys.stderr)
        sys.exit(1)
    fix_missing_else(sys.argv[1])
