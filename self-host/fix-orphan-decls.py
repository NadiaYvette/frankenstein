#!/usr/bin/env python3
"""Add missing private declarations for remaining $0 external references.

After fix-fld-refs.py and fix-dollar0-refs.py run, some $0 references may
remain unfixed while their private declarations were removed. This causes
mlir-opt to reject the module. This script re-adds declarations for any
function call that lacks a definition or declaration.

Usage: python3 fix-orphan-decls.py <file.mlir>
"""

import re
import sys


def main():
    if len(sys.argv) < 2:
        print("Usage: fix-orphan-decls.py <file.mlir>", file=sys.stderr)
        sys.exit(1)

    path = sys.argv[1]
    with open(path) as f:
        text = f.read()

    # Find all func.call @name(...) : (...) -> type
    call_re = re.compile(
        r'func\.call\s+@([A-Za-z0-9_.$]+)\s*\(([^)]*)\)\s*:\s*'
        r'\(([^)]*)\)\s*->\s*(\S+)')
    calls = {}  # name -> (param_types, return_type)
    for m in call_re.finditer(text):
        name = m.group(1)
        param_types = m.group(3).strip()
        ret_type = m.group(4)
        if name not in calls:
            calls[name] = (param_types, ret_type)

    # Find all defined or declared functions
    defined = set()
    for m in re.finditer(r'func\.func\s+(?:private\s+)?@([A-Za-z0-9_.$]+)\s*\(',
                         text):
        defined.add(m.group(1))

    # Also count func.constant references
    for m in re.finditer(r'func\.constant\s+@([A-Za-z0-9_.$]+)', text):
        name = m.group(1)
        if name not in calls:
            calls[name] = ('', 'i64')

    # Find missing declarations
    missing = {}
    for name, (param_types, ret_type) in calls.items():
        if name not in defined:
            missing[name] = (param_types, ret_type)

    if not missing:
        return

    # Insert declarations after 'module {' line
    lines = text.split('\n')
    insert_idx = None
    for i, line in enumerate(lines):
        if line.strip() == 'module {':
            insert_idx = i + 1
            break

    if insert_idx is None:
        return

    decls = []
    for name in sorted(missing):
        param_types, ret_type = missing[name]
        if param_types:
            decl = (f'  func.func private @{name}({param_types}) '
                    f'-> {ret_type}')
        else:
            decl = f'  func.func private @{name}() -> {ret_type}'
        decls.append(decl)

    lines[insert_idx:insert_idx] = decls

    with open(path, 'w') as f:
        f.write('\n'.join(lines))

    print(f"  Added {len(missing)} orphan declarations",
          file=sys.stderr)


if __name__ == '__main__':
    main()
