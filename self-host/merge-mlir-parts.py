#!/usr/bin/env python3
"""Merge multiple MLIR split-compilation outputs into a single module.

Handles:
- String global renaming (@str_N → @str_pI_N) to avoid collisions
- Lambda/closure function renaming (@..._lambdaN → @..._lambda_pI_N)
- PAP wrapper renaming (@pap_... → @pap_pI_...)
- Header deduplication (extern decls, format globals)

Usage:
    python3 merge-mlir-parts.py part1.mlir part2.mlir ... -o merged.mlir
"""

import re
import sys


def merge_mlir(part_files, output_file):
    headers = []
    seen_headers = set()
    string_globals = []
    bodies = []

    for part_idx, part_file in enumerate(part_files):
        with open(part_file) as f:
            content = f.read()

        prefix = f"p{part_idx}"

        # Rename @str_N → @str_pI_N
        str_nums = set(re.findall(r'@str_(\d+)', content))
        for num in sorted(str_nums, key=int, reverse=True):
            content = content.replace(f"@str_{num}", f"@str_{prefix}_{num}")

        # Rename lambda functions: @..._lambdaN → @..._lambda_pI_N
        # Also handles closure names, cap names in SSA
        lambda_names = set(re.findall(r'@(\S*?lambda\d+)', content))
        for lname in sorted(lambda_names, key=len, reverse=True):
            # Extract the numeric suffix
            m = re.match(r'(.*)lambda(\d+)', lname)
            if m:
                new_name = f"{m.group(1)}lambda_{prefix}_{m.group(2)}"
                content = content.replace(f"@{lname}", f"@{new_name}")
                # Also rename SSA references like %clos1870 → %clos_pI_1870
                # These use the same counter as the lambda

        # Rename PAP wrappers: @pap_... that aren't module-qualified
        pap_names = set(re.findall(r'@(pap_\S+)', content))
        for pname in sorted(pap_names, key=len, reverse=True):
            content = content.replace(f"@{pname}", f"@{prefix}_{pname}")

        lines = content.split('\n')

        # Find module body range
        start_idx = 0
        end_idx = len(lines) - 1
        for i, line in enumerate(lines):
            if line.strip() == 'module {':
                start_idx = i + 1
                break
        for i in range(len(lines) - 1, -1, -1):
            if lines[i].strip() == '}':
                end_idx = i
                break

        for i in range(start_idx, end_idx):
            line = lines[i]
            stripped = line.strip()

            if stripped == '' or stripped.startswith('//'):
                continue

            if stripped.startswith('llvm.func @') or 'func.func private @' in stripped:
                if stripped not in seen_headers:
                    seen_headers.add(stripped)
                    headers.append(line)
                continue

            if 'llvm.mlir.global' in stripped and '@fmt_' in stripped:
                if stripped not in seen_headers:
                    seen_headers.add(stripped)
                    headers.append(line)
                continue

            if 'llvm.mlir.global' in stripped and '@str_' in stripped:
                string_globals.append(line)
                continue

            bodies.append(line)

    out = ['module {', '']
    if headers:
        out.append('  // External declarations')
        out.extend(headers)
        out.append('')
    if string_globals:
        out.append('  // String literals')
        out.extend(string_globals)
        out.append('')
    out.extend(bodies)
    out.append('')
    out.append('}')

    with open(output_file, 'w') as f:
        f.write('\n'.join(out))


if __name__ == '__main__':
    if len(sys.argv) < 4 or '-o' not in sys.argv:
        print("Usage: merge-mlir-parts.py part1.mlir part2.mlir ... -o merged.mlir",
              file=sys.stderr)
        sys.exit(1)

    o_idx = sys.argv.index('-o')
    part_files = sys.argv[1:o_idx]
    output_file = sys.argv[o_idx + 1]

    merge_mlir(part_files, output_file)
    print(f"Merged {len(part_files)} parts → {output_file}")
