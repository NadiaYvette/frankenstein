#!/usr/bin/env python3
"""Extract specific functions from a stage1 MLIR file.

When a split-compiled part crashes the self-hosted compiler, this script
extracts the needed functions from the host-compiled stage1 MLIR and writes
them as a replacement part that can be merged with the other parts.

Transitively includes lambda/closure/PAP functions referenced by the
extracted functions.

Usage:
    python3 extract-mlir-funcs.py stage1.mlir part.organ.json -o output.mlir
"""

import json
import re
import sys


def get_def_names(organ_json_path):
    """Extract mangled function names from an OrganIR JSON file."""
    with open(organ_json_path) as f:
        data = json.load(f)

    names = set()
    for defn in data['module']['definitions']:
        text = defn['name']['name']['text']
        mangled = 'frankenstein_' + text.replace('.', '_')
        names.add(mangled)
    return names


def parse_mlir_functions(mlir_path):
    """Parse all function definitions from an MLIR file into a dict."""
    with open(mlir_path) as f:
        lines = f.readlines()

    functions = {}  # name -> (body_text, set of referenced @names)
    decl_lines = {}  # name -> declaration line
    global_lines = {}  # name -> global definition line

    i = 0
    while i < len(lines):
        line = lines[i]

        # Match llvm.mlir.global definitions (string literals, format strings)
        gm = re.search(r'llvm\.mlir\.global\s+\S+\s+\S+\s+@([A-Za-z0-9_.$]+)\s*\(', line)
        if gm:
            global_lines[gm.group(1)] = line.rstrip()
            i += 1
            continue

        # Match private declarations
        m = re.match(r'\s*func\.func\s+private\s+@(\S+)\s*\(', line)
        if m:
            decl_lines[m.group(1)] = line.rstrip()
            i += 1
            continue

        # Match function definitions
        m = re.match(r'\s*func\.func\s+@(\S+)\s*\(', line)
        if m:
            fname = m.group(1)
            func_lines = [line]
            depth = line.count('{') - line.count('}')
            i += 1
            while i < len(lines) and depth > 0:
                func_lines.append(lines[i])
                depth += lines[i].count('{') - lines[i].count('}')
                i += 1
            body = ''.join(func_lines)
            # Find all @name references in the body
            refs = set(re.findall(r'@([A-Za-z0-9_.$]+)', body))
            refs.discard(fname)  # don't self-reference
            functions[fname] = (body, refs)
            continue

        i += 1

    return functions, decl_lines, global_lines


def transitive_closure(functions, seed_names):
    """Find all functions transitively referenced by seed_names."""
    needed = set()
    queue = list(seed_names)

    while queue:
        name = queue.pop()
        if name in needed:
            continue
        if name not in functions:
            # Try stripping arity suffix
            base = re.sub(r'\$\d+$', '', name)
            if base in functions:
                name = base
            else:
                continue
        needed.add(name)
        _, refs = functions[name]
        for ref in refs:
            if ref not in needed and ref in functions:
                queue.append(ref)

    return needed


def main():
    if len(sys.argv) < 5 or '-o' not in sys.argv:
        print("Usage: extract-mlir-funcs.py stage1.mlir part.organ.json "
              "-o output.mlir", file=sys.stderr)
        sys.exit(1)

    mlir_path = sys.argv[1]
    organ_json_path = sys.argv[2]
    o_idx = sys.argv.index('-o')
    output_path = sys.argv[o_idx + 1]

    target_names = get_def_names(organ_json_path)
    functions, decl_lines, global_lines = parse_mlir_functions(mlir_path)

    # Match target names to actual function names
    matched = set()
    for name in target_names:
        if name in functions:
            matched.add(name)
        else:
            # Try with arity suffixes
            for fname in functions:
                base = re.sub(r'\$\d+$', '', fname)
                if base == name:
                    matched.add(fname)

    # Transitively include referenced functions (lambdas, PAPs, closures)
    all_needed = transitive_closure(functions, matched)

    # Collect output
    out_globals = []
    out_decls = []
    out_funcs = []
    for name in sorted(all_needed):
        if name in functions:
            out_funcs.append(functions[name][0].rstrip())
        if name in decl_lines:
            out_decls.append(decl_lines[name])

    # Also include declarations for external references
    for name in sorted(all_needed):
        if name in functions:
            _, refs = functions[name]
            for ref in refs:
                if ref not in all_needed and ref in decl_lines:
                    if decl_lines[ref] not in out_decls:
                        out_decls.append(decl_lines[ref])
                # Include any referenced globals (string literals, format strings)
                if ref in global_lines:
                    if global_lines[ref] not in out_globals:
                        out_globals.append(global_lines[ref])

    # Write as a valid MLIR module
    out = ['module {', '']
    if out_globals:
        out.append('  // Global constants (extracted from stage1)')
        out.extend(f'  {g.strip()}' if not g.startswith('  ') else g
                   for g in out_globals)
        out.append('')
    if out_decls:
        out.append('  // Declarations (extracted from stage1)')
        out.extend(f'  {d.strip()}' if not d.startswith('  ') else d
                   for d in out_decls)
        out.append('')
    if out_funcs:
        out.append('  // Functions (extracted from stage1)')
        for f in out_funcs:
            out.append(f)
            out.append('')
    out.append('}')

    with open(output_path, 'w') as f:
        f.write('\n'.join(out))

    n_direct = len(matched)
    n_transitive = len(all_needed) - n_direct
    print(f"  Extracted {n_direct} direct + {n_transitive} transitive "
          f"functions from stage1 MLIR ({len(target_names)} requested)",
          file=sys.stderr)


if __name__ == '__main__':
    main()
