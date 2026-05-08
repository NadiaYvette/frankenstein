#!/usr/bin/env python3
"""Fix func.call arity mismatches in MLIR output from the self-hosted compiler.

The self-hosted emitter has a bug where promoted let-bound lambdas can be
called with fewer arguments than their declaration (because captures are
computed in a scope where some free variables aren't available). This script
pads short calls with zero constants to match the declared parameter count.

Usage:
    python3 fix-mlir-arity.py input.mlir
    # Modifies the file in place
"""

import re
import sys


def fix_mlir_arity(path):
    with open(path) as f:
        content = f.read()

    lines = content.split('\n')

    # Pass 1: collect declared function arities and parameter names.
    # func.func @name(%p1: i64, %p2: i64) -> i64 {
    func_arity = {}
    func_params = {}  # name -> [param_name, ...] (without % prefix)
    func_decl_re = re.compile(
        r'func\.func\s+(?:private\s+)?@(\S+)\(([^)]*)\)')
    param_name_re = re.compile(r'%(\w+)\s*:')
    for line in lines:
        m = func_decl_re.search(line)
        if m:
            name = m.group(1)
            params_str = m.group(2).strip()
            if params_str:
                arity = len(params_str.split(','))
                pnames = [pm.group(1) for pm in param_name_re.finditer(params_str)]
            else:
                arity = 0
                pnames = []
            # Keep the first declaration (the actual definition)
            if name not in func_arity:
                func_arity[name] = arity
                func_params[name] = pnames

    # Pass 2: find func.call sites with wrong arg count and fix them.
    # %result = func.call @name(%a, %b) : (i64, i64) -> i64
    call_re = re.compile(
        r'(func\.call\s+@(\S+)\()([^)]*)(\)\s*:\s*\()([^)]*)(\)\s*->\s*i64)')

    fixes = 0
    new_lines = []
    zero_counter = [0]

    def fresh_zero():
        zero_counter[0] += 1
        return f"_arity_pad_{zero_counter[0]}"

    for line in lines:
        m = call_re.search(line)
        if m:
            fname = m.group(2)
            args_str = m.group(3).strip()
            types_str = m.group(5).strip()

            if fname in func_arity:
                expected = func_arity[fname]
                actual = len(args_str.split(',')) if args_str else 0

                if actual > expected and expected >= 0:
                    # Over-arity call: the self-hosted emitter inserted
                    # phantom capture args that are the function's own
                    # parameter names (not defined at the call site).
                    # Strip args that match the target function's param names.
                    call_args = [a.strip() for a in args_str.split(',')]
                    target_param_names = set(func_params.get(fname, []))
                    keep_args = []
                    removed = []
                    for arg in call_args:
                        # Extract the SSA name (strip % prefix)
                        arg_name = arg.lstrip('%').strip()
                        if arg_name in target_param_names and len(keep_args) + (len(call_args) - len(keep_args) - len(removed) - 1) >= expected:
                            # This arg matches a param name of the target fn
                            # and removing it gets us closer to the right arity
                            removed.append(arg)
                            target_param_names.discard(arg_name)
                        else:
                            keep_args.append(arg)

                    if len(keep_args) == expected:
                        new_args = ', '.join(keep_args)
                        new_types = ', '.join('i64' for _ in keep_args)
                        new_call = (m.group(1) + new_args + m.group(4)
                                    + new_types + m.group(6))
                        line = line[:m.start()] + new_call + line[m.end():]
                        fixes += 1
                        print(f"fix-mlir-arity: {path}: line {lines.index(line) + 1 if line in lines else '?'}: "
                              f"trimmed {fname} from {actual} to {expected} args "
                              f"(removed phantom params: {removed})",
                              file=sys.stderr)
                    else:
                        print(f"fix-mlir-arity: WARNING: {fname} call has {actual} args, "
                              f"expected {expected}, but could only trim to {len(keep_args)}",
                              file=sys.stderr)

                elif actual < expected and expected > 0:
                    # Need to pad with zeros. Insert zero constants before
                    # the call and add them as arguments.
                    deficit = expected - actual
                    pad_names = [fresh_zero() for _ in range(deficit)]

                    # Insert zero constant lines
                    indent = len(line) - len(line.lstrip())
                    indent_str = ' ' * indent
                    for pn in pad_names:
                        new_lines.append(
                            f"{indent_str}%{pn} = arith.constant 0 : i64")

                    # Build new arg and type lists
                    pad_args = ', '.join(f'%{pn}' for pn in pad_names)
                    pad_types = ', '.join('i64' for _ in pad_names)

                    if args_str:
                        new_args = f"{args_str}, {pad_args}"
                        new_types = f"{types_str}, {pad_types}"
                    else:
                        new_args = pad_args
                        new_types = pad_types

                    new_call = (m.group(1) + new_args + m.group(4)
                                + new_types + m.group(6))
                    line = line[:m.start()] + new_call + line[m.end():]
                    fixes += 1

        new_lines.append(line)

    if fixes > 0:
        with open(path, 'w') as f:
            f.write('\n'.join(new_lines))
        print(f"fix-mlir-arity: {path}: fixed {fixes} call(s)", file=sys.stderr)

    return fixes


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: fix-mlir-arity.py <file.mlir>", file=sys.stderr)
        sys.exit(1)

    total = 0
    for path in sys.argv[1:]:
        total += fix_mlir_arity(path)
    if total:
        print(f"Total: {total} arity fix(es) applied", file=sys.stderr)
