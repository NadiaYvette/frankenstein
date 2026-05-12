#!/usr/bin/env python3
"""Fix intra-module $N call mismatches in merged MLIR from split compilation.

When a module is split-compiled into parts, each part only sees its own
definitions in esTopFns. Calls to functions in other parts become "external"
references with $N suffixes. After merging, the definitions exist but the
$N symbols don't.

This script generates wrapper functions for each unresolved $N symbol that
has a matching definition in the same file.

Three cases:
  1. Simple alias: $N has same arity as definition → direct call wrapper
  2. Oversaturated: $N has MORE args than definition → call def, then apply
     extra args through the closure (extract fn ptr, indirect call)
  3. Undersaturated: $N has FEWER args than definition → should not happen
     in practice (the emitter would generate a PAP instead)

Usage: python3 fix-intra-module-calls.py <file.mlir>
"""

import re
import sys


def main():
    if len(sys.argv) < 2:
        print("Usage: fix-intra-module-calls.py <file.mlir>", file=sys.stderr)
        sys.exit(1)

    path = sys.argv[1]
    with open(path) as f:
        mlir = f.read()

    # Parse all func.func private @Name$N(...) -> i64 declarations
    # These are the unresolved $N references
    private_pattern = re.compile(
        r'^  func\.func private @(\w+)\$(\d+)\(([^)]*)\) -> i64$',
        re.MULTILINE
    )
    privates = {}
    for m in private_pattern.finditer(mlir):
        name = m.group(1)
        suffix = int(m.group(2))
        args_str = m.group(3).strip()
        nargs = len([a for a in args_str.split(',') if a.strip()]) if args_str else 0
        key = f"{name}${suffix}"
        privates[key] = (name, suffix, nargs, args_str)

    # Parse all func.func @Name(...) -> i64 { definitions
    # Include $ in name pattern to catch $N variants that are real definitions
    def_pattern = re.compile(
        r'^  func\.func @([\w$]+)\(([^)]*)\) -> i64 \{',
        re.MULTILINE
    )
    definitions = {}
    for m in def_pattern.finditer(mlir):
        name = m.group(1)
        args_str = m.group(2).strip()
        nargs = len([a for a in args_str.split(',') if a.strip()]) if args_str else 0
        definitions[name] = (nargs, args_str)

    # Generate wrappers
    wrappers = []
    removed_privates = set()

    for key, (name, suffix, call_nargs, call_args_str) in sorted(privates.items()):
        if name not in definitions:
            continue  # truly external, skip
        if key in definitions:
            # $N variant already defined as a real function — just remove the
            # private declaration so it doesn't conflict with the definition
            removed_privates.add(key)
            continue

        def_nargs, def_args_str = definitions[name]

        if def_nargs == call_nargs:
            # Case 1: Simple alias — same arity
            params = ", ".join(f"%a{i}: i64" for i in range(call_nargs))
            args = ", ".join(f"%a{i}" for i in range(call_nargs))
            if call_nargs == 0:
                wrapper = (
                    f"  func.func @{key}() -> i64 {{\n"
                    f"    %r = func.call @{name}() : () -> i64\n"
                    f"    func.return %r : i64\n"
                    f"  }}"
                )
            else:
                ty_list = ", ".join(["i64"] * call_nargs)
                wrapper = (
                    f"  func.func @{key}({params}) -> i64 {{\n"
                    f"    %r = func.call @{name}({args}) : ({ty_list}) -> i64\n"
                    f"    func.return %r : i64\n"
                    f"  }}"
                )
            wrappers.append(wrapper)
            removed_privates.add(key)

        elif call_nargs > def_nargs:
            # Case 2: Oversaturated — call the def with its args, get closure,
            # force thunk, extract fn ptr from field 0, indirect call with extras
            params = ", ".join(f"%a{i}: i64" for i in range(call_nargs))
            def_args = ", ".join(f"%a{i}" for i in range(def_nargs))
            extra_args = [f"%a{i}" for i in range(def_nargs, call_nargs)]

            if def_nargs == 0:
                def_call = f"    %raw = func.call @{name}() : () -> i64"
            else:
                def_ty = ", ".join(["i64"] * def_nargs)
                def_call = f"    %raw = func.call @{name}({def_args}) : ({def_ty}) -> i64"

            # Force thunk (safe no-op on non-thunks)
            force = "    %clos = func.call @kk_thunk_force(%raw) : (i64) -> i64"

            # Extract fn ptr from closure field 0
            extract = (
                "    %idx0 = arith.constant 0 : i64\n"
                "    %fptr_int = func.call @kk_field(%clos, %idx0) : (i64, i64) -> i64\n"
                "    %fptr_ptr = llvm.inttoptr %fptr_int : i64 to !llvm.ptr"
            )

            # Indirect call: fn_ptr(closure, extra_args...)
            all_clos_args = ["%clos"] + extra_args
            clos_args_str = ", ".join(all_clos_args)
            n_clos_args = len(all_clos_args)
            clos_ty = ", ".join(["i64"] * n_clos_args)
            indirect = (
                f"    %result = llvm.call %fptr_ptr({clos_args_str}) "
                f": !llvm.ptr, ({clos_ty}) -> i64"
            )

            wrapper = (
                f"  func.func @{key}({params}) -> i64 {{\n"
                f"{def_call}\n"
                f"{force}\n"
                f"{extract}\n"
                f"{indirect}\n"
                f"    func.return %result : i64\n"
                f"  }}"
            )
            wrappers.append(wrapper)
            removed_privates.add(key)

        elif call_nargs < def_nargs:
            # Case 3: Undersaturated — caller wants a partial application.
            # Build a CLOS closure that captures the supplied args and, when
            # fully applied, calls the original function.
            #
            # For the common sub-case (call 0 args, def N args): return a
            # closure whose fn ptr is a pap_wrapper that calls the def.
            # We need to generate both the $N wrapper AND a pap_wrapper.
            remaining = def_nargs - call_nargs
            pap_name = f"__pap_{key.replace('$', '_d')}"

            # PAP wrapper: takes (closure, remaining_args...) -> i64
            # Extracts captured args from closure fields 1..call_nargs,
            # then calls the original function with all args.
            pap_params = ["%clos_arg: i64"] + [f"%r{i}: i64" for i in range(remaining)]
            pap_params_str = ", ".join(pap_params)

            pap_body_lines = []
            call_args = []

            # Extract captured args from closure fields 1..call_nargs
            for i in range(call_nargs):
                idx_name = f"%cap_idx{i}"
                cap_name = f"%cap{i}"
                pap_body_lines.append(
                    f"    {idx_name} = arith.constant {i + 1} : i64")
                pap_body_lines.append(
                    f"    {cap_name} = func.call @kk_field(%clos_arg, {idx_name}) "
                    f": (i64, i64) -> i64")
                call_args.append(f"%cap{i}")

            # Add remaining args
            for i in range(remaining):
                call_args.append(f"%r{i}")

            call_args_str = ", ".join(call_args)
            call_ty = ", ".join(["i64"] * def_nargs)
            pap_body_lines.append(
                f"    %pap_result = func.call @{name}({call_args_str}) "
                f": ({call_ty}) -> i64")
            pap_body_lines.append("    func.return %pap_result : i64")

            pap_wrapper = (
                f"  func.func @{pap_name}({pap_params_str}) -> i64 {{\n"
                + "\n".join(pap_body_lines) + "\n"
                f"  }}"
            )
            wrappers.append(pap_wrapper)

            # Main $N wrapper: allocates a CLOS with fn ptr and captured args
            params = ", ".join(f"%a{i}: i64" for i in range(call_nargs))
            n_fields = 1 + call_nargs  # field 0 = fn ptr, fields 1..N = captured

            wrapper_lines = [
                f"  func.func @{key}({params}) -> i64 {{",
                f"    %clos_tag = arith.constant 1129074515 : i64",
                f"    %n_fields = arith.constant {n_fields} : i64",
                f"    %clos = func.call @kk_alloc_con(%clos_tag, %n_fields) "
                f": (i64, i64) -> i64",
            ]

            # Store fn ptr in field 0
            pap_fn_total_args = 1 + remaining  # closure + remaining args
            pap_fn_ty = ", ".join(["i64"] * pap_fn_total_args)
            wrapper_lines.extend([
                f"    %fptr = func.constant @{pap_name} "
                f": ({pap_fn_ty}) -> i64",
                f"    %fptr_llvm = builtin.unrealized_conversion_cast %fptr "
                f": ({pap_fn_ty}) -> i64 to !llvm.ptr",
                f"    %fptr_int = llvm.ptrtoint %fptr_llvm : !llvm.ptr to i64",
                f"    %idx0 = arith.constant 0 : i64",
                f"    func.call @kk_set_field(%clos, %idx0, %fptr_int) "
                f": (i64, i64, i64) -> ()",
            ])

            # Store captured args in fields 1..N
            for i in range(call_nargs):
                wrapper_lines.extend([
                    f"    %idx{i+1} = arith.constant {i + 1} : i64",
                    f"    func.call @kk_set_field(%clos, %idx{i+1}, %a{i}) "
                    f": (i64, i64, i64) -> ()",
                ])

            wrapper_lines.extend([
                f"    func.return %clos : i64",
                f"  }}"
            ])

            wrappers.append("\n".join(wrapper_lines))
            removed_privates.add(key)

    if not wrappers:
        return

    # Remove the private declarations for wrappers we're generating
    lines = mlir.split('\n')
    new_lines = []
    for line in lines:
        # Check if this line is a private declaration we're replacing
        m = re.match(r'^  func\.func private @(\w+\$\d+)\(', line)
        if m and m.group(1) in removed_privates:
            continue  # skip the private declaration
        new_lines.append(line)

    # Insert wrappers before the closing "}" of the module
    # Find the last "}" which closes the module
    result = '\n'.join(new_lines)

    # Insert wrappers before the final module closing brace
    wrapper_text = "\n\n  // Intra-module $N wrappers (generated by fix-intra-module-calls.py)\n"
    wrapper_text += "\n\n".join(wrappers)
    wrapper_text += "\n"

    # Find last "}" on its own line
    last_brace = result.rfind('\n}')
    if last_brace >= 0:
        result = result[:last_brace] + wrapper_text + result[last_brace:]
    else:
        result += wrapper_text

    with open(path, 'w') as f:
        f.write(result)

    n_simple = sum(1 for k in removed_privates
                   if privates[k][0] in definitions
                   and definitions[privates[k][0]][0] == privates[k][2])
    n_closure = len(removed_privates) - n_simple
    print(f"  Generated {len(removed_privates)} wrappers "
          f"({n_simple} simple, {n_closure} closure-apply)", file=sys.stderr)


if __name__ == '__main__':
    main()
