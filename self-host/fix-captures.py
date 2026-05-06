#!/usr/bin/env python3
"""Post-processor for MLIR files emitted by the self-hosted Frankenstein compiler.

Detects lambda functions that reference SSA values from enclosing scopes
(escaped references) and patches them to properly capture those values
via the closure ABI.

The fix involves:
1. Adding kk_field extraction to the function prologue
2. Replacing escaped SSA refs with the new capture names
3. Updating the closure allocation in the enclosing function
4. Updating func.func signature and func.constant type
"""

import re
import sys
from collections import defaultdict

# Match func.func definitions
FUNC_RE = re.compile(r'^  func\.func @(\S+)\(([^)]*)\) -> (\S+) \{')
# Match SSA definitions: %name = ...
DEF_RE = re.compile(r'%(\w+)\s*=')
# Match SSA references: %name in various contexts
REF_RE = re.compile(r'%(\w+)')
# Match closure allocation comments
CLOSURE_COMMENT_RE = re.compile(r'// closure for @(\S+) capturing (\d+) vars')
# Match kk_alloc_con calls
ALLOC_CON_RE = re.compile(r'(%\w+) = func\.call @kk_alloc_con\((%\w+), (%\w+)\)')
# Match kk_set_field calls
SET_FIELD_RE = re.compile(r'func\.call @kk_set_field\((%\w+), (%\w+), (%\w+)\)')
# Match func.constant for lambda
FUNC_CONST_RE = re.compile(r'(%\w+) = func\.constant @(\S+) : \(([^)]+)\) -> (\S+)')
# Match arith.constant for field count
ARITH_CONST_RE = re.compile(r'(%\w+) = arith\.constant (\d+) : i64')

def parse_params(param_str):
    """Parse function parameters like '%clos: i64, %p1: i64' into list of names."""
    if not param_str.strip():
        return []
    params = []
    for p in param_str.split(','):
        p = p.strip()
        m = re.match(r'%(\w+)', p)
        if m:
            params.append(m.group(1))
    return params

def find_ssa_refs(line):
    """Find all SSA references in a line, excluding definitions."""
    # Skip comment-only lines
    stripped = line.strip()
    if stripped.startswith('//'):
        return set()

    refs = set()
    # Find all %name references
    for m in REF_RE.finditer(line):
        refs.add(m.group(1))

    # Remove the defined name (if this line is a definition)
    dm = DEF_RE.match(stripped)
    if dm:
        refs.discard(dm.group(1))

    return refs

def find_ssa_defs(line):
    """Find SSA definitions in a line."""
    stripped = line.strip()
    if stripped.startswith('//'):
        return set()
    defs = set()
    dm = DEF_RE.match(stripped)
    if dm:
        defs.add(dm.group(1))
    return defs

def parse_functions(lines):
    """Parse MLIR into function blocks with their SSA defs and refs."""
    functions = {}  # name -> {start, end, params, defs, refs, lines}
    i = 0
    while i < len(lines):
        m = FUNC_RE.match(lines[i])
        if m:
            fname = m.group(1)
            params = parse_params(m.group(2))
            func_start = i
            # Find matching closing brace
            depth = 1
            j = i + 1
            while j < len(lines) and depth > 0:
                # Count braces (simplified — MLIR uses { and })
                for ch in lines[j]:
                    if ch == '{':
                        depth += 1
                    elif ch == '}':
                        depth -= 1
                        if depth == 0:
                            break
                j += 1
            func_end = j  # line after closing }

            # Collect all defs and refs in function body
            all_defs = set(params)
            all_refs = set()
            for k in range(func_start + 1, func_end):
                all_defs |= find_ssa_defs(lines[k])
                all_refs |= find_ssa_refs(lines[k])

            # Escaped refs: referenced but not defined locally
            escaped = all_refs - all_defs
            # Filter out non-SSA names (module-level params like %ds..., %k... are SSA too)
            # Keep anything that looks like an SSA value (has digits) and isn't a keyword
            escaped = {r for r in escaped if re.search(r'\d', r)
                       and not r.startswith('str_')}

            functions[fname] = {
                'start': func_start,
                'end': func_end,
                'params': params,
                'defs': all_defs,
                'refs': all_refs,
                'escaped': escaped,
                'lines': lines[func_start:func_end],
            }
            i = func_end
        else:
            i += 1
    return functions

def find_closure_sites(lines):
    """Find where closures are allocated: comment + alloc_con + set_field sequences."""
    sites = {}  # lambda_name -> {comment_line, alloc_line, set_fields, nfields, ptr_name, ...}
    i = 0
    while i < len(lines):
        m = CLOSURE_COMMENT_RE.search(lines[i])
        if m:
            lname = m.group(1)
            ncap = int(m.group(2))

            # Scan forward for the allocation pattern
            site = {
                'comment_line': i,
                'ncap_original': ncap,
                'alloc_line': None,
                'nfields_line': None,
                'nfields_name': None,
                'ptr_name': None,
                'set_fields': [],  # (line_idx, field_idx_name, value_name)
                'func_const_line': None,
                'func_const_name': None,
                'func_const_type': None,
                'last_set_field_line': None,
            }

            for j in range(i + 1, min(i + 30, len(lines))):
                # Find nfields constant (the line before kk_alloc_con, usually ncap+1)
                am = ARITH_CONST_RE.search(lines[j])
                ac = ALLOC_CON_RE.search(lines[j])
                sf = SET_FIELD_RE.search(lines[j])
                fc = FUNC_CONST_RE.search(lines[j])

                if ac:
                    site['alloc_line'] = j
                    site['ptr_name'] = ac.group(1)
                    # The nfields arg is the second arg to kk_alloc_con
                    # Find the line that defines that arg
                    nfields_ssa = ac.group(3)
                    for k in range(j - 1, max(j - 5, i), -1):
                        if nfields_ssa + ' = arith.constant' in lines[k]:
                            nm = ARITH_CONST_RE.search(lines[k])
                            if nm and nm.group(1) == nfields_ssa:
                                site['nfields_line'] = k
                                site['nfields_name'] = nfields_ssa
                            break

                if sf and site['ptr_name'] and sf.group(1) == site['ptr_name']:
                    site['set_fields'].append((j, sf.group(2), sf.group(3)))
                    site['last_set_field_line'] = j

                if fc and fc.group(2) == lname:
                    site['func_const_line'] = j
                    site['func_const_name'] = fc.group(1)
                    site['func_const_type'] = fc.group(3)

                # Stop scanning if we hit another function or closure comment
                if j > i + 2 and ('func.func @' in lines[j] or
                    ('// closure for @' in lines[j] and j != i)):
                    break
                # Stop after we've found set_fields and hit a non-set_field non-related line
                if site['set_fields'] and not sf and 'kk_set_field' not in lines[j] and j > site['last_set_field_line'] + 1:
                    break

            sites[lname] = site
            i += 1
        else:
            i += 1
    return sites

def get_next_ssa_counter(lines):
    """Find the highest SSA counter used in the file."""
    max_n = 0
    for line in lines:
        for m in REF_RE.finditer(line):
            name = m.group(1)
            # Extract trailing number
            nm = re.match(r'[a-z_]+(\d+)', name)
            if nm:
                max_n = max(max_n, int(nm.group(1)))
    return max_n + 1

def fix_mlir(input_path, output_path=None):
    """Main fix: detect escaped SSA refs and patch closures."""
    with open(input_path) as f:
        lines = f.readlines()

    # Strip newlines for easier processing, add back later
    lines = [l.rstrip('\n') for l in lines]

    functions = parse_functions(lines)

    # Find functions with escaped references
    problems = {}
    for fname, finfo in functions.items():
        if finfo['escaped']:
            problems[fname] = finfo['escaped']

    if not problems:
        if output_path:
            with open(output_path, 'w') as f:
                f.write('\n'.join(lines) + '\n')
        return 0

    # Report problems
    print(f"fix-captures: {input_path}: found {len(problems)} functions with escaped SSA refs:", file=sys.stderr)
    for fname, escaped in problems.items():
        print(f"  {fname}: escaped refs: {', '.join(sorted(escaped))}", file=sys.stderr)

    # Find closure allocation sites
    closure_sites = find_closure_sites(lines)

    # Get next available SSA counter
    counter = get_next_ssa_counter(lines)

    def fresh(prefix):
        nonlocal counter
        name = f"{prefix}{counter}"
        counter += 1
        return name

    # Plan fixes: for each problematic function, determine what captures to add
    fixes = []
    for fname, escaped in problems.items():
        finfo = functions[fname]

        if fname not in closure_sites:
            print(f"  WARNING: no closure site found for {fname}, skipping", file=sys.stderr)
            continue

        site = closure_sites[fname]

        # For each escaped ref, we need to:
        # 1. Add it as a captured field in the closure
        # 2. Add kk_field extraction in the function prologue
        # 3. Replace references in the function body

        new_captures = sorted(escaped)  # sorted for determinism
        fixes.append({
            'fname': fname,
            'finfo': finfo,
            'site': site,
            'new_captures': new_captures,
        })

    if not fixes:
        if output_path:
            with open(output_path, 'w') as f:
                f.write('\n'.join(lines) + '\n')
        return 0

    # Apply fixes in reverse order (bottom of file first) to preserve line numbers
    # We need to:
    # a) Modify func.func signature (no change needed — captures aren't params)
    # b) Add kk_field extractions to prologue
    # c) Replace escaped refs in body with new capture names
    # d) Update closure allocation: nfields, add set_field calls
    # e) Update func.constant type (no change — captures aren't in type)

    # Sort fixes by function start line (descending) for safe line insertion
    fixes.sort(key=lambda f: f['finfo']['start'], reverse=True)

    for fix in fixes:
        fname = fix['fname']
        finfo = fix['finfo']
        site = fix['site']
        new_captures = fix['new_captures']
        ncap_orig = site['ncap_original']
        ncap_new = ncap_orig + len(new_captures)
        nfields_new = ncap_new + 1  # field 0 = fptr

        print(f"  Fixing {fname}: adding {len(new_captures)} captures: {new_captures}", file=sys.stderr)

        # === Fix the function body ===
        # Add kk_field extractions right after existing cap extractions
        # Find the last idx_cap/cap line in the function prologue
        func_start = finfo['start']
        func_end = finfo['end']

        last_cap_line = func_start  # default: right after func.func line
        for k in range(func_start + 1, min(func_start + 50, func_end)):
            if 'kk_field(%clos' in lines[k] or 'idx_cap' in lines[k]:
                last_cap_line = k

        # Generate new capture extraction lines
        new_prologue_lines = []
        replacement_map = {}  # old_name -> new_cap_name
        clos_param = finfo['params'][0]  # first param is always closure

        for i, esc_ref in enumerate(new_captures):
            field_idx = ncap_orig + 1 + i  # 1-indexed (field 0 = fptr)
            idx_name = fresh("idx_cap")
            cap_name = fresh("cap")
            new_prologue_lines.append(
                f"    %{idx_name} = arith.constant {field_idx} : i64"
            )
            new_prologue_lines.append(
                f"    %{cap_name} = func.call @kk_field(%{clos_param}, %{idx_name}) : (i64, i64) -> i64"
            )
            replacement_map[esc_ref] = cap_name

        # Insert prologue lines after last_cap_line
        for j, pl in enumerate(new_prologue_lines):
            lines.insert(last_cap_line + 1 + j, pl)

        # Adjust line numbers for subsequent operations
        n_inserted_prologue = len(new_prologue_lines)

        # Replace escaped refs in function body (after prologue, before func.return)
        # Need to re-calculate func_end after insertion
        func_end += n_inserted_prologue

        for k in range(last_cap_line + 1 + n_inserted_prologue, func_end):
            for old_name, new_name in replacement_map.items():
                # Use word-boundary-aware replacement
                lines[k] = re.sub(
                    r'%' + re.escape(old_name) + r'(?=[\s,):])',
                    '%' + new_name,
                    lines[k]
                )

        # === Fix the closure allocation site ===
        # The site is in the ENCLOSING function, which is above the current function.
        # After our prologue insertion, site line numbers need adjustment if site is
        # after the insertion point (it shouldn't be — site is in enclosing func).

        # Check if site lines are before or after our insertion
        site_comment = site['comment_line']
        if site_comment > last_cap_line:
            # Site is after insertion — adjust
            site_adj = n_inserted_prologue
        else:
            site_adj = 0

        # Update the nfields constant
        if site['nfields_line'] is not None:
            nf_line = site['nfields_line'] + site_adj
            lines[nf_line] = re.sub(
                r'arith\.constant \d+ : i64',
                f'arith.constant {nfields_new} : i64',
                lines[nf_line]
            )

        # Update the closure comment
        comment_line = site['comment_line'] + site_adj
        lines[comment_line] = lines[comment_line].replace(
            f'capturing {ncap_orig} vars',
            f'capturing {ncap_new} vars'
        )

        # Add new kk_set_field calls after the last existing one
        if site['last_set_field_line'] is not None:
            insert_after = site['last_set_field_line'] + site_adj
        else:
            # No existing set_fields? Insert after alloc_con
            insert_after = site['alloc_line'] + site_adj

        ptr_name = site['ptr_name']
        new_set_field_lines = []
        for i, esc_ref in enumerate(new_captures):
            field_idx = ncap_orig + 1 + i  # 1-indexed
            idx_name = fresh("v")
            new_set_field_lines.append(
                f"    %{idx_name} = arith.constant {field_idx} : i64"
            )
            new_set_field_lines.append(
                f"    func.call @kk_set_field({ptr_name}, %{idx_name}, %{esc_ref}) : (i64, i64, i64) -> ()"
            )

        for j, sf_line in enumerate(new_set_field_lines):
            lines.insert(insert_after + 1 + j, sf_line)

    # Write output
    out = output_path or input_path
    with open(out, 'w') as f:
        f.write('\n'.join(lines) + '\n')

    return len(fixes)


# --- Arity mismatch fix ---

FUNC_CALL_RE = re.compile(
    r'func\.call @(\S+)\(([^)]*)\)\s*:\s*\(([^)]*)\)\s*->\s*(\S+)'
)

def fix_arity_mismatches(input_path, output_path=None):
    """Fix func.call sites where arg count != callee's param count.

    This happens when the self-hosted compiler promotes a recursive let
    to a top-level function but emits the call with fewer args than the
    function expects — the missing args correspond to free variables from
    the enclosing scope that were not threaded through.
    """
    with open(input_path) as f:
        lines = [l.rstrip('\n') for l in f.readlines()]

    # Pass 1: collect function definitions (name -> param names, param count)
    func_defs = {}  # name -> [param_name, ...]
    for line in lines:
        m = FUNC_RE.match(line)
        if m:
            fname = m.group(1)
            params = parse_params(m.group(2))
            func_defs[fname] = params

    # Pass 2: find calls with arity mismatch
    mismatches = []  # (line_idx, func_name, call_args, expected_params)
    for i, line in enumerate(lines):
        m = FUNC_CALL_RE.search(line)
        if m:
            fname = m.group(1)
            if fname not in func_defs:
                continue  # external function — skip
            call_args_str = m.group(2).strip()
            if not call_args_str:
                call_args = []
            else:
                call_args = [a.strip() for a in call_args_str.split(',')]
            expected = func_defs[fname]
            if len(call_args) != len(expected):
                mismatches.append((i, fname, call_args, expected))

    if not mismatches:
        if output_path and output_path != input_path:
            with open(output_path, 'w') as f:
                f.write('\n'.join(lines) + '\n')
        return 0

    print(f"fix-arity: {input_path}: found {len(mismatches)} arity mismatches:", file=sys.stderr)
    for line_idx, fname, call_args, expected in mismatches:
        print(f"  line {line_idx+1}: {fname}: {len(call_args)} args, expected {len(expected)}", file=sys.stderr)

    # Pass 3: collect "let" comments that map GHC names to SSA values
    # Pattern: // let name = %ssa_value
    let_comment_re = re.compile(r'//\s*let\s+(\S+)\s*=\s*%(\w+)')

    # Build a scope map: for each line, what GHC names are defined above it
    # We only need this for lines with mismatches
    # Scan the enclosing function to find let-comment bindings

    fixes_applied = 0

    for line_idx, fname, call_args, expected_params in mismatches:
        # Determine which params are missing
        # Strategy: the call args are a subsequence of expected params.
        # Match them positionally and find gaps.
        n_call = len(call_args)
        n_exp = len(expected_params)
        if n_call >= n_exp:
            continue  # shouldn't happen, but skip

        # Try to find the mapping: which expected param positions are provided?
        # Build a map from SSA values to GHC names using let-comments above call site.
        n_missing = n_exp - n_call

        # Find enclosing function first (needed for let-comment scanning)
        enclosing_func_line_pre = None
        for k in range(line_idx - 1, -1, -1):
            fm = FUNC_RE.match(lines[k])
            if fm:
                enclosing_func_line_pre = k
                break

        # Collect SSA-to-name mappings from let-comments
        ssa_to_name = {}
        if enclosing_func_line_pre is not None:
            for k in range(enclosing_func_line_pre + 1, line_idx):
                lm = let_comment_re.search(lines[k])
                if lm:
                    ssa_to_name[lm.group(2)] = lm.group(1)
            # Also include function params directly
            fm = FUNC_RE.match(lines[enclosing_func_line_pre])
            if fm:
                for pn in parse_params(fm.group(2)):
                    ssa_to_name[pn] = pn

        # Try to identify which call args correspond to which expected params
        # by matching SSA value names to expected param names
        call_arg_names = []
        for ca in call_args:
            ssa = ca.lstrip('%').split(':')[0].strip()
            call_arg_names.append(ssa_to_name.get(ssa))

        # Try all possible positions for the missing args (contiguous block)
        best_match = None
        best_score = -1
        for skip_start in range(n_exp - n_missing + 1):
            remaining_positions = [j for j in range(n_exp) if j < skip_start or j >= skip_start + n_missing]
            if len(remaining_positions) != n_call:
                continue
            # Score: how many call args match expected param names at their positions
            score = 0
            for ci, pi in enumerate(remaining_positions):
                if call_arg_names[ci] is not None and call_arg_names[ci] == expected_params[pi]:
                    score += 1
            if score > best_score:
                best_score = score
                best_match = (skip_start, skip_start + n_missing)

        if best_match is None:
            print(f"  WARNING: cannot determine missing param positions for {fname} at line {line_idx+1}", file=sys.stderr)
            continue

        skip_start, skip_end = best_match
        missing_params = expected_params[skip_start:skip_end]
        print(f"  Missing params at positions {skip_start}-{skip_end-1}: {missing_params}", file=sys.stderr)

        # Find SSA values for the missing params.
        # Strategy 1: Search let-comments and enclosing function params.
        # Strategy 2: Use a correct call site as template.
        missing_values = {}

        # Strategy 1: let-comments and enclosing params
        enclosing_func_line = None
        for k in range(line_idx - 1, -1, -1):
            fm = FUNC_RE.match(lines[k])
            if fm:
                enclosing_func_line = k
                enc_params = parse_params(fm.group(2))
                for mp in missing_params:
                    if mp in enc_params:
                        missing_values[mp] = mp
                break

        if enclosing_func_line is not None:
            for k in range(enclosing_func_line + 1, line_idx):
                lm = let_comment_re.search(lines[k])
                if lm:
                    let_name = lm.group(1)
                    let_value = lm.group(2)
                    for mp in missing_params:
                        if mp not in missing_values and mp == let_name:
                            missing_values[mp] = let_value

        # Strategy 2: find a correct call to the same function and use its args
        if len(missing_values) != len(missing_params):
            # Look for other calls to the same function with correct arity
            for k, line2 in enumerate(lines):
                if k == line_idx:
                    continue
                m2 = FUNC_CALL_RE.search(line2)
                if m2 and m2.group(1) == fname:
                    ref_args_str = m2.group(2).strip()
                    if not ref_args_str:
                        ref_args = []
                    else:
                        ref_args = [a.strip() for a in ref_args_str.split(',')]
                    if len(ref_args) == n_exp:
                        # Found a correct call — extract the values for missing positions
                        for pi in range(skip_start, skip_end):
                            mp = expected_params[pi]
                            if mp not in missing_values:
                                ref_ssa = ref_args[pi].lstrip('%').split(':')[0].strip()
                                # Check if this SSA value is in scope at the call site
                                # (defined before the call site, in the same function)
                                if enclosing_func_line is not None:
                                    for k2 in range(enclosing_func_line, line_idx):
                                        if f'%{ref_ssa} ' in lines[k2] or f'%{ref_ssa},' in lines[k2]:
                                            missing_values[mp] = ref_ssa
                                            break
                        break

        if len(missing_values) != len(missing_params):
            unfound = [mp for mp in missing_params if mp not in missing_values]
            print(f"  WARNING: cannot find SSA values for: {unfound}", file=sys.stderr)
            continue

        # Build the new args list
        new_args = []
        call_idx = 0
        for j in range(n_exp):
            if skip_start <= j < skip_end:
                pname = expected_params[j]
                new_args.append('%' + missing_values[pname])
            else:
                new_args.append(call_args[call_idx])
                call_idx += 1

        # Rebuild the type signature
        new_type_args = ', '.join(['i64'] * n_exp)
        old_type_args = ', '.join(['i64'] * n_call)

        # Replace the call in the line
        old_call_args_str = ', '.join(call_args)
        new_call_args_str = ', '.join(new_args)
        old_pattern = f'func.call @{fname}({old_call_args_str}) : ({old_type_args})'
        new_pattern = f'func.call @{fname}({new_call_args_str}) : ({new_type_args})'
        if old_pattern in lines[line_idx]:
            lines[line_idx] = lines[line_idx].replace(old_pattern, new_pattern)
            print(f"  Fixed call at line {line_idx+1}", file=sys.stderr)
            fixes_applied += 1
        else:
            print(f"  WARNING: could not find exact call pattern at line {line_idx+1}", file=sys.stderr)

    out = output_path or input_path
    with open(out, 'w') as f:
        f.write('\n'.join(lines) + '\n')

    return fixes_applied


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input.mlir> [output.mlir]", file=sys.stderr)
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2] if len(sys.argv) > 2 else None

    n1 = fix_mlir(input_path, output_path)
    # Arity mismatch fix disabled for now — the missing values aren't in SSA scope
    # at the call site (they're in enclosing scf.if blocks and can't be referenced
    # from inner regions without threading through yields).
    # target = output_path or input_path
    # n2 = fix_arity_mismatches(target, target)
    n2 = 0
    total = n1 + n2
    if total:
        print(f"fix-captures: fixed {n1} capture issues, {n2} arity mismatches", file=sys.stderr)
