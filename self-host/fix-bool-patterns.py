#!/usr/bin/env python3
"""Pre-processor for OrganIR JSON files.

Fixes two issues that cause exponential code generation in the self-hosted
compiler's emitter:

1. Bool patterns: Converts PatCon False/True to PatLit 0/1 so the emitter
   uses IntLitCase instead of ConCase (the self-hosted isBoolConCase fails).

2. Exhaustive N-branch constructor cases (N >= 2): Converts the last PatCon
   to PatWild so the emitter terminates with "con + default" path (line 1878)
   instead of hitting the single-element list pattern at line 1872 which
   fails in the self-hosted binary, causing duplicate code generation.

Usage:
    python3 fix-bool-patterns.py input.organ.json
    # Modifies the file in place
"""

import json
import sys


def fix_patterns(obj):
    """Recursively fix case branch patterns that trigger exponential codegen."""
    if isinstance(obj, dict):
        if 'ecase' in obj:
            branches = obj['ecase'].get('branches', [])
            if len(branches) >= 2:
                pats = [br.get('pattern', {}) for br in branches]
                all_con = all('pat_con' in p for p in pats)
                if all_con:
                    names = [p['pat_con'].get('name', {}).get('name', {}).get('text', '')
                             for p in pats]
                    if len(branches) == 2 and set(names) == {'False', 'True'}:
                        # Bool case: convert to integer literals (Bool is unboxed 0/1)
                        for br in branches:
                            name = br['pattern']['pat_con']['name']['name']['text']
                            if name == 'False':
                                br['pattern'] = {'pat_lit': {'int': 0}}
                            elif name == 'True':
                                br['pattern'] = {'pat_lit': {'int': 1}}
                    else:
                        # Exhaustive N-constructor case (N >= 2, non-Bool):
                        # The self-hosted emitter's single-element list path
                        # (emitConChain line 1872) has a bug causing duplicate
                        # code. Work around by adding a PatWild default branch
                        # with a dummy body. This makes the emitter use the
                        # "con + default" path (line 1878) which is correct.
                        #
                        # We APPEND a new PatWild branch instead of converting
                        # the last PatCon, because converting drops pattern
                        # binders (e.g. Just v → _) making v a free variable.
                        branches.append({
                            'pattern': {'pat_wild': {}},
                            'body': {'elit': {'int': 0}}
                        })
        for v in obj.values():
            fix_patterns(v)
    elif isinstance(obj, list):
        for item in obj:
            fix_patterns(item)


def main():
    if len(sys.argv) < 2:
        print("Usage: fix-bool-patterns.py <file.organ.json>", file=sys.stderr)
        sys.exit(1)

    path = sys.argv[1]
    with open(path, 'rb') as f:
        data = json.loads(f.read(), strict=False)

    fix_patterns(data)

    with open(path, 'w') as f:
        json.dump(data, f)


if __name__ == '__main__':
    main()
