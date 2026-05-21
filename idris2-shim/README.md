# Frankenstein Idris2 Shim

Custom Idris2 codegen plugin that emits OrganIR JSON instead of running one
of the standard Idris2 backends (chez/racket/js/node/refc).  Analogous to
`rustc-shim` for the Rust side: lets the Frankenstein bridge consume
*already-elaborated* Idris2 Core (the `CompileData` from
`Compiler.Common`) instead of having to hand-parse Idris2 surface syntax.

## Build

Requires Idris2 (system idris2 0.8.0 works) and a built idris2api alongside.
The package lookup uses a local `depends/` directory pointing at the in-tree
build artifacts of an idris2 source checkout.

```
# in /home/nyc/src/Idris2 (already on origin/main, version 0.8.0)
make src/IdrisPaths.idr
idris2 --install idris2api.ipkg              # builds the api
```

Then:

```
cd idris2-shim
# depends/idris2-0.8.0/ should hold a stripped ipkg + symlink to the api
# build/ttc:
#   depends/idris2-0.8.0/idris2.ipkg          (just `package idris2; version = 0.8.0; depends = network`)
#   depends/idris2-0.8.0/2025081600 -> /home/nyc/src/Idris2/build/ttc/2025081600

idris2 --build idris2-shim.ipkg
```

## Use

`build/exec/frankenstein-idris2` is a drop-in idris2 replacement with an
extra `organir` codegen registered.  System idris2's stdlib lives at
`/usr/lib64/idris2-0.8.0` on Fedora; the shim doesn't bake its own prefix,
so callers point at the system install:

```
IDRIS2_PREFIX=/usr/lib64 build/exec/frankenstein-idris2 \
    --cg organir -o out path/to/Hello.idr
```

For now the codegen just prints a count of NamedDefs.  Next steps in
`src/Main.idr`: walk `namedDefs` and emit OrganIR JSON matching
`Frankenstein.OrganIR.Types`.
