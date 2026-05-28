# Literature on Polyglot FFI Typing

*A starting bibliography and topic guide for the cross-language
type-system question raised in `ffi-cross-language.md`.  Citations
in this document have been verified against DBLP/ACM/arXiv except
where noted.*

---

## Status

This is a **literature guide** for the cross-language FFI typing
question.  It collects the body of work that addresses (or partially
addresses) the design space Frankenstein needs to navigate before
committing to a real cross-language type discipline.  Pair this with
`ffi-cross-language.md`, which describes the current interim state
(uniform `i64` ABI, name-based linker resolution, no cross-language
type checking).

---

There's a small but well-defined research line on exactly this
question — most of it grouped around a handful of research groups.
The map below organises citations by relevance to what Frankenstein
needs, with explicit notes on the open territory the literature has
*not* yet covered.

## Most directly relevant: "linking types" / multi-language semantics

The closest match to Frankenstein's situation is **Amal Ahmed and
collaborators' work on multi-language linking and type soundness
across compilation**.

### The setting paper

- **Daniel Patterson and Amal Ahmed, "Linking Types for Multi-Language
  Software: Have Your Cake and Eat It Too,"** _SNAPL 2017_ (LIPIcs
  vol. 71, paper 12).
  [Dagstuhl](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.SNAPL.2017.12)
  /
  [arXiv](https://arxiv.org/abs/1711.04559).
  Argues that the type system of a *linker* is its own thing and
  should be designed deliberately.  Frames "what does it mean for
  module M (compiled from language A) to call module N (compiled
  from language B)?" as the central question.  This is essentially
  the framing the closing-section design questions in
  `ffi-cross-language.md` need.

### Modern follow-up — most important single paper to read

- **Daniel Patterson, Noble Mushtak, Andrew Wagner, Amal Ahmed,
  "Semantic Soundness for Language Interoperability,"** _PLDI 2022_.
  [ACM DL](https://dl.acm.org/doi/10.1145/3519939.3523703) /
  [arXiv](https://arxiv.org/abs/2202.13158).
  Develops a semantic-soundness framework general enough for
  arbitrary source languages meeting at a shared compilation target
  — the closest published treatment of Frankenstein's exact
  situation.  Effectively supersedes the SNAPL 2017 framing as the
  modern reference.

### Surrounding Ahmed-group work on fully abstract compilation

- **James T. Perconti and Amal Ahmed, "Verifying an Open Compiler
  Using Multi-Language Semantics,"** _ESOP 2014_ (LNCS 8410).
  [Springer](https://link.springer.com/chapter/10.1007/978-3-642-54833-8_8).

- **Max S. New, William J. Bowman, Amal Ahmed, "Fully Abstract
  Compilation via Universal Embedding,"** _ICFP 2016_.
  [ACM DL](https://dl.acm.org/doi/10.1145/2951913.2951941).

- **Max S. New, Daniel R. Licata, Amal Ahmed, "Gradual Type
  Theory,"** _POPL 2019_.
  [arXiv](https://arxiv.org/abs/1811.02440).
  Not strictly multi-language but provides the type-theoretic
  vocabulary the Ahmed line builds on.

### The foundational operational-semantics paper

- **Jacob Matthews and Robert Bruce Findler, "Operational Semantics
  for Multi-Language Programs":**
  - Conference: _POPL 2007_, pp. 3-10.
    [ACM DL](https://dl.acm.org/doi/10.1145/1190215.1190220) /
    [PDF](https://users.cs.northwestern.edu/~robby/pubs/papers/popl2007-mf-bw.pdf).
  - Journal: _ACM TOPLAS_ 31(3):12, 1-44, 2009.
    [ACM DL](https://dl.acm.org/doi/10.1145/1498926.1498930).

  Introduces the **lump / natural / boundary** discipline: explicit
  boundary terms in the operational semantics, with "lump"
  embeddings for values whose representation isn't shared and
  "natural" embeddings for compatible ones.  This is the cleanest
  formal framework for "two languages share part of a value lattice
  and reject the rest."

A Frankenstein-specific note: the uniform-`i64` ABI corresponds to a
*single shared natural boundary* for the integer-shaped fragment,
with everything else implicitly lumped (and trusted).
Matthews-Findler would give vocabulary to make this discipline
explicit.

## Practical polyglot platforms (industrial state of the art)

These are less "literature" and more "engineering papers + standards
docs," but they're the existing solutions in production.

### GraalVM / Truffle

- **Thomas Würthinger, Christian Wimmer, Andreas Wöss, Lukas Stadler,
  Gilles Duboscq, Christian Humer, Gregor Richards, Doug Simon,
  Mario Wolczko, "One VM to Rule Them All,"** _Onward! 2013_
  (SPLASH).
  [Conference page](https://2013.splashcon.org/details/onward-2013-papers/6/One-VM-to-Rule-Them-All).
  The foundational GraalVM paper.

- **Matthias Grimmer, Chris Seaton, Roland Schatz, Thomas Würthinger,
  Hanspeter Mössenböck, "High-Performance Cross-Language
  Interoperability in a Multi-Language Runtime,"** _DLS 2015_.
  [PDF](https://chrisseaton.com/rubytruffle/dls15-interop/dls15-interop.pdf).
  The closest GraalVM paper to the cross-language *type* model
  question.

  Caveat: GraalVM's more recent polyglot-type-model work is mostly
  in vendor documentation rather than peer-reviewed publications.

GraalVM's overall approach is **explicit conversion** at the
boundary, not unification — every cross-language access goes through
the polyglot API with documented type coercions.

### WebAssembly Component Model / Interface Types

- **Interface Types proposal:** championed by Luke Wagner (Mozilla /
  Fastly) with Francis McCabe, Jacob Gravelle, Alex Crichton, Nick
  Fitzgerald.  Started ca. 2019; introductory blog:
  [Mozilla Hacks](https://hacks.mozilla.org/2019/08/webassembly-interface-types/).
  Proposal repo:
  [github.com/WebAssembly/interface-types](https://github.com/WebAssembly/interface-types).
  Subsumed by the Component Model.

- **Component Model (current):**
  - Main repo:
    [github.com/WebAssembly/component-model](https://github.com/WebAssembly/component-model)
  - Explainer:
    [`Explainer.md`](https://github.com/WebAssembly/component-model/blob/main/design/mvp/Explainer.md)
  - WIT (Wasm Interface Type) spec:
    [`WIT.md`](https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md)
  - Bytecode Alliance docs site:
    [component-model.bytecodealliance.org](https://component-model.bytecodealliance.org/).

  Defines an *interface type lattice* (records, variants, lists,
  resources/handles) that sits between guest-language types.  Each
  guest binding emits adapters into/out of the interface types.
  This is currently the most operational, deployed answer to
  exactly the question Frankenstein is asking.

- **Foundational Wasm type paper:** Andreas Haas, Andreas Rossberg,
  Derek L. Schuff, Ben L. Titzer, Michael Holman, Dan Gohman, Luke
  Wagner, Alon Zakai, JF Bastien, "Bringing the Web up to Speed
  with WebAssembly," _PLDI 2017_ (Distinguished Paper).
  [ACM DL](https://dl.acm.org/doi/10.1145/3062341.3062363).

### Other industrial polyglot platforms (less academically papered)

- **Microsoft CLR / .NET interop.**  The CLR was built specifically
  as a polyglot meeting point — C#, F#, VB, IronPython, etc.  Don
  Syme's papers on F# / .NET discuss how F# preserves its type
  discipline while still calling into CLR-shared types.  The
  "Common Type System" is the operational answer; the academic
  literature on the CTS specifically is thin.

- **JVM polyglot (Scala, Clojure, Kotlin, Groovy on the JVM).**  Less
  papered, but the Odersky-era Scala-on-JVM literature has detailed
  writeups of how Scala's type system maps to JVM bytecode types.
  Relevant because the JVM ABI is *not* a uniform i64 — it has a
  richer type lattice (object refs vs primitives), which is closer
  to what Frankenstein might evolve into.

## Gradual typing — the typed/untyped boundary

The trust/no-trust setup at FFI boundaries has structural similarity
to gradual-typing's typed/untyped boundary, and the gradual-typing
literature is mature.

- **Sam Tobin-Hochstadt, Matthias Felleisen, "The Design and
  Implementation of Typed Scheme,"** _POPL 2008_, pp. 395-406.
  [ACM DL](https://dl.acm.org/doi/10.1145/1328438.1328486) /
  [PDF](https://www2.ccs.neu.edu/racket/pubs/popl08-thf.pdf).
  The starting point.  Subsequent work (especially with Greenman,
  Vitousek) explores the performance and soundness implications.
  Their "natural" / "transient" / "erasure" semantics for the
  typed-untyped boundary directly maps onto cross-language FFI
  choices.

- **Robert Bruce Findler, Matthias Felleisen, "Contracts for
  Higher-Order Functions,"** _ICFP 2002_, pp. 48-59.
  [ACM DL](https://dl.acm.org/doi/10.1145/581478.581484) /
  [PDF](https://www2.ccs.neu.edu/racket/pubs/icfp2002-ff.pdf).
  The contract-monitoring approach to enforcing type discipline at
  a runtime boundary.  Probably the most directly portable idea: a
  Frankenstein FFI type checker could insert *contract wrappers* at
  language boundaries that enforce shape invariants at runtime.

- **Ben Greenman, Matthias Felleisen, "A Spectrum of Type Soundness
  and Performance,"** _PACMPL_ 2(ICFP), article 71, 2018.
  [ACM DL](https://dl.acm.org/doi/10.1145/3236766).
  Quantifies the cost of different boundary disciplines.  Useful if
  choosing between strict checking, optimistic checking, and pure
  unchecked transport.

- **Ben Greenman, Matthias Felleisen, Christos Dimoulas, "Complete
  Monitors for Gradual Types,"** _PACMPL_ 3(OOPSLA), article 122,
  2019. [ACM DL](https://dl.acm.org/doi/10.1145/3360548).
  The "complete monitoring" property is closely related to what
  Frankenstein would want from a cross-language type checker.

## Effect systems crossing boundaries

This is more thinly papered than basic-type FFI.  No direct extension
of Patterson-Ahmed-style linking types to algebraic effect handlers
appears to be published; the two most relevant works are:

- **Patterson, Mushtak, Wagner, Ahmed, "Semantic Soundness for
  Language Interoperability,"** _PLDI 2022_ (cited above).  The
  semantic framework is general enough to accommodate effects but
  does not focus on handlers per se.

- **Luna Phipps-Costin, Andreas Rossberg, Arjun Guha, Daan Leijen,
  Daniel Hillerström, KC Sivaramakrishnan, Matija Pretnar, Sam
  Lindley, "Continuing WebAssembly with Effect Handlers"** (WasmFX),
  _PACMPL_ 7(OOPSLA2), article 318, 2023.
  [ACM DL](https://dl.acm.org/doi/10.1145/3622814) /
  [arXiv](https://arxiv.org/abs/2308.08347).
  Addresses how effect handlers cross an ABI boundary in practice —
  operationally what Frankenstein needs for the Mercury `exn` /
  Koka interaction.

### Foundational effect-handler papers (background, not FFI-specific)

- **Ohad Kammar, Sam Lindley, Nicolas Oury, "Handlers in Action,"**
  _ICFP 2013_, pp. 145-158.
  [ACM DL](https://dl.acm.org/doi/10.1145/2500365.2500590) /
  [PDF](https://denotational.co.uk/publications/kammar-lindley-oury-handlers-in-action.pdf).
  Most-Influential-ICFP-Paper Award 2023.  Discusses reifying
  foreign side-effects as algebraic operations — the cleanest
  published statement of "treat the FFI call as an effectful
  operation."

- **Sam Lindley, Conor McBride, Craig McLaughlin, "Do Be Do Be
  Do,"** _POPL 2017_, pp. 500-514.
  [ACM DL](https://dl.acm.org/doi/10.1145/3009837.3009897) /
  [arXiv](https://arxiv.org/abs/1611.09259).
  The Frank language; the cleanest minimal effect-handler calculus.

## Koka / Frankenstein's effect-passing backend

Relevant because Frankenstein uses Koka and inherits its effect
discipline:

- **Daan Leijen, "Koka: Programming with Row-Polymorphic Effect
  Types,"** _MSFP 2014_ (EPTCS 153).
  [arXiv](https://arxiv.org/abs/1406.2061).

- **Daan Leijen, "Type Directed Compilation of Row-Typed Algebraic
  Effects,"** _POPL 2017_, pp. 486-499.
  [ACM DL](https://dl.acm.org/doi/10.1145/3009837.3009872) /
  [PDF](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/algeff.pdf).
  Most-cited Koka effects paper; explicitly discusses compilation
  to existing platforms (JS/JVM/.NET), which is the closest
  published treatment of Koka's FFI / extern story.

- **Ningning Xie, Daan Leijen, "Generalized Evidence Passing for
  Effect Handlers (Efficient Compilation of Effect Handlers to
  C),"** _PACMPL_ 5(ICFP), article 71, 2021.
  [ACM DL](https://dl.acm.org/doi/10.1145/3473576) /
  [MS Research](https://www.microsoft.com/en-us/research/publication/generalized-evidence-passing-for-effect-handlers/).
  The paper underlying Koka's current C backend — the most relevant
  Koka paper for an FFI-style ABI.

## Schema-driven IDL approaches (engineering, not research)

These bypass the unification question by requiring an *external*
schema:

- **Apache Thrift, Protocol Buffers, Cap'n Proto, FlatBuffers** —
  define types in a shared IDL, generate bindings per language.
  The IDL is the cross-language type system.  Limited expressivity
  (no higher-order, no effects) but operationally simple.

- **Apache Arrow** — columnar data format, more focused on data
  than functions, but the "shared canonical representation" idea is
  the same.

These won't give Frankenstein language-level features (closures,
effects, refinement types), but they're worth considering as a
*fallback discipline* — if cross-language calls had to go through an
IDL-described schema, soundness would be much easier to enforce.

## What seems *under*-covered in the literature

A few things worth wanting and that don't have obvious published
treatments:

- **Cross-language ownership / multiplicity reconciliation.**
  Rust's affine and Haskell's `Many` meeting at a function
  boundary.  Some Rust-team blog posts touch on this (the C interop
  story) but no published paper formalizes the safe
  ownership-transfer question across two source languages with
  different multiplicity disciplines.

- **Effect-row negotiation under partial handlers.**  When Mercury's
  `exn` enters a Koka caller that has *some* exception handling
  but not the right kind — what's the principled answer?  WasmFX
  (above) is the closest published work but doesn't directly
  address row-level negotiation across source languages.

- **Cross-language inlining and optimization.**  If language A's
  call to language B can be inlined (because both ended up in the
  same MLIR), what type-system guarantees survive?  GHC's RULES
  pragmas and Rust's inlining each have their own story, but no
  published treatment of what cross-language inlining preserves is
  known.

## Suggested starting point for Frankenstein

If reading just a handful of papers before sketching a design:

1. **Matthews & Findler (TOPLAS 2009)** — for the lump / natural /
   boundary vocabulary.
2. **Patterson, Mushtak, Wagner & Ahmed (PLDI 2022)** — for the
   modern semantic-soundness framing.
3. **The WebAssembly Component Model spec** — for an existence proof
   of an industrial cross-language type lattice that actually ships.
4. **Greenman & Felleisen (ICFP 2018)** — for cost realism on
   different boundary disciplines.
5. **Xie & Leijen (ICFP 2021)** — for the effect-row half of the
   problem and the evidence-passing translation Frankenstein
   already uses.
6. **Phipps-Costin et al. (OOPSLA 2023)** — for an existence proof
   that effect handlers can cross an ABI boundary in practice.
