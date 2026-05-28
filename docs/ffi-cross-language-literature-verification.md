# Verification Notes for `ffi-cross-language-literature.md`

*A record of which citations in the literature guide were corrected
or added during the verification pass, so the provenance of the
bibliography is auditable.*

---

## Why this exists

The literature guide (`ffi-cross-language-literature.md`) was first
drafted from working memory, then verified by web search against
DBLP, the ACM Digital Library, arXiv, and author homepages.  The
verification surfaced five real errors and added one major modern
reference plus several useful adjacent citations.  This document
records what changed so the bibliography's confidence level can be
re-audited later if needed.

---

## Corrections

| Original claim | Corrected |
|---|---|
| "Linking Types for Multi-Language Software" (no subtitle) | "Linking Types for Multi-Language Software: **Have Your Cake and Eat It Too**" — full title |
| Matthews & Findler journal extension in JFP ca. 2009 | **ACM TOPLAS** 31(3):12, 2009 — wrong journal in the draft |
| Greenman, Felleisen, **Dimoulas**, "A Spectrum of Type Soundness and Performance," ICFP 2018 | Just **Greenman & Felleisen** ICFP 2018.  Dimoulas joins the trio on the OOPSLA 2019 follow-up "Complete Monitors for Gradual Types" |
| "**Lindley/Cheney/McBride** Edinburgh effect handlers" | Kammar/Lindley/Oury, "Handlers in Action" (ICFP 2013) + Lindley/McBride/McLaughlin, "Do Be Do Be Do" (POPL 2017). **Cheney is not an author** on either paper |
| "Patterson and Ahmed have at least one paper extending the linking-types framework to languages with algebraic effects" | **Not verifiable.**  No such paper found.  Closest matches are Patterson et al. PLDI 2022 (semantic soundness, not effect-specific) and Phipps-Costin et al. WasmFX OOPSLA 2023 |

---

## Major addition

**Daniel Patterson, Noble Mushtak, Andrew Wagner, Amal Ahmed,
"Semantic Soundness for Language Interoperability," PLDI 2022.**

This is the modern Ahmed-group paper that effectively supersedes the
SNAPL 2017 framing.  It's the single most on-topic published work for
what Frankenstein needs and moves to position #2 in the suggested
reading list.

- [ACM DL](https://dl.acm.org/doi/10.1145/3519939.3523703)
- [arXiv](https://arxiv.org/abs/2202.13158)

---

## Other useful additions

- **Phipps-Costin, Rossberg, Guha, Leijen, Hillerström,
  Sivaramakrishnan, Pretnar, Lindley, "Continuing WebAssembly with
  Effect Handlers" (WasmFX),** _PACMPL_ 7(OOPSLA2), article 318,
  2023.
  [ACM DL](https://dl.acm.org/doi/10.1145/3622814)
  /
  [arXiv](https://arxiv.org/abs/2308.08347).
  Published existence proof that effect handlers can cross an ABI
  boundary in practice — directly relevant to the Mercury `exn` /
  Koka interaction in the polyglot demo.

- **Xie & Leijen, "Generalized Evidence Passing for Effect
  Handlers,"** _PACMPL_ 5(ICFP), article 71, 2021.
  [ACM DL](https://dl.acm.org/doi/10.1145/3473576).
  The paper underlying Koka's evidence-passing C backend that
  Frankenstein actually uses for its own evidence pass.

- **Haas, Rossberg, Schuff, Titzer, Holman, Gohman, Wagner, Zakai,
  Bastien, "Bringing the Web up to Speed with WebAssembly,"** _PLDI
  2017_ (Distinguished Paper).
  [ACM DL](https://dl.acm.org/doi/10.1145/3062341.3062363).
  The foundational WebAssembly type-system paper.

- **Patterson, Mushtak, Wagner, Ahmed, "Semantic Soundness for
  Language Interoperability,"** PLDI 2022 (cited above) is the
  most relevant modern Ahmed-group paper.

- **Grimmer, Seaton, Schatz, Würthinger, Mössenböck,
  "High-Performance Cross-Language Interoperability in a
  Multi-Language Runtime,"** _DLS 2015_.
  [PDF](https://chrisseaton.com/rubytruffle/dls15-interop/dls15-interop.pdf).
  The GraalVM paper closest to the cross-language *type* model
  question, supplementing the Onward! 2013 paper.

---

## Softened claims

- "More recent papers from the GraalVM group discuss the polyglot
  type model in detail."  →  GraalVM's recent polyglot-type-model
  work is mostly in vendor documentation rather than peer-reviewed
  publications; the literature in this area is thinner than the
  initial draft implied.

---

## Method

The verification dispatched a general-purpose subagent with
WebSearch + WebFetch capabilities, briefed with the nine citation
clusters in the draft.  Each cluster was confirmed against at least
one primary source (DBLP, ACM DL, arXiv, conference page, or author
homepage).  Where the original claim could not be confirmed, it was
removed or rephrased rather than left in.

Anything still tagged "verify exact title" or similar in the
literature guide indicates a confidence level below this pass and
should be checked independently before formal citation.
