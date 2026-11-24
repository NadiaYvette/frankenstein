{-# LANGUAGE OverloadedStrings #-}
-- | Module: Frankenstein.Driver.Roster15
-- Description: Curated 15-language data-driven driver roster for Ingolstadt
--
-- Registers all 15 core interop languages (4 systems, 2 core high-level,
-- 3 theorem provers, 3 functional siblings, 3 logic/relational) into
-- 'DriverRegistry' with zero per-language Hackage packages and zero extension collisions.
module Frankenstein.Driver.Roster15
  ( roster15Manifests
  , buildRoster15Registry
  ) where

import Control.Monad (foldM)
import Frankenstein.Core.Interface (MemProfile (..))
import Frankenstein.Driver.Registry
  ( DriverError
  , DriverManifest (..)
  , DriverRegistry
  , EmitStrategy (..)
  , FfiTypeRule (..)
  , FormatterHook (..)
  , IngestStrategy (..)
  , ParadigmTag (..)
  , emptyRegistry
  , registerDriver
  )

-- | Complete 15-language interop roster mirroring @drivers/*.dhall@.
roster15Manifests :: [DriverManifest]
roster15Manifests =
  [ -- 4 Low-Level / Systems Languages
    DriverManifest "c11" "C11 (Freestanding / POSIX)" ParadigmSystems [".c", ".h"]
      (IngestExternalOrganJson "clang-organ" ["--std=c11"] "clang --version")
      (EmitIdiomaticTarget "c11" Nothing)
      [FfiTypeRule "int64_t" "i64" Profile0FreestandingLinear]
      (Just (FormatterHook "clang-format" [] True))
  , DriverManifest "cpp" "C++20 (C-ABI Boundary)" ParadigmSystems [".cpp", ".cc", ".hpp"]
      (IngestExternalOrganJson "clang-organ" ["--std=c++20"] "clang++ --version")
      (EmitOutboundFfiWrapper "cpp-extern-c" ["-lstdc++"])
      [FfiTypeRule "int64_t" "i64" Profile0FreestandingLinear]
      (Just (FormatterHook "clang-format" [] True))
  , DriverManifest "fortran-ffi" "Fortran 2003 (ISO_C_BINDING)" ParadigmSystems [".f90", ".f95"]
      (IngestExternalOrganJson "gfortran-organ" [] "gfortran --version")
      (EmitOutboundFfiWrapper "fortran-iso-c" ["-lgfortran"])
      [FfiTypeRule "integer(c_int64_t)" "i64" Profile0FreestandingLinear]
      (Just (FormatterHook "fprettify" [] False))
  , DriverManifest "rust" "Rust (MIR JSON + Ownership Emit)" ParadigmSystems [".rs"]
      (IngestExternalOrganJson "rustc-shim" ["--emit-mir-json"] "rustc --version")
      (EmitIdiomaticTarget "rust" Nothing)
      [FfiTypeRule "i64" "i64" Profile0FreestandingLinear]
      (Just (FormatterHook "rustfmt" [] False))

    -- 2 Core High-Level Languages
  , DriverManifest "haskell" "Haskell (Stock GHC 9.14 Core Plugin)" ParadigmFunctional [".hs", ".lhs"]
      (IngestExternalOrganJson "ghc" ["-fplugin=Frankenstein.GhcBridge.Plugin"] "ghc --version")
      EmitNone
      [FfiTypeRule "Int64" "i64" Profile1ThreadLocalPerceus]
      (Just (FormatterHook "fourmolu" ["--stdin-input-file", "-"] False))
  , DriverManifest "python-ext" "Python (Outbound C-API Extension)" ParadigmScriptingFfi [".py"]
      IngestNone
      (EmitOutboundFfiWrapper "python-capi" [])
      [FfiTypeRule "int" "i64" Profile1ThreadLocalPerceus]
      Nothing

    -- 3 Theorem Provers
  , DriverManifest "lean4" "Lean 4 (Perceus-Native LCNF)" ParadigmTheoremProver [".lean"]
      (IngestExternalOrganJson "lean-organ" [] "lean --version")
      EmitNone
      [FfiTypeRule "Int" "i64" Profile1ThreadLocalPerceus]
      Nothing
  , DriverManifest "rocq" "Rocq / Iris Proof & Spec Target" ParadigmTheoremProver [".v"]
      IngestNone
      (EmitIdiomaticTarget "rocq" Nothing)
      [FfiTypeRule "Z" "i64" Profile0FreestandingLinear]
      Nothing
  , DriverManifest "idris2" "Idris 2 (QTT Multiplicities)" ParadigmTheoremProver [".idr"]
      (IngestExternalOrganJson "idris2-organ" [] "idris2 --version")
      EmitNone
      [FfiTypeRule "Int64" "i64" Profile0FreestandingLinear]
      Nothing

    -- 3 Functional Siblings
  , DriverManifest "ocaml" "OCaml (-dlambda Ingest)" ParadigmFunctional [".ml", ".mli"]
      (IngestExternalOrganJson "ocaml-organ" ["-dlambda"] "ocamlopt -version")
      EmitNone
      [FfiTypeRule "int64" "i64" Profile1ThreadLocalPerceus]
      (Just (FormatterHook "ocamlformat" ["-"] False))
  , DriverManifest "sml" "Standard ML (Declarative / Isabelle Extraction)" ParadigmFunctional [".sml", ".sig"]
      (IngestDeclarativeGrammar "drivers/grammars/sml.smirk" "drivers/rules/sml-organ.dhall")
      EmitNone
      [FfiTypeRule "Int64.int" "i64" Profile1ThreadLocalPerceus]
      Nothing
  , DriverManifest "scheme" "R7RS Scheme (Declarative Grammar)" ParadigmFunctional [".scm", ".sld"]
      (IngestDeclarativeGrammar "drivers/grammars/scheme.smirk" "drivers/rules/scheme-organ.dhall")
      EmitNone
      [FfiTypeRule "integer" "i64" Profile1ThreadLocalPerceus]
      Nothing

    -- 3 Logic / Relational Languages
  , DriverManifest "mercury" "Mercury (mmc HLDS Determinism Ingest)" ParadigmLogic [".m"]
      (IngestExternalOrganJson "mmc" ["--dump-hlds"] "mmc --version")
      EmitNone
      [FfiTypeRule "int64" "i64" Profile1ThreadLocalPerceus]
      Nothing
  , DriverManifest "prolog" "ISO Prolog (Declarative Grammar)" ParadigmLogic [".pl", ".pro"]
      (IngestDeclarativeGrammar "drivers/grammars/prolog.smirk" "drivers/rules/prolog-organ.dhall")
      EmitNone
      [FfiTypeRule "integer" "i64" Profile1ThreadLocalPerceus]
      Nothing
  , DriverManifest "datalog" "Datalog / Souffle (Declarative Grammar)" ParadigmLogic [".dl"]
      (IngestDeclarativeGrammar "drivers/grammars/datalog.smirk" "drivers/rules/datalog-organ.dhall")
      EmitNone
      [FfiTypeRule "number" "i64" Profile0FreestandingLinear]
      Nothing
  ]

-- | Populates a validated 'DriverRegistry' containing all 15 canonical drivers.
buildRoster15Registry :: Either DriverError DriverRegistry
buildRoster15Registry =
  foldM (flip registerDriver) emptyRegistry roster15Manifests
