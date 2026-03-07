-- | MLIR Dialect Definitions
--
-- Defines the MLIR dialects we use for code generation:
-- - func: function definitions and calls
-- - arith: integer and floating-point arithmetic
-- - scf: structured control flow (if, for, while)
-- - memref: memory references (for Perceus refcounting)
-- - frankenstein: custom dialect for effect handler frames
--
-- Phase 1: We emit textual MLIR and shell out to mlir-opt/mlir-translate.
-- Phase 2: Use MLIR C API via Haskell FFI for direct construction.

module Frankenstein.MlirEmit.Dialects
  ( MlirOp(..)
  , MlirType(..)
  , MlirValue(..)
  , MlirBlock(..)
  , MlirFunc(..)
  , MlirModule(..)
  , renderModule
  , renderFunc
  , renderOp
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | MLIR SSA value reference
data MlirValue = MlirValue
  { valName :: !Text   -- e.g., "%0", "%arg0"
  , valType :: !MlirType
  } deriving (Show, Eq)

-- | MLIR types we emit
data MlirType
  = MlirI1 | MlirI8 | MlirI32 | MlirI64 | MlirF32 | MlirF64
  | MlirIndex
  | MlirPtr                          -- opaque pointer (for Koka runtime values)
  | MlirFunc_ [MlirType] [MlirType] -- function type: args -> results
  | MlirMemRef MlirType [Int]       -- memref<NxT>
  | MlirNone                         -- none type (for void)
  | MlirStruct [MlirType]           -- tuple/struct
  | MlirNamed Text                    -- named type (!frankenstein.value etc.)
  deriving (Show, Eq)

-- | An MLIR operation
data MlirOp
  -- arith dialect
  = ArithConstI Integer MlirType    -- arith.constant N : type
  | ArithConstF Double MlirType     -- arith.constant N.M : type
  | ArithAddI MlirValue MlirValue   -- arith.addi
  | ArithSubI MlirValue MlirValue   -- arith.subi
  | ArithMulI MlirValue MlirValue   -- arith.muli
  | ArithCmpI Text MlirValue MlirValue  -- arith.cmpi "eq"|"ne"|"slt"|...
  -- func dialect
  | FuncCall Text [MlirValue] [MlirType]  -- func.call @name(args) : types
  | FuncReturn [MlirValue]                 -- func.return
  -- scf dialect
  | ScfIf MlirValue [MlirOp] [MlirOp] MlirType  -- scf.if cond then else -> type
  | ScfWhile [MlirOp] [MlirOp]                    -- scf.while { before } do { after }
  -- Koka runtime calls (via func.call to C runtime)
  | KokaAlloc MlirValue              -- kk_alloc(size)
  | KokaRetain MlirValue             -- kk_retain(ptr) — increment refcount
  | KokaRelease MlirValue            -- kk_release(ptr) — decrement refcount
  | KokaDrop MlirValue               -- kk_drop(ptr) — decrement + free if 0
  | KokaReuse MlirValue MlirValue    -- kk_reuse(ptr, tag) — reuse if refcount==1
  | KokaIsUnique MlirValue           -- kk_is_unique(ptr) — check refcount==1
  -- Effect handler operations
  | EvvGet                            -- get evidence vector
  | EvvPush MlirValue                 -- push handler onto evidence vector
  | EvvPop                            -- pop handler from evidence vector
  | EvvLookup MlirValue              -- lookup handler in evidence vector
  -- Raw MLIR text (escape hatch)
  | RawMlir Text
  deriving (Show)

-- | A basic block in MLIR
data MlirBlock = MlirBlock
  { blockLabel :: !Text
  , blockArgs  :: ![(Text, MlirType)]
  , blockOps   :: ![(Maybe Text, MlirOp)]  -- optional result name, operation
  } deriving (Show)

-- | An MLIR function
data MlirFunc = MlirFunc
  { funcName    :: !Text
  , funcArgs    :: ![(Text, MlirType)]
  , funcResults :: ![MlirType]
  , funcBlocks  :: ![MlirBlock]
  , funcAttrs   :: ![(Text, Text)]
  } deriving (Show)

-- | An MLIR module
data MlirModule = MlirModule
  { modFuncs  :: ![MlirFunc]
  , modExtern :: ![(Text, [MlirType], [MlirType])]  -- external function declarations
  } deriving (Show)

-- Rendering to textual MLIR

renderModule :: MlirModule -> Text
renderModule m = T.unlines $
  [ "module {" ]
  ++ map renderExtern (modExtern m)
  ++ map renderFunc (modFuncs m)
  ++ [ "}" ]

renderExtern :: (Text, [MlirType], [MlirType]) -> Text
renderExtern (name, args, rets) =
  "  func.func private @" <> name <> "(" <> renderTypes args <> ")" <>
  (if null rets then "" else " -> " <> renderRetTypes rets)

renderFunc :: MlirFunc -> Text
renderFunc f = T.unlines $
  [ "  func.func @" <> funcName f <> "(" <> renderFuncArgs (funcArgs f) <> ")" <>
    (if null (funcResults f) then "" else " -> " <> renderRetTypes (funcResults f))
    <> " {" ]
  ++ concatMap renderBlock (funcBlocks f)
  ++ [ "  }" ]

renderFuncArgs :: [(Text, MlirType)] -> Text
renderFuncArgs args = T.intercalate ", " [n <> ": " <> renderType t | (n, t) <- args]

renderBlock :: MlirBlock -> [Text]
renderBlock b =
  [ "  " <> blockLabel b <> (if null (blockArgs b) then ":" else
      "(" <> T.intercalate ", " [n <> ": " <> renderType t | (n, t) <- blockArgs b] <> "):") ]
  ++ map (\(mname, op) -> "    " <> renderAssign mname op) (blockOps b)

renderAssign :: Maybe Text -> MlirOp -> Text
renderAssign Nothing op = renderOp op
renderAssign (Just name) op = "%" <> name <> " = " <> renderOp op

renderOp :: MlirOp -> Text
renderOp (ArithConstI n ty) = "arith.constant " <> T.pack (show n) <> " : " <> renderType ty
renderOp (ArithConstF n ty) = "arith.constant " <> T.pack (show n) <> " : " <> renderType ty
renderOp (ArithAddI a b) = "arith.addi " <> valName a <> ", " <> valName b <> " : " <> renderType (valType a)
renderOp (ArithSubI a b) = "arith.subi " <> valName a <> ", " <> valName b <> " : " <> renderType (valType a)
renderOp (ArithMulI a b) = "arith.muli " <> valName a <> ", " <> valName b <> " : " <> renderType (valType a)
renderOp (ArithCmpI pred' a b) = "arith.cmpi " <> pred' <> ", " <> valName a <> ", " <> valName b <> " : " <> renderType (valType a)
renderOp (FuncCall name args rets) =
  "func.call @" <> name <> "(" <> T.intercalate ", " (map valName args) <> ") : ("
  <> renderTypes (map valType args) <> ") -> " <> renderRetTypes rets
renderOp (FuncReturn vals) = "func.return " <> T.intercalate ", " (map valName vals)
                              <> (if null vals then "" else " : " <> renderTypes (map valType vals))
renderOp (KokaRetain v) = "func.call @kk_retain(" <> valName v <> ") : (!llvm.ptr) -> ()"
renderOp (KokaRelease v) = "func.call @kk_release(" <> valName v <> ") : (!llvm.ptr) -> ()"
renderOp (KokaDrop v) = "func.call @kk_drop(" <> valName v <> ") : (!llvm.ptr) -> ()"
renderOp (KokaReuse ptr tag) = "func.call @kk_reuse(" <> valName ptr <> ", " <> valName tag <> ") : (!llvm.ptr, i32) -> !llvm.ptr"
renderOp (KokaIsUnique v) = "func.call @kk_is_unique(" <> valName v <> ") : (!llvm.ptr) -> i1"
renderOp (RawMlir t) = t
renderOp op = "// TODO: " <> T.pack (show op)

renderType :: MlirType -> Text
renderType MlirI1 = "i1"
renderType MlirI8 = "i8"
renderType MlirI32 = "i32"
renderType MlirI64 = "i64"
renderType MlirF32 = "f32"
renderType MlirF64 = "f64"
renderType MlirIndex = "index"
renderType MlirPtr = "!llvm.ptr"
renderType (MlirFunc_ args rets) = "(" <> renderTypes args <> ") -> " <> renderRetTypes rets
renderType (MlirMemRef ty dims) = "memref<" <> T.intercalate "x" (map (T.pack . show) dims) <> "x" <> renderType ty <> ">"
renderType MlirNone = "none"
renderType (MlirStruct ts) = "tuple<" <> renderTypes ts <> ">"
renderType (MlirNamed n) = "!" <> n

renderTypes :: [MlirType] -> Text
renderTypes = T.intercalate ", " . map renderType

renderRetTypes :: [MlirType] -> Text
renderRetTypes [t] = renderType t
renderRetTypes ts = "(" <> renderTypes ts <> ")"
