-- | Frankenstein Core Types
--
-- This is a simplified view of Koka Core that serves as the translation
-- target for all three frontend bridges (GHC, Mercury, rustc).
-- The actual compilation goes through Koka's real Core; these types
-- exist to make the translation layers readable and testable before
-- we wire them into Koka's internals.
--
-- Once the bridges are working, we replace these with direct Koka Core
-- construction (importing from the Koka codebase).

module Frankenstein.Core.Types
  ( -- * Names and identifiers
    Name(..)
  , QName(..)
  , Visibility(..)
    -- * Types
  , Type(..)
  , Kind(..)
  , Multiplicity(..)
  , EffectRow(..)
  , TypeVar(..)
  , TypeCon(..)
    -- * Core expressions
  , Expr(..)
  , Bind(..)
  , BindGroup
  , Lit(..)
  , Pattern(..)
  , Branch(..)
    -- * Definitions and programs
  , Def(..)
  , DefSort(..)
  , Program(..)
    -- * Effect-related
  , EffectDecl(..)
  , OpDecl(..)
    -- * Data type declarations
  , DataDecl(..)
  , ConDecl(..)
  ) where

import Data.Text (Text)

-- Names

data Name = Name
  { nameText   :: !Text
  , nameUnique :: !Int
  } deriving (Show, Eq, Ord)

data QName = QName
  { qnameModule :: !Text
  , qnameName   :: !Name
  } deriving (Show, Eq, Ord)

data Visibility = Public | Private
  deriving (Show, Eq)

-- Multiplicity (Gap 1 from the design doc)

data Multiplicity
  = Many        -- ^ Unrestricted (default Koka, default Haskell)
  | Affine      -- ^ At most once (Rust ownership)
  | Linear      -- ^ Exactly once (Mercury di/uo)
  deriving (Show, Eq, Ord)

-- Kinds

data Kind
  = KindValue             -- ^ Type of values (Koka: V)
  | KindEffect            -- ^ Effect label (Koka: X)
  | KindEffectRow         -- ^ Effect row (Koka: E)
  | KindHeap              -- ^ Heap type (Koka: H)
  | KindArrow Kind Kind   -- ^ Kind constructor
  | KindStar              -- ^ Alias for KindValue
  deriving (Show, Eq, Ord)

-- Types

data TypeVar = TypeVar
  { tvName         :: !Name
  , tvKind         :: !Kind
  , tvMultiplicity :: !Multiplicity
  } deriving (Show, Eq, Ord)

data TypeCon = TypeCon
  { tcName :: !QName
  , tcKind :: !Kind
  } deriving (Show, Eq)

data EffectRow
  = EffectRowEmpty                          -- ^ Total/pure
  | EffectRowExtend !QName !EffectRow       -- ^ Add an effect label
  | EffectRowVar !TypeVar                   -- ^ Polymorphic effect tail
  deriving (Show, Eq)

data Type
  = TForall  [TypeVar] Type                 -- ^ Universal quantification
  | TFun     [(Multiplicity, Type)] EffectRow Type  -- ^ Function: args (with multiplicity) -> eff -> result
  | TApp     Type Type                      -- ^ Type application
  | TCon     TypeCon                        -- ^ Type constructor
  | TVar     TypeVar                        -- ^ Type variable
  | TSyn     QName [Type] Type              -- ^ Type synonym (carries expansion)
  deriving (Show, Eq)

-- Literals

data Lit
  = LitInt    !Integer
  | LitFloat  !Double
  | LitChar   !Char
  | LitString !Text
  deriving (Show, Eq)

-- Patterns

data Pattern
  = PatCon   QName [Pattern]  -- ^ Constructor pattern
  | PatVar   Name Type        -- ^ Variable binding
  | PatWild  Type             -- ^ Wildcard
  | PatLit   Lit              -- ^ Literal pattern
  deriving (Show, Eq)

data Branch = Branch
  { branchPattern :: !Pattern
  , branchGuard   :: !(Maybe Expr)
  , branchBody    :: !Expr
  } deriving (Show, Eq)

-- Core expressions

data Expr
  = EVar    Name                          -- ^ Variable reference
  | ELit    Lit                           -- ^ Literal
  | ECon    QName                         -- ^ Constructor reference
  | EApp    Expr [Expr]                   -- ^ Application
  | ELam    [(Name, Type)] Expr           -- ^ Lambda abstraction
  | ELet    [BindGroup] Expr              -- ^ Let bindings
  | ECase   Expr [Branch]                 -- ^ Pattern matching
  | ETypeApp  Expr [Type]                 -- ^ Type application (System F)
  | ETypeLam [TypeVar] Expr               -- ^ Type abstraction (System F)
  -- Effect operations
  | EPerform QName [Expr]                 -- ^ Perform effect operation
  | EHandle  EffectRow Expr Expr          -- ^ Handle effect: effect, handler, body
  -- Resource management (Perceus)
  | ERetain  Expr                         -- ^ Increment refcount
  | ERelease Expr                         -- ^ Decrement refcount
  | EDrop    Expr                         -- ^ Drop value (decrement + free if zero)
  | EReuse   Expr Expr                    -- ^ Reuse allocation if refcount==1
  -- Laziness (Haskell bridge)
  | EDelay   Expr                         -- ^ Create thunk (suspend computation)
  | EForce   Expr                         -- ^ Force thunk
  deriving (Show, Eq)

-- Bindings

data Bind = Bind
  { bindName  :: !Name
  , bindType  :: !Type
  , bindExpr  :: !Expr
  , bindSort  :: !DefSort
  } deriving (Show, Eq)

type BindGroup = [Bind]  -- mutually recursive group

-- Definition sorts

data DefSort
  = DefFun         -- ^ Regular function
  | DefVal         -- ^ Value binding
  | DefVar         -- ^ Mutable variable (translated from Rust let mut)
  deriving (Show, Eq)

-- Top-level definitions

data Def = Def
  { defName       :: !QName
  , defType       :: !Type
  , defExpr       :: !Expr
  , defSort       :: !DefSort
  , defVisibility :: !Visibility
  } deriving (Show, Eq)

-- Data type declarations

data ConDecl = ConDecl
  { conName   :: !QName
  , conFields :: ![(Name, Type)]
  , conVis    :: !Visibility
  } deriving (Show, Eq)

data DataDecl = DataDecl
  { dataName   :: !QName
  , dataParams :: ![TypeVar]
  , dataCons   :: ![ConDecl]
  , dataVis    :: !Visibility
  } deriving (Show, Eq)

-- Effect declarations

data OpDecl = OpDecl
  { opName    :: !QName
  , opType    :: !Type
  } deriving (Show, Eq)

data EffectDecl = EffectDecl
  { effectName   :: !QName
  , effectParams :: ![TypeVar]
  , effectOps    :: ![OpDecl]
  } deriving (Show, Eq)

-- Programs

data Program = Program
  { progName    :: !QName
  , progDefs    :: ![Def]
  , progData    :: ![DataDecl]
  , progEffects :: ![EffectDecl]
  } deriving (Show, Eq)
