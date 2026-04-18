-- | Auto-generate field selector functions for record-like data types.
--
-- A "record-like" data type is a 'DataDecl' whose 'dataCons' has exactly
-- one constructor with at least one named field. For each such record,
-- this pass generates one selector function per field:
--
--     data Point = Point { x :: Int, y :: Int }
--   ⟹
--     x :: Point -> Int
--     x = \p -> case p of Point fx _ -> fx
--     y :: Point -> Int
--     y = \p -> case p of Point _ fy -> fy
--
-- The generated selectors are added to 'progDefs'. Selectors are skipped
-- when:
--
--   * the data type has multiple constructors (selector would be partial),
--   * a top-level binding with the same name already exists (the bridge
--     or user has provided their own definition).
--
-- The pass is bridge-agnostic: any frontend that puts a single-constructor
-- 'DataDecl' into 'progData' gets selectors for free, which means records
-- "just work" once a bridge surfaces them.

module Frankenstein.Core.DeriveSelectors
  ( deriveSelectors
  , recordSelectors
  ) where

import Frankenstein.Core.Types

import qualified Data.Set as Set
import qualified Data.Text as T

-- | Run the selector-derivation pass over a 'Program'. Selectors are
-- appended to 'progDefs'; existing definitions are never overwritten.
deriveSelectors :: Program -> Program
deriveSelectors prog =
  let existingNames =
        Set.fromList [ nameText (qnameName (defName d)) | d <- progDefs prog ]
      newDefs = concatMap (recordSelectors existingNames) (progData prog)
  in prog { progDefs = progDefs prog ++ newDefs }

-- | Generate selector definitions for a single 'DataDecl', skipping any
-- whose name is already present in @existing@. Multi-constructor data
-- types yield no selectors (selection would be partial).
recordSelectors :: Set.Set T.Text -> DataDecl -> [Def]
recordSelectors existing dd = case dataCons dd of
  [con] | hasRecordFields con ->
            concatMap (mkSelector existing dd con) (zip [0..] (conFields con))
  _     -> []  -- multi-ctor, empty, or positional: no auto selectors

-- | A constructor has record fields if at least one field name is NOT
-- an auto-generated positional name (\"field_0\", \"field_1\", ...).
-- Positional types should not get selector functions — their fields
-- are accessed only via pattern matching.
hasRecordFields :: ConDecl -> Bool
hasRecordFields con = any (not . isPositionalName . fst) (conFields con)

isPositionalName :: Name -> Bool
isPositionalName n = case T.stripPrefix "field_" (nameText n) of
  Just rest -> T.all (\c -> c >= '0' && c <= '9') rest && not (T.null rest)
  Nothing   -> False

-- | Build a single selector function for field @fieldIdx@ of @con@ in
-- record type @dd@. Skipped if @existing@ already contains the field name.
mkSelector
  :: Set.Set T.Text  -- ^ already-defined top-level names
  -> DataDecl
  -> ConDecl
  -> (Int, (Name, Type))
  -> [Def]
mkSelector existing dd con (fieldIdx, (fieldName, fieldTy))
  | nameText fieldName `Set.member` existing = []
  | otherwise =
      let recordTy = dataDeclType dd
          paramName = Name "rec" 0
          paramExpr = EVar paramName
          -- One PatVar for the field we want, PatWild for the others.
          mkPat i (_, ty)
            | i == fieldIdx = PatVar (Name "fld" 0) ty
            | otherwise     = PatWild ty
          conPat = PatCon (conName con)
                          [ mkPat i f | (i, f) <- zip [0..] (conFields con) ]
          body = ECase paramExpr
                   [ Branch conPat Nothing (EVar (Name "fld" 0)) ]
          selectorTy = TFun [(Many, recordTy)] EffectRowEmpty fieldTy
          selectorExpr = ELam [(paramName, recordTy)] body
          selectorQN = QName (qnameModule (dataName dd)) fieldName
      in [ Def
             { defName       = selectorQN
             , defType       = selectorTy
             , defExpr       = selectorExpr
             , defSort       = DefFun
             , defVisibility = Public
             }
         ]

-- | The fully-applied type of a 'DataDecl', e.g. @Point@ or @Pair a b@.
-- Type parameters become 'TVar' references; nullary data declarations are
-- just the bare 'TCon'.
dataDeclType :: DataDecl -> Type
dataDeclType dd =
  let tc = TCon (TypeCon (dataName dd) KindValue)
  in foldl TApp tc [TVar tv | tv <- dataParams dd]
