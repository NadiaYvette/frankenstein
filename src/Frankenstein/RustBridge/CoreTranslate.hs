-- | Rust MIR -> Frankenstein Core Translation
--
-- Translates rustc's MIR (post-borrow-check) into Frankenstein Core.
-- MIR is a CFG of basic blocks — lower level than GHC Core or Mercury HLDS.
-- The key advantage: ownership is already resolved by rustc.
--
-- MIR operations -> Core:
--   Move(_x)           -> EVar x + EDrop x (consumed, Perceus release)
--   Copy(_x)           -> EVar x + ERetain x (refcount increment)
--   Drop(_x)           -> EDrop x (explicit destructor)
--   StorageLive(_x)    -> (scope marker, allocate)
--   StorageDead(_x)    -> EDrop x (scope exit, release)
--   _x = Rvalue        -> ELet binding
--   Call(fn, args)      -> EApp
--   SwitchInt           -> ECase
--   Return              -> result variable
--   Goto(bbN)           -> (control flow, handled by CFG->expression conversion)
--
-- The main challenge: converting CFG (basic blocks) back to structured
-- expressions (let/case/lambda). We use a simple dominator-based algorithm.

module Frankenstein.RustBridge.CoreTranslate
  ( translateMir
  , translateBody
  ) where

import Frankenstein.Core.Types
import Frankenstein.RustBridge.MirParse

import Data.Text (Text)
import qualified Data.Text as T

-- | Translate a full MIR program to Frankenstein Core
translateMir :: MirProgram -> Either Text Program
translateMir prog = do
  defs <- mapM translateBody (mirBodies prog)
  Right $ Program
    { progName = QName "rust" (Name "main" 0)
    , progDefs = defs
    , progData = []
    , progEffects = []  -- Rust has no user-defined effects (IO is implicit)
    }

-- | Translate a single MIR function body to a Frankenstein definition
translateBody :: MirBody -> Either Text Def
translateBody body = do
  let name = QName "rust" (Name (mirName body) 0)

      -- Build argument types from local declarations
      -- In MIR, _0 is the return place, _1.._N are arguments
      argLocals = take (mirArgCount body) (drop 1 (mirLocals body))
      argTypes = [(Affine, localTypeToType l) | l <- argLocals]

      -- Return type from _0
      retType = case mirLocals body of
        (ret:_) -> localTypeToType ret
        []      -> TCon (TypeCon (QName "std" (Name "unit" 0)) KindValue)

      -- Rust functions have IO effect (they can do arbitrary side effects)
      effRow = EffectRowExtend (QName "std" (Name "io" 0)) EffectRowEmpty

      funType = TFun argTypes effRow retType

      -- Translate basic blocks to expression tree
      expr = translateBlocks body

  Right $ Def
    { defName = name
    , defType = funType
    , defExpr = expr
    , defSort = DefFun
    , defVisibility = Public
    }

-- | Convert a MIR type string to Frankenstein Type
-- Phase 1: opaque type references. Phase 2: parse the type properly.
localTypeToType :: MirLocalDecl -> Type
localTypeToType decl =
  TCon (TypeCon (QName "rust" (Name (localType decl) 0)) KindValue)

-- | Convert MIR basic blocks (CFG) into a structured expression.
--
-- Strategy: Simple cases first.
-- - Linear sequence of blocks (no branches) -> let-chain
-- - SwitchInt -> case expression
-- - Return -> return the _0 variable
-- - Drop + Goto -> EDrop + continue
--
-- For the general case (loops, complex control flow), we need
-- dominator analysis or a reducibility check. Rust's MIR is
-- always reducible (no irreducible control flow), so we can
-- use interval analysis.
translateBlocks :: MirBody -> Expr
translateBlocks body =
  case mirBlocks body of
    [] -> ELit (LitInt 0)  -- empty function
    (bb:_) -> translateBlock body bb

translateBlock :: MirBody -> MirBasicBlock -> Expr
translateBlock body bb =
  let -- Translate statements as a chain of let-bindings
      stmtExprs = map translateStatement (bbStatements bb)
      -- Translate terminator
      termExpr = case bbTerminator bb of
        Just term -> translateTerminator body term
        Nothing   -> EVar (Name "_0" 0)  -- implicit return
      -- Chain: stmt1; stmt2; ...; terminator
  in foldr (\stmt rest ->
        ELet [[Bind (Name "_" 0)
                     (TCon (TypeCon (QName "std" (Name "unit" 0)) KindValue))
                     stmt DefVal]]
             rest)
      termExpr stmtExprs

-- | Translate a MIR statement (currently as text, will be structured later)
translateStatement :: Text -> Expr
translateStatement stmtText
  -- Detect common MIR statement patterns
  | "StorageLive" `T.isInfixOf` stmtText =
      ELit (LitString "storage_live")  -- no-op for now
  | "StorageDead" `T.isInfixOf` stmtText =
      -- Extract variable and emit drop
      let varName = extractVar stmtText
      in EDrop (EVar (Name varName 0))
  | "Assign" `T.isInfixOf` stmtText || " = " `T.isInfixOf` stmtText =
      -- Assignment: will be a let-binding
      EApp (EVar (Name "assign" 0)) [ELit (LitString stmtText)]
  | otherwise =
      EApp (EVar (Name "mir_stmt" 0)) [ELit (LitString stmtText)]

-- | Translate a MIR terminator
translateTerminator :: MirBody -> Text -> Expr
translateTerminator body termText
  | "Return" `T.isInfixOf` termText || "return" `T.isInfixOf` termText =
      EVar (Name "_0" 0)  -- return place
  | "Goto" `T.isInfixOf` termText =
      let target = extractBlockTarget termText
      in case lookupBlock body target of
           Just bb -> translateBlock body bb
           Nothing -> EApp (EVar (Name "goto" 0)) [ELit (LitString termText)]
  | "SwitchInt" `T.isInfixOf` termText =
      -- Branch: becomes a case expression
      EApp (EVar (Name "switch" 0)) [ELit (LitString termText)]
  | "Call" `T.isInfixOf` termText =
      -- Function call
      EApp (EVar (Name "call" 0)) [ELit (LitString termText)]
  | "Drop" `T.isInfixOf` termText =
      let varName = extractVar termText
      in EDrop (EVar (Name varName 0))
  | otherwise =
      EApp (EVar (Name "mir_term" 0)) [ELit (LitString termText)]

-- Helpers

extractVar :: Text -> Text
extractVar t =
  -- Look for _N pattern
  case T.breakOn "_" t of
    (_, rest) | not (T.null rest) ->
      let varPart = T.takeWhile (\c -> c == '_' || (c >= '0' && c <= '9')) rest
      in if T.null varPart then "unknown" else varPart
    _ -> "unknown"

extractBlockTarget :: Text -> Int
extractBlockTarget t =
  -- Look for "bb" followed by a number
  case T.breakOn "bb" t of
    (_, rest) | not (T.null rest) ->
      let numPart = T.takeWhile (\c -> c >= '0' && c <= '9') (T.drop 2 rest)
      in case reads (T.unpack numPart) of
           [(n, _)] -> n
           _ -> 0
    _ -> 0

lookupBlock :: MirBody -> Int -> Maybe MirBasicBlock
lookupBlock body idx =
  case filter (\bb -> bbIndex bb == idx) (mirBlocks body) of
    (bb:_) -> Just bb
    []     -> Nothing
