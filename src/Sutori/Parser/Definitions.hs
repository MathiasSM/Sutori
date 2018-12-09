{-|
Description : Provides definition functions that add the appropiate symbols to the table,
              given that all checks passed and the definition is legal.

              TODO: Refactor code. all these functions use mostly repeated code
-}
module Sutori.Parser.Definitions where

import Control.Monad            (when)
import Control.Monad.State.Lazy (get, put)
import Data.Maybe               (catMaybes, fromJust)
import qualified Data.Map as Map

import Sutori.AST      (SutID, SutExpression(ExprID), SutAST, SutModule(SutModule), SutInstruction(..))
import Sutori.Error    (duplicateSymbolError)
import Sutori.Monad    (SutMonad, SutState(SutState, parserTable, parserOffset, typesGraph, mainModule), parserCurrentScope)
import Sutori.Types    (SutTypeID, lookupType)
import Sutori.SymTable

import Sutori.Parser.Symbols
import Sutori.Parser.Expressions


-- |Decides on whether the given 'SutID' was used for a different symbol in the same scope
whenSymbolIsNew :: SutSymbol a => (SutID -> SymTable -> [a]) -> SutID -> SutMonad () -> SutMonad ()
whenSymbolIsNew lookupSym sid a = do
  s    <- get
  vars <- lookupInScope lookupSym sid
  let currentScope = parserCurrentScope s
      isDefined    = not (null vars)
      isCurrent    = isDefined && (symScope (head vars) == currentScope)
  if isCurrent then duplicateSymbolError (head vars) else a


-- |Defines a new variable of given type and optionally assigns it an initial value.
defVariable :: SutID -> SutTypeID -> (SutID, Maybe SutExpression) -> SutMonad (Maybe SutInstruction)
defVariable pid tid (vid, mexp) = do
  s@SutState
    { parserTable  = table
    , parserOffset = currentOffset
    , typesGraph   = g       } <- get
  let currentScope = parserCurrentScope s

  whenSymbolIsNew lookupSymbolsVariable vid $ do
    let variable = SymVariable' $ SymVariable vid currentScope tid currentOffset
        (Just (_,size)) = lookupType tid g
    put s
      { parserTable  = insertSymbol variable table
      , parserOffset = currentOffset + size }

  case mexp of
    Just expr -> do
      ety <- findExistentType tid
      let left = ExprID ety vid currentScope
      res <- assignment left expr
      return $ Just $ InstExpression res
    Nothing  -> return Nothing

-- |Defines a SutID to be now known as a type
insertType :: SutID -> SutID -> SutMonad SutID
insertType pid sid = whenSymbolIsNew lookupSymbolsType sid insertTypeID >> return sid
  where insertTypeID = do
          s@SutState{ parserTable = table } <- get
          let tvar = SymType' $ SymType sid currentScope (-1)
              currentScope = parserCurrentScope s
          put s{parserTable = insertSymbol tvar table}

-- |Associates the SutID to the newly constructed type
defineType :: SutID -> SutTypeID -> SutMonad ()
defineType sid tid = do
  s@SutState{ parserTable = table } <- get
  let (s@(SymType _ scp _):_) = lookupSymbolsType sid table
      tvar = SymType' $ SymType sid scp tid
  get >>= \s' -> put s'{ parserTable = updateSymbol tvar table }

-- |Includes a new person into the story
defPerson :: SutID -> SutMonad ()
defPerson pid = whenSymbolIsNew lookupSymbolsPerson pid $ do
  s@SutState{ parserTable = table } <- get
  let variable = SymPerson' $ SymPerson pid currentScope
      currentScope = parserCurrentScope s
  put s{parserTable = insertSymbol variable table}

-- |Defines a new parameter into a function's scope.
defParams :: [SutParam] -> SutMonad [SutInstruction]
defParams ps = fmap catMaybes (mapM defParam ps)
  where defParam p@SutParam{paramType = t, paramID = sid} =
          defVariable (if isRef p then "-" else "") t (sid, Nothing)

-- |Defined a list of variables and returns a list of instructions, if any definition included an assigment
defVariables :: SutID -> SutTypeID -> [(SutID, Maybe SutExpression)] -> SutMonad [SutInstruction]
defVariables pid tid defs = fmap catMaybes (mapM (defVariable pid tid) defs)

-- |Defines a module for importing/exporting
--
-- Right now it's basically a stub as there's only one module and
-- the data structure only keeps the 'SutID' and "AST"
defModule :: SutID -> SutAST -> SutMonad ()
defModule mid b = get >>= \s -> put s{ mainModule = SutModule mid b }

-- |Defines a AST-able function (updates the top function with this ID's AST)
--
-- Note: We are now in the scope the function is being defined
defineFunction :: SutID -> SutAST -> SutMonad ()
defineFunction fid ast = do
  SutState{ parserTable = table } <- get
  let (s@(SymFunction _ tid ps pre ast'):_) = lookupSymbolsFunction fid table
  case ast' of
    Just _  -> duplicateSymbolError s
    Nothing -> do
      let func = SymFunction' $ SymFunction fid tid ps pre (Just ast)
      get >>= \s' -> put s'{ parserTable = updateSymbol func table }

-- |Inserts a new function symbol with a given name, type and parameters,
-- if none is present (no body yet)
--
-- Is this action's job to build the initial function symbol (and that includes the PreAST)
--
-- Note: We are inside the function's scope, so we need to insert the symbol in the second-to-last scope
insertFunction :: SutID -> SutTypeID -> [SutParam] -> SutMonad SutID
insertFunction fid tid ps = do
  state@SutState{ parserTable = table } <- get
  case lookupSymbolsFunction fid table of
    (f@SymFunction{}:_)  -> when (symType f /= tid || symParams f /= ps) $ duplicateSymbolError f
    _ -> do
      pre <- defParams ps
      let func = SymFunction' $ SymFunction fid tid ps pre Nothing
      s'@SutState{ parserTable = table' } <- get
      put s'{ parserTable = insertSymbol func table' }
  return fid
