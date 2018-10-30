{-|
Description : Provides definition functions that add the appropiate symbols to the table,
              given that all checks passed and the definition is legal.

              TODO: Refactor code. all these functions use mostly repeated code
-}
module Sutori.Parser.Definitions where

import Control.Monad            (filterM, when)
import Control.Monad.State.Lazy (get, put)
import Data.List                (find)
import Data.Maybe               (catMaybes, fromJust, isJust)

import Debug.Trace

import Sutori.AST      (SutID, SutExpression, SutAST, SutModule(SutModule), SutInstruction(..))
import Sutori.Error    (duplicateSymbolError)
import Sutori.Monad    (SutMonad, SutState(SutState, parserTable, mainModule), parserCurrentScope)
import Sutori.Types    (SutTypeID)
import Sutori.SymTable

import Sutori.Parser.Expressions (assignment)
import Sutori.Parser.Symbols     (findVariable, findFunction, lookupInScope)


-- |Decides on whether the given 'SutID' was used for a different symbol in the same scope
whenSymbolIsNew :: SutSymbol a => (SutID -> SymTable -> [a]) -> SutID -> SutMonad () -> SutMonad ()
whenSymbolIsNew lookup id a = do
  s    <- get
  vars <- lookupInScope lookup id
  let currentScope = parserCurrentScope s
      isDefined    = not (null vars)
      isCurrent    = isDefined && (symScope (head vars) == currentScope)
  if isCurrent then duplicateSymbolError (head vars) else a


-- |Defines a new variable of given type and optionally assigns it an initial value.
defVariable :: SutID -> SutTypeID -> (SutID, Maybe SutExpression) -> SutMonad (Maybe SutInstruction)
defVariable pid tid (id, mexp) = do
  whenSymbolIsNew lookupSymbolsVariable id $ do
    s@SutState{ parserTable = table } <- get
    let variable = SymVariable' $ SymVariable id currentScope tid
        currentScope = parserCurrentScope s
    put s{parserTable = insertSymbol variable table}
  case mexp of
    Just exp -> return $ Just (InstExpression exp)
    Nothing  -> return Nothing

-- |Associates the SutID to the newly constructed type, assuming the name has not been used before
defType :: SutID -> SutID -> SutTypeID -> SutMonad ()
defType pid id tid = whenSymbolIsNew lookupSymbolsType id $ do
  s@SutState{ parserTable = table } <- get
  let variable = SymType' $ SymType id currentScope tid
      currentScope = parserCurrentScope s
  put s{parserTable = insertSymbol variable table}

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
  where defParam p@SutParam{paramType = t, paramID = id} = defVariable (if isRef p then "-" else "") t (id, Nothing)

-- |Defined a list of variables and returns a list of instructions, if any definition included an assigment
defVariables :: SutID -> SutTypeID -> [(SutID, Maybe SutExpression)] -> SutMonad [SutInstruction]
defVariables pid tid defs = fmap catMaybes (mapM (defVariable pid tid) defs)

-- |Defines a module for importing/exporting
--
-- Right now it's basically a stub as there's only one module and
-- the data structure only keeps the 'SutID' and "AST"
defModule :: SutID -> SutAST -> SutMonad ()
defModule id b = get >>= \s -> put s{ mainModule = SutModule id b }

-- |Defines a AST-able function (updates the top function with this ID's AST)
--
-- Note: We are now in the scope the function is being defined
defineFunction :: SutID -> SutAST -> SutMonad ()
defineFunction id ast = do
  SutState{ parserTable = table } <- get
  let (s@(SymFunction _ tid ps pre ast'):_) = lookupSymbolsFunction id table
  case ast' of
    Just _  -> duplicateSymbolError s
    Nothing -> do
      let func = SymFunction' $ SymFunction id tid ps pre (Just ast)
      get >>= \s' -> put s'{ parserTable = updateSymbol func table }

-- |Inserts a new function symbol with a given name, type and parameters,
-- if none is present (no body yet)
--
-- Is this action's job to build the initial function symbol (and that includes the PreAST)
--
-- Note: We are inside the function's scope, so we need to insert the symbol in the second-to-last scope
insertFunction :: SutID -> SutTypeID -> [SutParam] -> SutMonad SutID
insertFunction id tid ps = do
  state@SutState{ parserTable = table } <- get
  case lookupSymbolsFunction id table of
    (f@SymFunction{}:_)  -> when (symType f /= tid || symParams f /= ps) $ duplicateSymbolError f
    _ -> do
      pre <- defParams ps
      let func = SymFunction' $ SymFunction id tid ps pre Nothing
      s'@SutState{ parserTable = table' } <- get
      put s'{ parserTable = insertSymbol func table' }
  return id
