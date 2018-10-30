{-|
Description: Defines the symbol table and common functions to handle it
-}
module Sutori.SymTable.Table
( SymTable
, insertSymbol
, updateSymbol
, lookupSymbols

, lookupSymbolsVariable
, lookupSymbolsFunction
, lookupSymbolsModule
, lookupSymbolsType
, lookupSymbolsPerson

, lookupAllFunctions
) where

import qualified Data.Map as Map

import Sutori.AST (SutID)

import Sutori.SymTable.Symbol

-- |A 'SymTable' represents the SymTable of a Sutori parsing, so
-- it matches a SutID (symbol name) to a list of symbols
type SymTable = Map.Map SutID [SutSymbol']

-- | Inserts a symbol into its ID stack in the map
insertSymbol :: SutSymbol' -> SymTable -> SymTable
insertSymbol s = Map.insertWith (\[s] ss -> s:ss) (symID s) [s]

-- | Updates (read: replaces) the head of the symbol stack (for this symbol's ID
updateSymbol :: SutSymbol' -> SymTable -> SymTable
updateSymbol s = Map.insertWith (\[s] (_:ss) -> s:ss) (symID s) [s]


-- | Lookup the list of symbols using the given ID
lookupSymbols :: SutID -> SymTable -> Maybe [SutSymbol']
lookupSymbols = Map.lookup


-- | Lookup the list of some symbol category with a given ID
lookupSymbolsSome :: SutSymbol a => (SymbolCat -> Bool) -> (SutSymbol' -> a) -> SutID -> SymTable -> [a]
lookupSymbolsSome isSome unBox id t = maybe [] (map unBox . filter (isSome . symCat)) (lookupSymbols id t)

-- | Lookup the list of variables with an ID
lookupSymbolsVariable :: SutID -> SymTable -> [SymVariable]
lookupSymbolsVariable = lookupSymbolsSome isVariable unBoxVariable

-- | Lookup the list of functions with an ID
lookupSymbolsFunction :: SutID -> SymTable -> [SymFunction]
lookupSymbolsFunction = lookupSymbolsSome isFunction unBoxFunction

-- | Lookup the list of modules with an ID
lookupSymbolsModule :: SutID -> SymTable -> [SymModule]
lookupSymbolsModule = lookupSymbolsSome isModule unBoxModule

-- | Lookup the list of types with an ID
lookupSymbolsType :: SutID -> SymTable -> [SymType]
lookupSymbolsType = lookupSymbolsSome isType unBoxType

-- | Lookup the list of persons with an ID
lookupSymbolsPerson :: SutID -> SymTable -> [SymPerson]
lookupSymbolsPerson = lookupSymbolsSome isPerson unBoxPerson


-- | Lookup all defined symbols of a category (with any given ID) in a given Scope
lookupAllSome :: SutSymbol a => (SymbolCat -> Bool) -> (SutSymbol' -> a) -> Scope -> SymTable -> [a]
lookupAllSome isSome unBox scope = Map.foldr f []
  where f s' ss = (map unBox . filter (isSome . symCat)) s' ++ ss

-- | Lookup all defined functions in a given scope
lookupAllFunctions :: Scope -> SymTable -> [SymFunction]
lookupAllFunctions = lookupAllSome isFunction unBoxFunction
