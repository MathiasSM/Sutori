{-|
Description : Defines the SymTable structure and functions to deal with it.

This module defines all the interaction with the table, along with the symbols themselves,
their different categories and payloads.
-}
module Sutori.SymTable
( SutSymbol(..)
, SutSymbol'(..)
, ASTSymbol(..)
, OffsetSymbol(..)
, ParametricSymbol(..)
, Scope
, SutParam(..)
, SymFunction(..)
, SymModule(..)
, SymPerson(..)
, SymTable
, SymType(..)
, SymVariable(..)
, SymbolCat(..)
, TypedSymbol(..)
, insertSymbol
, lookupSymbols , lookupSymbolsVariable, lookupSymbolsPerson, lookupSymbolsModule, lookupSymbolsType, lookupSymbolsFunction
, updateSymbol
, paramByValue, paramByRef

, lookupAllFunctions
) where

import Sutori.SymTable.Symbol
import Sutori.SymTable.Table
import Sutori.SymTable.Logger
