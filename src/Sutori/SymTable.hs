{-|
Description : Defines the SymTable structure and functions to deal with it.

This module defines all the interaction with the table, along with the symbols themselves,
their different categories and payloads.
-}
module Sutori.SymTable
( SutSymbol(SutSymbol, symID, symCat, symScope, symType, symOther)
, SutSymCategory(..)
, SutSymOther(..)
, SymTable
, Scope
, SutParamKind(..)
, SutParam(..)
, isFunction, isMember, isModule, isType, isPerson, isVariable
, insert
, insertSymbol
, insertParams
, symTypeDef
, lookupID
) where

import Sutori.SymTable.Data
import Sutori.SymTable.Actions
import Sutori.SymTable.Logger
