{-|
Description : Interactions with the SymTable
-}
module Sutori.SymTable.Actions
( isFunction, isMember, isModule, isType, isPerson, isVariable
, insert
, insertSymbol
, insertParams
, symTypeDef
, lookupID
) where

import qualified Data.Map as Map

import Sutori.AST   (SutID)
import Sutori.Types (SutTypeID)

import Sutori.SymTable.Data



-- Utils
---------------------------------------------------------------------------------------------------
-- |Extracts a type symbol type definition (SutTypeID)
symTypeDef :: SutSymbol -> SutTypeID
symTypeDef = otherTypeDef . symOther

-- |Boolean functions to check if a symbol if of some category
isFunction, isMember, isModule, isParameter, isPerson, isType, isVariable :: SutSymCategory -> Bool
isFunction CatFunction   = True
isFunction _             = False
isMember CatMember       = True
isMember _               = False
isModule CatModule       = True
isModule _               = False
isParameter CatParameter = True
isParameter _            = False
isPerson CatPerson       = True
isPerson _               = False
isType CatType           = True
isType _                 = False
isVariable CatVariable   = True
isVariable _             = False



-- Insertion
-- ------------------------------------------------------------------------------------------------
-- |Folds a list of IDs with a given function over the table
insertWith :: SymTable -> [a] -> (SymTable -> a -> SymTable) -> SymTable
insertWith table ids f = foldl f table ids

-- |Inserts symbol into the table the given constructed symbol
insertSymbol :: SymTable -> SutSymbol -> SymTable
insertSymbol table s = let id   = symID s
                           syms = lookupID table id
                        in Map.insert id (s:syms) table

-- |Inserts new symbols into the table (from a list of IDs)
insert :: SymTable -> Scope -> SutSymCategory -> SutTypeID -> SutSymOther -> [SutID] -> SymTable
insert table scope cat t o ids = insertWith table ids insertID
  where insertID table id = let newSymbol = SutSymbol { symID    = id,
                                                        symCat   = cat,
                                                        symScope = scope,
                                                        symType  = t,
                                                        symOther = o }
                             in insertSymbol table newSymbol

-- |Inserts new symbols into the table (from a list of params)
insertParams :: SymTable -> Scope -> SutSymCategory -> [SutParam] -> SymTable
insertParams table scope cat ps = insertWith table ps insertParam
  where insertParam table' p = let newSymbol = SutSymbol { symID    = paramID p,
                                                           symCat   = cat, -- TODO: Isn't this always CatParameter?
                                                           symScope = scope,
                                                           symType  = paramType p,
                                                           symOther = SymParamKind $ paramKind p }
                                in insertSymbol table' newSymbol


-- Lookup
---------------------------------------------------------------------------------------------------
-- |Search for all symbols that match a SutID
lookupID :: SymTable -> SutID -> [SutSymbol]
lookupID table id = Map.findWithDefault [] id table

-- |Search for all symbols that match a token in a given scope
lookupInScope :: SymTable -> Scope -> SutID -> [SutSymbol]
lookupInScope table scope id = filter isHere $ lookupID table id
  where isHere = (== scope) . symScope

-- |Makes several token searches in the same scope
lookupsInScope :: SymTable -> Scope -> [SutID] -> [[SutSymbol]]
lookupsInScope table scope = map (lookupInScope table scope)
