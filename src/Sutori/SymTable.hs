module Sutori.SymTable
( SutSymbol(SutSymbol, symID, symCat, symScope, symType, symOther)
, SutSymCategory(..)
, SutSymOther(..)
, SymTable
, Scope
, SutParamKind(..)
, SutParam(..)
, insert
, insertSymbol
, insertParams
, symTypeDef
, lookupID
, compareParamTypes
) where

import qualified Data.Map as Map

import Sutori.AST(SutID, SutBlock)
import Sutori.Types.Primitives(SutTypeID)


-- Data definition
---------------------------------------------------------------------------------------------------

-- A Scope is just a number uniquely representing a scope (NOT a level)
type Scope = Int

-- A SutSymCategory. Can be shown by showSut
data SutSymCategory = CatModule
                    | CatFunction
                    | CatPerson
                    | CatVariable
                    | CatParameter
                    | CatType
                    | CatMember
                    deriving Eq

-- A SutParam can be either a sutori reference or a value
data SutParamKind = SutRef | SutVal

-- A SutSymOther is either a Function definition AST, the parameter kind, the type definition or nothing
data SutSymOther = SymAST       { otherASTParams :: [SutParam], otherASTBlock :: SutBlock }
                 | SymParamKind { otherParamKind :: SutParamKind }
                 | SymTypeDef   { otherTypeDef   :: SutTypeID }
                 | SymNothing

-- A SutSymbol is made of a Token, a Category, a Scope, a Type, and perhaps other information
data SutSymbol = SutSymbol {
  symID    :: SutID,
  symCat   :: SutSymCategory,
  symScope :: Scope,
  symType  :: SutTypeID,
  symOther :: SutSymOther
}

-- A SutParam has a kind, a type and a token representing
data SutParam = SutParam {
  paramKind :: SutParamKind,
  paramType :: SutTypeID,
  paramID   :: SutID
}

-- A SymTable matches a SutID (symbol name) to a list of symbols
type SymTable = Map.Map SutID [SutSymbol]



-- Actions on the SymTable
-- ===============================================================================================

-- Insertion
-- ------------------------------------------------------------------------------------------------
-- Folds a list of IDs with a given function over the table
insertWith :: SymTable -> [a] -> (SymTable -> a -> SymTable) -> SymTable
insertWith table ids f = foldl f table ids

-- Inserts symbol into the table the given constructed symbol
insertSymbol :: SymTable -> SutSymbol -> SymTable
insertSymbol table s = let id   = symID s
                           syms = lookupID table id
                        in Map.insert id (s:syms) table

-- Inserts new symbols into the table (from a list of IDs)
insert :: SymTable -> Scope -> SutSymCategory -> SutTypeID -> SutSymOther -> [SutID] -> SymTable
insert table scope cat t o ids = insertWith table ids insertID
  where insertID table id = let newSymbol = SutSymbol { symID    = id,
                                                        symCat   = cat,
                                                        symScope = scope,
                                                        symType  = t,
                                                        symOther = o }
                             in insertSymbol table newSymbol

-- Inserts new symbols into the table (from a list of params)
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
-- Search for all symbols that match a SutID
lookupID :: SymTable -> SutID -> [SutSymbol]
lookupID table id = Map.findWithDefault [] id table

-- Search for all symbols that match a token in a given scope
lookupInScope :: SymTable -> Scope -> SutID -> [SutSymbol]
lookupInScope table scope id = filter isHere $ lookupID table id
  where isHere = (== scope) . symScope

-- Makes several token searches in the same scope
lookupsInScope :: SymTable -> Scope -> [SutID] -> [[SutSymbol]]
lookupsInScope table scope = map (lookupInScope table scope)

-- Utils
---------------------------------------------------------------------------------------------------
-- Get a type symbol type definition (SutTypeID)
symTypeDef :: SutSymbol -> SutTypeID
symTypeDef = otherTypeDef . symOther

compareParamTypes :: SutParam -> SutParam -> Bool
compareParamTypes fp p = paramType fp == paramType p
