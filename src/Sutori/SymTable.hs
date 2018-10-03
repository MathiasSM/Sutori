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
, insert
, insertSymbol
, insertParams
, symTypeDef
, lookupID
) where

import qualified Data.Map as Map

import Sutori.AST(SutID, SutBlock)
import Sutori.Types.Primitives(SutTypeID)


-- Data definition
---------------------------------------------------------------------------------------------------

-- |A 'Scope' is just a number uniquely representing a scope (NOT a level)
type Scope = Int

-- |A 'SutSymCategory'. Can be shown by showSut
data SutSymCategory = CatModule     -- ^ A module
                    | CatFunction   -- ^ A (top-level) function
                    | CatPerson     -- ^ A person who may do things in the story
                    | CatVariable   -- ^ A variable
                    | CatParameter  -- ^ A function parameter
                    | CatType       -- ^ A reference to a type
                    | CatMember     -- ^ A member of a structured type (does not go inot the SymTable)
                    deriving Eq


-- |A 'SutSymOther' represents the extra payload of a symbol and
-- it's either a function definition's AST, the parameter kind, the type definition or nothing,
-- depending on the kind of symbol
data SutSymOther = SymAST       { otherASTParams :: [SutParam], otherASTBlock :: SutBlock }
                 | SymParamKind { otherParamKind :: SutParamKind }
                 | SymTypeDef   { otherTypeDef   :: SutTypeID }
                 | SymNothing


-- |A 'SutSymbol' represents a symbol in the SymTable of a Sutori parsing.
-- It is made of a Token, a Category, a Scope, a Type, and perhaps 'other' information
data SutSymbol = SutSymbol {
  symID    :: SutID,           -- ^ The symbol ID
  symCat   :: SutSymCategory,  -- ^ The category of the symbol
  symScope :: Scope,           -- ^ The scope the symbol is defined
  symType  :: SutTypeID,       -- ^ The type of the symbol, if any (might be void)
  symOther :: SutSymOther      -- ^ The extra payload some symbol kinds have
}


-- |A 'SutParam' represents a function parameter, and
-- it has a kind, type and ID
data SutParam = SutParam {
  paramKind :: SutParamKind,   -- ^ Whether it is a reference or a value parameter
  paramType :: SutTypeID,      -- ^ The type of the parameter
  paramID   :: SutID           -- ^ The ID of the parameter
}

-- |A 'SutParamKind' represents a function parameter kind, that is
-- it can be passed either by reference or by value.
data SutParamKind = SutRef | SutVal

-- |A 'SymTable' represents the SymTable of a Sutori parsing, so
-- it matches a SutID (symbol name) to a list of symbols
type SymTable = Map.Map SutID [SutSymbol]



-- Actions on the SymTable
-- ===============================================================================================

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

-- Utils
---------------------------------------------------------------------------------------------------
-- |Extracts a type symbol type definition (SutTypeID)
symTypeDef :: SutSymbol -> SutTypeID
symTypeDef = otherTypeDef . symOther
