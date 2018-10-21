{-|
Description : The Table, Symbols and Payload structure
-}
module Sutori.SymTable.Data
( SutSymbol(..)
, SutSymCategory(..)
, SutSymOther(..)
, SymTable
, Scope
, SutParamKind(..)
, SutParam(..)
) where

import qualified Data.Map as Map

import Sutori.AST   (SutID, SutBlock)
import Sutori.Types (SutTypeID)


-- |A 'Scope' is just a number uniquely representing a scope (NOT a level, but an ID)
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


-- |A 'SutParamKind' represents a function parameter kind, that is
-- it can be passed either by reference or by value.
data SutParamKind = SutRef | SutVal


-- |A 'SutParam' represents a function parameter, and
-- it has a kind, type and ID
data SutParam = SutParam {
  paramKind :: SutParamKind,   -- ^ Whether it is a reference or a value parameter
  paramType :: SutTypeID,      -- ^ The type of the parameter
  paramID   :: SutID           -- ^ The ID of the parameter
}


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


-- |A 'SymTable' represents the SymTable of a Sutori parsing, so
-- it matches a SutID (symbol name) to a list of symbols
type SymTable = Map.Map SutID [SutSymbol]
