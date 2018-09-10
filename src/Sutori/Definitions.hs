module Sutori.Definitions where

import Sutori.AST
import Sutori.Monad

-- Declarations / Definitions
defPeople :: SutID -> SutParserM
defFunction :: SutID -> SutType -> [SutParam] -> SutBlock -> SutParserM
defFunction' :: SutID -> SutParserM
defVariable :: SutID -> SutType -> [(SutID, Maybe SutExpression)] -> SutParserM
defType :: SutID -> SutID -> SutType -> SutParserM
