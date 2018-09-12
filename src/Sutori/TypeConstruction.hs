module Sutori.TypeConstruction where

import Sutori.AST
import Sutori.Monad


-- Type construction
buildTypeBool :: SutParserM
buildTypeChar :: SutParserM
buildTypeInt :: SutParserM
buildTypeFloat :: SutParserM
buildTypeString :: SutParserM
buildTypeArray :: SutExpression -> SutType -> SutParserM
buildTypeStruct :: [(SutType, SutID)] -> SutParserM
buildTypeUnion :: [(SutType, SutID)] -> SutParserM
buildTypePointer :: SutType -> SutParserM
buildTypeNamed :: SutParserM
