module Sutori.TypeConstruction where

import Sutori.AST(SutExpression, SutLiteral, SutID)
import Sutori.Types(SutType(..))
import Sutori.Monad(SutMonad)


-- Type construction
buildTypeBool :: SutMonad SutType
buildTypeBool = return SutTypeBool

buildTypeChar :: SutMonad SutType
buildTypeChar = return SutTypeChar

buildTypeInt :: SutMonad SutType
buildTypeInt = return SutTypeInt

buildTypeFloat :: SutMonad SutType
buildTypeFloat = return SutTypeFloat

buildTypeString :: SutMonad SutType
buildTypeString = return SutTypeString

buildTypeArray :: SutLiteral -> SutType -> SutMonad SutType


buildTypePointer :: SutType -> SutMonad SutType
buildTypeNamed :: SutMonad SutType

buildTypeStruct :: [(SutType, SutID)] -> SutMonad SutType

buildTypeUnion :: [(SutType, SutID)] -> SutMonad SutType
