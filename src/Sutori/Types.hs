module Sutori.Types
(
  SutMember, SutType(..), SutTypedExpression(..), typesLCA, predefinedTypes
) where

import Sutori.Utils

type SutMember = (String, SutType)

data SutType  = SutTypeInt
              | SutTypeFloat
              | SutTypeString
              | SutTypeBool
              | SutTypeChar
              | SutTypeVoid
              | SutTypeError
              | SutTypePointer SutType
              | SutTypeStruct [SutMember]
              | SutTypeUnion [SutMember]
              | SutTypeArray SutType Int
              deriving (Show, Eq)

instance SutShow SutType where
  showSut SutTypeInt          = "Bag type (Int)"
  showSut SutTypeFloat        = "Wallet type (Float)"
  showSut SutTypeString       = "Phrase type (String)"
  showSut SutTypeBool         = "Light type (Bool)"
  showSut SutTypeChar         = "Letter type (Char)"
  showSut SutTypeVoid         = "No type (Void)"
  showSut SutTypeError        = "Type error"
  showSut (SutTypePointer t)  = "Direction type (Pointer) to type: { "++showSut t++" }"
  showSut (SutTypeStruct ms)  = "Machine type (Struct) with: { "++concatMap showMember ms++" }"
  showSut (SutTypeUnion ms)   = "Thing type (Union) with: { "++concatMap showMember ms++ " }"
  showSut (SutTypeArray t s)  = "Chain type (Array) of size "++show s++" and contained type: { "++showSut t++" }"

showMember (s, t) = "( "++show s++": "++showSut t++" )"


class SutTypedExpression a where
  getExpressionType :: a -> SutType

typesLCA :: SutType -> SutType -> SutType
typesLCA SutTypeInt SutTypeFloat  = SutTypeFloat
typesLCA SutTypeInt SutTypeBool   = SutTypeInt
typesLCA SutTypeInt SutTypeChar   = SutTypeInt

typesLCA SutTypeFloat SutTypeInt  = SutTypeFloat
typesLCA SutTypeFloat SutTypeChar = SutTypeFloat
typesLCA SutTypeFloat SutTypeBool = SutTypeFloat

typesLCA SutTypeChar SutTypeInt   = SutTypeInt
typesLCA SutTypeChar SutTypeFloat = SutTypeFloat
typesLCA SutTypeChar SutTypeBool  = SutTypeInt

typesLCA SutTypeBool SutTypeInt   = SutTypeInt
typesLCA SutTypeBool SutTypeFloat = SutTypeFloat
typesLCA SutTypeBool SutTypeChar  = SutTypeInt

typesLCA SutTypeError _ = SutTypeError
typesLCA _ SutTypeError = SutTypeError
typesLCA t1 t2 = if t1 == t2 then t1 else SutTypeError


predefinedTypes =
  [
    "bag",
    "wallet",
    "phrase",
    "light",
    "direction",
    "machine",
    "thing",
    "chain"
  ]
