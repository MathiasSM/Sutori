module Sutori.Types where

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
typesLCA SutTypeVoid _ = SutTypeVoid
typesLCA _ SutTypeVoid = SutTypeVoid

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


toTypeNum :: SutType -> SutType
toTypeNum SutTypeFloat = SutTypeFloat
toTypeNum SutTypeBool = SutTypeInt
toTypeNum SutTypeInt = SutTypeInt
toTypeNum SutTypeChar = SutTypeInt
toTypeNum _ = SutTypeError

toTypeFloat :: SutType -> SutType
toTypeFloat SutTypeFloat = SutTypeFloat
toTypeFloat SutTypeBool = SutTypeFloat
toTypeFloat SutTypeInt = SutTypeFloat
toTypeFloat SutTypeChar = SutTypeFloat
toTypeFloat _ = SutTypeError

toTypeBool :: SutType -> SutType
toTypeBool SutTypeBool = SutTypeBool
toTypeBool SutTypeInt = SutTypeBool
toTypeBool SutTypeChar = SutTypeBool
toTypeBool _ = SutTypeError


getNumExprType :: SutTypedExpression a => a -> SutType
getNumExprType = toTypeNum . getExpressionType

getBoolExprType :: SutTypedExpression a => a -> SutType
getBoolExprType = toTypeBool . getExpressionType

binaryNum2NumType e1 e2   = toTypeNum $ typesLCA (getNumExprType e1) (getNumExprType e2)
binaryNum2FloatType e1 e2 = toTypeFloat $ typesLCA (getNumExprType e1) (getNumExprType e2)
binaryNum2BoolType e1 e2  = toTypeBool $ typesLCA (getNumExprType e1) (getNumExprType e2)

binaryBoolType e1 e2 = toTypeBool $ typesLCA (getExpressionType e1) (getExpressionType e2)

binaryEqualityType e1 e2 = let t1 = getExpressionType e1
                               t2 = getExpressionType e2
                            in if (t1 == t2) || (typesLCA t1 t2 /= SutTypeError)
                                  then SutTypeBool
                                  else SutTypeError


predefinedTypes =
  [
    ("bag", SutTypeInt),
    ("wallet", SutTypeFloat),
    ("phrase", SutTypeString),
    ("light", SutTypeBool),
    ("letter", SutTypeChar)
  ]
