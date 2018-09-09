module Sutori.Types where

import Sutori.Utils

type SutMember = (String, SutType)

-- Sutori data types
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

-- Types string representation (for easier user reading)
instance SutShow SutType where
  showSut SutTypeInt          = "Bag (Int)"
  showSut SutTypeFloat        = "Wallet (Float)"
  showSut SutTypeString       = "Phrase (String)"
  showSut SutTypeBool         = "Light (Bool)"
  showSut SutTypeChar         = "Letter (Char)"
  showSut SutTypeVoid         = "Nothing (Void)"
  showSut SutTypeError        = "Error"
  showSut (SutTypePointer t)  = "Direction (Pointer) to: { "++showSut t++" }"
  showSut (SutTypeStruct ms)  = "Machine (Struct) with: { "++concatMap showMember ms++" }"
  showSut (SutTypeUnion ms)   = "Thing (Union) with: { "++concatMap showMember ms++ " }"
  showSut (SutTypeArray t s)  = "Chain (Array) of "++show s++": { "++showSut t++" }"

showMember (s, t) = "( "++show s++": "++showSut t++" )"

-- Predefined Sutori types to initialize symtable
basicTypes =
  [
    ("bag",    SutTypeInt),
    ("wallet", SutTypeFloat),
    ("phrase", SutTypeString),
    ("light",  SutTypeBool),
    ("letter", SutTypeChar)
  ]


-- From two types, return the most general one (LCA)
generalizeTypes :: SutType -> SutType -> SutType

generalizeTypes SutTypeVoid SutTypeBool  = SutTypeBool
generalizeTypes SutTypeVoid SutTypeChar  = SutTypeChar
generalizeTypes SutTypeVoid SutTypeInt   = SutTypeInt
generalizeTypes SutTypeVoid SutTypeFloat = SutTypeFloat

generalizeTypes SutTypeBool SutTypeVoid  = SutTypeBool
generalizeTypes SutTypeBool SutTypeChar  = SutTypeChar
generalizeTypes SutTypeBool SutTypeInt   = SutTypeInt
generalizeTypes SutTypeBool SutTypeFloat = SutTypeFloat

generalizeTypes SutTypeChar SutTypeVoid  = SutTypeChar
generalizeTypes SutTypeChar SutTypeBool  = SutTypeChar
generalizeTypes SutTypeChar SutTypeInt   = SutTypeInt
generalizeTypes SutTypeChar SutTypeFloat = SutTypeFloat

generalizeTypes SutTypeInt SutTypeVoid   = SutTypeInt
generalizeTypes SutTypeInt SutTypeBool   = SutTypeInt
generalizeTypes SutTypeInt SutTypeChar   = SutTypeInt
generalizeTypes SutTypeInt SutTypeFloat  = SutTypeFloat

generalizeTypes SutTypeFloat SutTypeVoid = SutTypeFloat
generalizeTypes SutTypeFloat SutTypeBool = SutTypeFloat
generalizeTypes SutTypeFloat SutTypeChar = SutTypeFloat
generalizeTypes SutTypeFloat SutTypeInt  = SutTypeFloat

generalizeTypes t1 t2 = if t1 == t2 then t1 else SutTypeError


-- Go to a boolean type
toTypeBool :: SutType -> SutType
toTypeBool SutTypeVoid = SutTypeBool
toTypeBool SutTypeBool = SutTypeBool
toTypeBool SutTypeChar = SutTypeBool
toTypeBool SutTypeInt  = SutTypeBool
toTypeBool _           = SutTypeError

-- Go to the most specific numerical type available
toTypeNum :: SutType -> SutType
toTypeNum SutTypeVoid  = SutTypeInt
toTypeNum SutTypeBool  = SutTypeInt
toTypeNum SutTypeChar  = SutTypeInt
toTypeNum SutTypeInt   = SutTypeInt
toTypeNum SutTypeFloat = SutTypeFloat
toTypeNum _            = SutTypeError

-- Go to the general float type
toTypeFloat :: SutType -> SutType
toTypeFloat SutTypeVoid  = SutTypeFloat
toTypeFloat SutTypeBool  = SutTypeFloat
toTypeFloat SutTypeChar  = SutTypeFloat
toTypeFloat SutTypeInt   = SutTypeFloat
toTypeFloat SutTypeFloat = SutTypeFloat
toTypeFloat _            = SutTypeError


-- Class for typed expressions
class SutTypedExpression a where
  getExpressionType :: a -> SutType

getNumExprType :: SutTypedExpression a => a -> SutType
getNumExprType = toTypeNum . getExpressionType

getBoolExprType :: SutTypedExpression a => a -> SutType
getBoolExprType = toTypeBool . getExpressionType

-- binaryOp
-- binaryNum2NumType e1 e2   = toTypeNum $ typesLCA (getNumExprType e1) (getNumExprType e2)
-- binaryNum2FloatType e1 e2 = toTypeFloat $ typesLCA (getNumExprType e1) (getNumExprType e2)
-- binaryNum2BoolType e1 e2  = toTypeBool $ typesLCA (getNumExprType e1) (getNumExprType e2)
--
-- binaryBoolType e1 e2 = toTypeBool $ typesLCA (getExpressionType e1) (getExpressionType e2)
--
-- binaryEqualityType e1 e2 = let t1 = getExpressionType e1
--                                t2 = getExpressionType e2
--                             in if (t1 == t2) || (typesLCA t1 t2 /= SutTypeError)
--                                   then SutTypeBool
--                                   else SutTypeError
