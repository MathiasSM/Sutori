module Sutori.Types
( SutType(..)
, SutTypedExpression(getExpressionType)
, showMember
, primitiveTypes
) where

import Sutori.Logger(SutShow(showSut), SutLog(SutLogLeave, SutLogNode))

type SutMember = (String, SutType)

-- Sutori data types
data SutType  = SutTypeInt
              | SutTypeFloat
              | SutTypeString
              | SutTypeBool
              | SutTypeChar
              | SutTypePointer SutType
              | SutTypeStruct [SutMember]
              | SutTypeUnion [SutMember]
              | SutTypeArray SutType Int
              | SutTypeVoid
              | SutTypeError
              deriving (Show, Eq)

-- Types string representation (for easier user reading)
instance SutShow SutType where
  showSut SutTypeInt          = SutLogLeave "Bag (Int)"
  showSut SutTypeFloat        = SutLogLeave "Wallet (Float)"
  showSut SutTypeString       = SutLogLeave "Phrase (String)"
  showSut SutTypeBool         = SutLogLeave "Light (Bool)"
  showSut SutTypeChar         = SutLogLeave "Letter (Char)"
  showSut SutTypeVoid         = SutLogLeave "Nothing (Void)"
  showSut SutTypeError        = SutLogLeave "Type Error"
  showSut (SutTypePointer t)  = SutLogNode  "Direction (Pointer) to:" [showSut t]
  showSut (SutTypeStruct ms)  = SutLogNode  "Machine (Struct) with:"  (map showMember ms)
  showSut (SutTypeUnion ms)   = SutLogNode  "Thing (Union) with:"     (map showMember ms)
  showSut (SutTypeArray t s)  = SutLogNode  ("Chain (Array) of size " ++ show s ++ " and type:") [showSut t]

showMember (id, t) = SutLogNode (show id ++ " of type:") [showSut t]

-- Predefined Sutori types to initialize symtable
primitiveTypes =
  [ ("bag"   , SutTypeInt)
  , ("wallet", SutTypeFloat)
  , ("phrase", SutTypeString)
  , ("light" , SutTypeBool)
  , ("letter", SutTypeChar)
  ]


-- From two types, return the most general one (LCA)
generalizeTypes :: SutType -> SutType -> SutType

generalizeTypes SutTypeVoid  SutTypeBool  = SutTypeBool
generalizeTypes SutTypeVoid  SutTypeChar  = SutTypeChar
generalizeTypes SutTypeVoid  SutTypeInt   = SutTypeInt
generalizeTypes SutTypeVoid  SutTypeFloat = SutTypeFloat

generalizeTypes SutTypeBool  SutTypeVoid  = SutTypeBool
generalizeTypes SutTypeBool  SutTypeChar  = SutTypeChar
generalizeTypes SutTypeBool  SutTypeInt   = SutTypeInt
generalizeTypes SutTypeBool  SutTypeFloat = SutTypeFloat

generalizeTypes SutTypeChar  SutTypeVoid  = SutTypeChar
generalizeTypes SutTypeChar  SutTypeBool  = SutTypeChar
generalizeTypes SutTypeChar  SutTypeInt   = SutTypeInt
generalizeTypes SutTypeChar  SutTypeFloat = SutTypeFloat

generalizeTypes SutTypeInt   SutTypeVoid  = SutTypeInt
generalizeTypes SutTypeInt   SutTypeBool  = SutTypeInt
generalizeTypes SutTypeInt   SutTypeChar  = SutTypeInt
generalizeTypes SutTypeInt   SutTypeFloat = SutTypeFloat

generalizeTypes SutTypeFloat SutTypeVoid  = SutTypeFloat
generalizeTypes SutTypeFloat SutTypeBool  = SutTypeFloat
generalizeTypes SutTypeFloat SutTypeChar  = SutTypeFloat
generalizeTypes SutTypeFloat SutTypeInt   = SutTypeFloat

generalizeTypes t1 t2 = if t1 == t2 then t1 else SutTypeError


-- Go to a boolean type
toTypeBool :: SutType -> SutType
toTypeBool SutTypeVoid = SutTypeBool
toTypeBool SutTypeBool = SutTypeBool
toTypeBool SutTypeChar = SutTypeBool
toTypeBool SutTypeInt  = SutTypeBool
toTypeBool _           = SutTypeError

-- Go to the most specific numerical type available
toTypeInt :: SutType -> SutType
toTypeInt SutTypeVoid  = SutTypeInt
toTypeInt SutTypeBool  = SutTypeInt
toTypeInt SutTypeChar  = SutTypeInt
toTypeInt SutTypeInt   = SutTypeInt
toTypeInt SutTypeFloat = SutTypeInt
toTypeInt _            = SutTypeError

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
