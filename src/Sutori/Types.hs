module Sutori.Types where

type SutMember = (String, SutType)

data SutType  = SutTypeInt
              | SutTypeFloat
              | SutTypeString
              | SutTypeBool
              | SutTypePointer SutType
              | SutTypeStruct [SutMember]
              | SutTypeUnion [SutMember]
              | SutTypeArray SutType Int
              deriving (Show, Eq)
