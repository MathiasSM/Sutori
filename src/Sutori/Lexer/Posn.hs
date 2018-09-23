module Sutori.Lexer.Posn(SutPosn(SutPosn), initialPosn) where

-- Source position: Number of characters before, row, col
data SutPosn = SutPosn !Int !Int !Int
  deriving (Eq,Show)

initialPosn = SutPosn 0 1 1
