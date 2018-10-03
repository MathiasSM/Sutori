{-|
Description : Defines a positin to keep track of in the scanner
-}
module Sutori.Lexer.Posn(SutPosn(SutPosn), initialPosn) where

-- |Source position (Characters before, Row number, Column number)
data SutPosn = SutPosn !Int !Int !Int
  deriving (Eq,Show)

-- |Initial position of the scanner
initialPosn :: SutPosn
initialPosn = SutPosn 0 1 1
