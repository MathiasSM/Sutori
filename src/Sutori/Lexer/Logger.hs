{-|
Description : Provides 'ShowSut' instances for "Sutori.Lexer"
-}
module Sutori.Lexer.Logger() where

import Sutori.Logger(SutShow(..), SutLog(SutLogNode, SutLogLeave))
import Sutori.Lexer.Tokens(SutToken(..))
import Sutori.Lexer.Posn(SutPosn(SutPosn))

-- |Sutori tokens can be printed nicely
instance SutShow SutToken where
  showSut SutTkEOF = SutLogLeave   "Token: EOF"
  showSut tk       = SutLogLeave $ "Token: " ++ show tk

-- |Sutori positions can be printed nicely
instance SutShow SutPosn where
  showSut (SutPosn _ line col) = SutLogLeave $ "Line " ++ show line ++ ": Column " ++ show col
