module Sutori.Lexer.Logger() where

import Sutori.Logger(SutShow(..), SutLog(SutLogNode, SutLogLeave))
import Sutori.Lexer.Tokens(SutToken(..))
import Sutori.Lexer.Posn(SutPosn(SutPosn))

instance SutShow SutToken where
  showSut SutTkEOF = SutLogLeave "Token EOF"
  showSut tk       = SutLogLeave $ "Token: " ++ show tk

instance SutShow SutPosn where
  showSut (SutPosn _ line col) = SutLogLeave $ show line ++ ":" ++ show col
