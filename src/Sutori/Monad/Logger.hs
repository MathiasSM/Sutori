module Sutori.Monad.Logger
( SutError(..)
, logError
) where

import Control.Monad.State(get)

import Sutori.Logger(SutShow(showSut), SutLog(SutLogLeave, SutLogNode))
import Sutori.Lexer.Logger

import Sutori.Monad(SutMonad, SutState(SutState, lexerPosn, lexerChar))


data SutError = LexicalError | GrammaticalError | TypeError | InternalError deriving Show

instance SutShow SutError where
  showSut = SutLogLeave . show

logError :: SutError -> SutMonad SutLog
logError errType = do
  SutState{ lexerPosn = posn, lexerChar = char } <- get
  let errorTypeLog = showSut errType
      information = SutLogNode "Info" [showSut posn, SutLogLeave $ "Character: " ++ [char]]
  return $ SutLogNode "Error" [errorTypeLog, information]
