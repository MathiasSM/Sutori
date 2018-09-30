module Sutori.Monad.Logger
( SutError(..)
, logError
, lexerError
, parserError
) where

import Control.Monad             (when)
import Control.Monad.State.Lazy  (get)
import Control.Monad.Writer.Lazy (tell)
import Control.Monad.Except

import Data.List (dropWhileEnd)
import Data.Char (isSpace)

import Sutori.Logger(SutShow(showSut), SutLog(..), SutLogger(..), SutError(..), fromLeave)
import Sutori.Lexer.Logger
import Sutori.Lexer.Tokens (SutToken)

import Sutori.Types.Constructors (SutTypeID)
import Sutori.Monad(SutMonad, SutState(SutState, lexerPosn, lexerChar, lexerInput, logVerbose), setErrorCode)

instance SutShow SutError where
  showSut = SutLogLeave . show



-- Handling errors
-- ================================================================================================

-- Information gathering
-- ------------------------------------------------------------------------------------------------
errorPos :: SutMonad SutLog
errorPos = do
  SutState{ lexerPosn = posn, lexerChar = char, lexerInput = input } <- get
  return $ SutLogLeave $ "On position: " ++ fromLeave (showSut posn)

errorChar :: SutMonad SutLog
errorChar = do
  SutState{ lexerPosn = posn, lexerChar = char, lexerInput = input } <- get
  return $ SutLogLeave $ "On char: " ++ [char]

errorExpr :: SutMonad SutLog
errorExpr = return $ SutLogLeave $ "On expression: " ++ "<TODO: Get current expression>"


-- API: The different possible errora
-- ------------------------------------------------------------------------------------------------
lexerError :: String -> SutMonad ()
lexerError msg = do
  pos  <- errorPos
  char <- errorChar
  let code     = LexicalError
      logTitle = "Lexical Error: " ++  msg
      log      = SutLogNode logTitle [pos, char]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode LexicalError

parserError :: SutToken -> SutMonad a
parserError tk = do
  pos <- errorPos
  let code     = GrammaticalError
      logTitle = "Grammatical Error"
      log      = SutLogNode logTitle [pos]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode code
  throwError errMsg

typeError :: SutTypeID -> SutMonad ()
typeError tid = do
  let code     = TypeError
  pos  <- errorPos
  expr <- errorExpr
  let logTitle = "Type Error: Expected " ++ show tid ++ "<TODO: find expected>"
      log      = SutLogNode logTitle [pos, expr]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode code
