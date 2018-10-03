{-|
Description : Provides 'ShowSut' instances for "Sutori.Monad".
              Right now it also provides error handling functions
-}
module Sutori.Monad.Logger
( SutError(..)
, logError
, lexerError
, parserError
, typeError
, undefinedError
, duplicateSymbolError
, argumentsNumberError
) where

import Control.Monad             (when)
import Control.Monad.State.Lazy  (get)
import Control.Monad.Writer.Lazy (tell)
import Control.Monad.Except

import Data.List                 (dropWhileEnd)
import Data.Char                 (isSpace)

import Sutori.AST                (SutExpression)
import Sutori.AST.Logger
import Sutori.Lexer.Logger
import Sutori.Lexer.Tokens       (SutToken)
import Sutori.Logger             (SutShow(showSut), SutLog(..), SutLogger(..), SutError(..), fromLeave)
import Sutori.Monad              (SutMonad, SutState(SutState, lexerPosn, lexerChar, lexerInput, logVerbose), setErrorCode)
import Sutori.SymTable           (SutSymCategory)
import Sutori.SymTable.Logger
import Sutori.Types.Constructors (SutType)
import Sutori.Utils              (SutID)

-- |A 'SutError' can be printed nicely
instance SutShow SutError where
  showSut = SutLogLeave . show



-- Handling errors
-- ================================================================================================

-- Information gathering
-- ------------------------------------------------------------------------------------------------
-- |Gets the current position and formats it as an error log
errorPos :: SutMonad SutLog
errorPos = do
  SutState{ lexerPosn = posn, lexerChar = char, lexerInput = input } <- get
  return $ SutLogLeave $ "On position: " ++ fromLeave (showSut posn)

-- |Gets the current character from the state and formats it as an error log
errorChar :: SutMonad SutLog
errorChar = do
  SutState{ lexerPosn = posn, lexerChar = char, lexerInput = input } <- get
  return $ SutLogLeave $ "On char: " ++ [char]


-- API: The different possible errors
-- ------------------------------------------------------------------------------------------------
-- |Logs a lexical error and continues
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

-- |Logs a parsing error and throws out
parserError :: SutToken -> SutMonad a
parserError tk = do
  pos <- errorPos
  let code     = GrammaticalError
      logTitle = "Grammatical Error"
      log      = SutLogNode logTitle [showSut tk, pos]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode code
  throwError errMsg

-- |Logs a type error and continues
typeError :: SutExpression -> SutType -> SutType -> String -> SutMonad ()
typeError e expected actual msg = do
  let code     = TypeError
  pos  <- errorPos
  let logTitle    = "Type Error: " ++ msg
      logExpected = SutLogLeave $ "Expected type: " ++ fromLeave (showSut expected)
      logActual   = SutLogLeave $ "Actual type:   " ++ fromLeave (showSut actual)
      log      = SutLogNode logTitle [pos, showSut e, logExpected, logActual]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode code

-- |Logs an undefined symbol error and continues
undefinedError :: SutID -> SutSymCategory -> String -> SutMonad ()
undefinedError id cat msg = do
  let code     = UndefinedSymbolError
  pos  <- errorPos
  let logTitle = "Undefined symbol '" ++ id ++ "': " ++ msg
      log      = SutLogNode logTitle [pos, showSut cat]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode code

-- |Logs a wrong number of parameters error and continues
argumentsNumberError :: SutID -> Int -> Int -> SutMonad ()
argumentsNumberError id expected actual = do
  let code     = ArgumentsNumberError
  pos  <- errorPos
  let logTitle    = "Wrong number of arguments in call to '" ++ id ++"'"
      logExpected = SutLogLeave $ "Number of formal parameters: " ++ show expected
      logActual   = SutLogLeave $ "Number of actual arguments:  " ++ show actual
      log      = SutLogNode logTitle [pos, logExpected, logActual]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode code

-- |Logs a duplicate symbol error and continues
duplicateSymbolError :: SutID -> SutSymCategory -> String -> SutMonad ()
duplicateSymbolError id cat msg = do
  let code     = DuplicateSymbolError
  pos  <- errorPos
  let logTitle    = "Duplicate definition for  '" ++ id ++"': " ++ msg
      log      = SutLogNode logTitle [pos, showSut cat]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode code
