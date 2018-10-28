{-|
Description : Provides error-reporting functions
-}
module Sutori.Error.Report
( logError
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

import Sutori.AST           (SutID, SutExpression)
import Sutori.Lexer.Tokens  (SutToken)
import Sutori.Lexer.Logger  ()
import Sutori.SymTable      (SymbolCat)
import Sutori.Types         (SutType)
import Sutori.Logger        (SutShow(showSut), SutLog(..), SutLogger(..), fromLeave)
import Sutori.Monad         (SutState(SutState, lexerPosn, lexerChar, lexerInput, logVerbose), SutMonad, setErrorCode)

import Sutori.Error.Error   (SutError(..))


-- |Gets the current position and formats it as an error log
errorPos :: SutMonad String
errorPos = do
  SutState{ lexerPosn = posn, lexerChar = char, lexerInput = input } <- get
  return $ fromLeave (showSut posn)

-- |Gets the current character from the state and formats it as an error log
errorChar :: SutMonad SutLog
errorChar = do
  SutState{ lexerPosn = posn, lexerChar = char, lexerInput = input } <- get
  return $ SutLogLeave $ "On char: " ++ [char]


-- ------------------------------------------------------------------------------------------------
-- |Logs a lexical error and continues
lexerError :: String -> SutMonad ()
lexerError msg = do
  pos  <- errorPos
  char <- errorChar
  let code     = LexicalError
      errType  = SutLogLeave $ "Lexical Error: " ++  msg
      log      = SutLogNode pos [errType, char]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode LexicalError

-- |Logs a parsing error and throws out
parserError :: SutToken -> SutMonad a
parserError tk = do
  pos <- errorPos
  let code     = GrammaticalError
      errType  = SutLogLeave "Parse Error"
      log      = SutLogNode pos [errType, showSut tk]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode code
  throwError errMsg

-- |Logs a type error and continues
typeError :: SutExpression -> SutType -> SutType -> String -> SutMonad ()
typeError e expected actual msg = do
  let code     = TypeError
  pos  <- errorPos
  let errType     = SutLogLeave $ "Type Error: " ++ msg
      logExpected = SutLogLeave $ "Expected type: " ++ fromLeave (showSut expected)
      logActual   = SutLogLeave $ "Actual type:   " ++ fromLeave (showSut actual)
      log      = SutLogNode pos [errType, showSut e, logExpected, logActual]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode code

-- |Logs an undefined symbol error and continues
undefinedError :: SutID -> SymbolCat -> String -> SutMonad ()
undefinedError id cat msg = do
  let code     = UndefinedSymbolError
  pos  <- errorPos
  let errType  = SutLogLeave $ "Undefined symbol '" ++ id ++ "': " ++ msg
      log      = SutLogNode pos [errType, showSut cat]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode code

-- |Logs a wrong number of parameters error and continues
argumentsNumberError :: SutID -> Int -> Int -> SutMonad ()
argumentsNumberError id expected actual = do
  let code     = ArgumentsNumberError
  pos  <- errorPos
  let errType     = SutLogLeave $ "Wrong number of arguments in call to '" ++ id ++"'"
      logExpected = SutLogLeave $ "Number of formal parameters: " ++ show expected
      logActual   = SutLogLeave $ "Number of actual arguments:  " ++ show actual
      log      = SutLogNode pos [errType, logExpected, logActual]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode code

-- |Logs a duplicate symbol error and continues
duplicateSymbolError :: SutID -> SymbolCat -> String -> SutMonad ()
duplicateSymbolError id cat msg = do
  let code     = DuplicateSymbolError
  pos  <- errorPos
  let errType  = SutLogLeave $ "Duplicate definition for  '" ++ id ++"': " ++ msg
      log      = SutLogNode pos [errType, showSut cat]
      errMsg   = (code, log)
  tell mempty{logError = [errMsg]}
  setErrorCode code
