module Sutori.Monad.Logger
( SutError(..)
, logError
) where

import Control.Monad.State(get)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

import Sutori.Logger(SutShow(showSut), SutLog(SutLogLeave, SutLogNode), fromLeave)
import Sutori.Lexer.Logger

import Sutori.Monad(SutMonad, SutState(SutState, lexerPosn, lexerChar, lexerInput))


data SutError = LexicalError | GrammaticalError | TypeError | InternalError deriving Show

instance SutShow SutError where
  showSut = SutLogLeave . show

logError :: SutError -> String -> SutMonad a
logError LexicalError = lexerError
  -- SutState{ lexerPosn = posn, lexerChar = char } <- get
  -- let errorTypeLog = showSut errType
  --     information = SutLogNode "Info" [showSut posn, SutLogLeave $ "Character: " ++ [char]]
  -- return $ SutLogNode "Error" [errorTypeLog, information]



-- Handling errors
-- ================================================================================================
lexerError :: String -> SutMonad a
lexerError msg = do
    SutState{ lexerPosn = posn, lexerChar = char, lexerInput = input } <- get
    let cleanInput = clean input
        errorTitle = "Lexical Error: " ++  msg
        -- log     = SutLogNode errorTitle [logPosn, logChar]
        -- logPosn = SutLogLeave $ "On position: " ++ (fromLeave (showSut posn))
        -- logChar = SutLogLeave $ "On char: " ++ char
        -- logBefore
    error $ errorTitle ++ " at " ++ fromLeave (showSut posn) ++ placeError input cleanInput char
  where
    clean        = shorten . removeBreaks . trim
    trim         = dropWhileEnd isSpace . dropWhile isSpace
    removeBreaks = filter (/= '\r') . takeWhile (/= '\n')
    maxSize   = 20
    shorten s    = trim (take maxSize s) ++ if length s > maxSize then "..." else ""
    placeError s1 s2 c
      | null s1   = " at end of file"
      | null s2   = " before end of line"
      | otherwise = " on char " ++ show c ++ " before : '" ++ s2 ++ "'"
