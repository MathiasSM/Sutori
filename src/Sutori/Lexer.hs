{-|
Description : API for the alex-generated lexer for Sutori language tokens
-}
module Sutori.Lexer
( lexerScanClean -- We only export the clean version (has lexical error handling)
, runLexer       -- Runs the lexer with the given action (parsing?)
, runLexer'
, runLexerScan   -- Runs the lexer by itself (with the usual scan loop)
, lexwrap        -- Wrapper around scan used by happy
, SutToken(..)
, initialPosn
, SutPosn
) where

import Sutori.Lexer.Lexer
import Sutori.Lexer.Posn
import Sutori.Lexer.Tokens
import Sutori.Lexer.Logger
