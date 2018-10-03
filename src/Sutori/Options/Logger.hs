{-|
Description : Provides 'ShowSut' instances for "Sutori.Options"
-}
module Sutori.Options.Logger() where

import Sutori.Options
import Sutori.Logger  (SutShow(showSut), SutLog(SutLogLeave, SutLogNode))

-- |The execution options can be printed nicely
instance SutShow Options where
  showSut Options
    { optVerbose = verbose
    , optShowVersion = version
    , optShowHelp = help
    , optOutput = output
    , optStopOnLexer = lexer
    , optStopOnParser = parser
    } = let showVerbose = SutLogLeave $ "Verbose:       " ++ show verbose
            showVersion = SutLogLeave $ "Show Version:  " ++ show version
            showHelp    = SutLogLeave $ "Show Help:     " ++ show help
            showOutput  = SutLogLeave $ "Output:        " ++ show output
            showLexer   = SutLogLeave $ "Only lexer:    " ++ show lexer
            showParser  = SutLogLeave $ "Only frontend: " ++ show parser
            showOptions = [showVerbose, showHelp, showOutput, showLexer, showParser]
         in SutLogNode "Compiler options:" showOptions
