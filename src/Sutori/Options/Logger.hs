module Sutori.Options.Logger
() where

import Sutori.Options
import Sutori.Logger  (SutShow(showSut), SutLog(SutLogLeave, SutLogNode))

instance SutShow Options where
  showSut Options
    { optVerbose = verbose
    , optShowVersion = version
    , optShowHelp = help
    , optOutput = output
    , optInput = input
    , optStopOnLexer = lexer
    , optStopOnParser = parser
    } = let showVerbose = SutLogLeave $ "Verbose:       " ++ show verbose
            showVersion = SutLogLeave $ "Show Version:  " ++ show version
            showHelp    = SutLogLeave $ "Show Help:     " ++ show help
            showOutput  = SutLogLeave $ "Output:        " ++ show output
            showInput   = SutLogLeave $ "Input:         " ++ show input
            showLexer   = SutLogLeave $ "Only lexer:    " ++ show lexer
            showParser  = SutLogLeave $ "Only frontend: " ++ show parser
            showOptions = [showVerbose, showHelp, showOutput, showInput, showLexer, showParser]
         in SutLogNode "Compiler Options:" showOptions
