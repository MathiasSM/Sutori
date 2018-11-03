{-|
Description : Provides 'ShowSut' instances for "Sutori.Options"
-}
module Sutori.Options.Logger() where

import Sutori.Logger  (SutShow(showSut), SutLog(SutLogLeave, SutLogNode))

import Sutori.Options.Options

-- |The execution options can be printed nicely
instance SutShow Options where
  showSut Options
    { optVerbose     = verbose
    , optShowVersion = version
    , optShowHelp    = help
    , optOutput      = output
    , optStopOnLexer = lexer
    , optStopOnAST   = ast
    , optStopOnTAC   = tac
    } = let showVerbose = SutLogLeave $ "Verbose:       " ++ show verbose
            showVersion = SutLogLeave $ "Show Version:  " ++ show version
            showHelp    = SutLogLeave $ "Show Help:     " ++ show help
            showOutput  = SutLogLeave $ "Output:        " ++ show output
            showLexer   = SutLogLeave $ "Only lexer:    " ++ show lexer
            showAST     = SutLogLeave $ "Only frontend: " ++ show ast
            showTAC     = SutLogLeave $ "Only intermediate code: " ++ show tac
            showOptions = [showVerbose, showHelp, showOutput, showLexer, showAST, showTAC]
         in SutLogNode "Compiler options:" showOptions
