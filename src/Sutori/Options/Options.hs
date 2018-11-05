{-|
Description : Defines the different CLI options and a parser function to get them
-}
module Sutori.Options.Options(Options(..), defaultOptions, handleFlags, usage) where

import Data.Maybe
import System.Console.GetOpt


-- |Options for command-line parsing
data Options = Options
  { optVerbose      :: Bool            -- ^ Verbose switch
  , optDebug        :: Bool            -- ^ Debugging switch
  , optOutput       :: Maybe FilePath  -- ^ Output file
  , optShowVersion  :: Bool            -- ^ Mode: Show version
  , optShowHelp     :: Bool            -- ^ Mode: Show help
  , optStopOnLexer  :: Bool            -- ^ Mode: Run only lexer
  , optStopOnAST    :: Bool            -- ^ Mode: Run only frontend
  , optStopOnTAC    :: Bool            -- ^ Mode: Run only until intermediate code generation
  }

-- |The default options for a Sutori run
defaultOptions :: Options
defaultOptions = Options
  { optVerbose      = False
  , optDebug        = False
  , optOutput       = Nothing
  , optShowVersion  = False
  , optShowHelp     = False
  , optStopOnLexer  = False
  , optStopOnAST    = False
  , optStopOnTAC    = False
  }

-- |Description of the different command-line options
options :: [OptDescr (Options -> Options)]
options =
  [
  -- Flags
    Option ['v'] ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True }))
      "chatty output"
  , Option ['d'] ["debug", "debugging"]
      (NoArg (\opts -> opts { optDebug = True }))
      "chatty output"
  , Option ['o'] ["output"]
      (OptArg ((\f opts -> opts { optOutput = Just f }) . fromMaybe "output") "FILE")
      "output FILE"

  -- Modes
  , Option ['V'] ["version"]
      (NoArg (\opts -> opts { optShowVersion = True }))
      "show version number"
  , Option ['h', '?'] ["help"]
      (NoArg (\opts -> opts { optShowHelp = True }))
      "show help"

  , Option [] ["lexer", "tokens"]
      (NoArg (\opts -> opts { optStopOnLexer = True }))
      "stop after running lexer (prints Token list)"
  , Option [] ["frontend", "ast", "typecheck"]
      (NoArg (\opts -> opts { optStopOnAST = True }))
      "stop after generating AST, Symbols and Types"
  , Option [] ["tac"]
      (NoArg (\opts -> opts { optStopOnTAC = True }))
      "stop after generating intermediate TAC (skips AST)"
  ]

-- |Parse command-line flags
handleFlags :: [String] -> IO (Options, [String])
handleFlags argv = case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usage))

-- |Complete usage information
usage :: String
usage = usageInfo "Usage: sutori [OPTION...] files..." options
