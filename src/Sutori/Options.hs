module Sutori.Options(Options(..), handleFlags, usage) where

import Data.Maybe
import System.Console.GetOpt


-- Options for command-line parsing
data Options = Options
  { optVerbose      :: Bool
  , optDebugging    :: Bool
  , optShowVersion  :: Bool
  , optShowHelp     :: Bool
  , optOutput       :: Maybe FilePath
  , optStopOnLexer  :: Bool
  , optStopOnParser :: Bool
  }

defaultOptions = Options
  { optVerbose      = False
  , optDebugging    = False
  , optShowVersion  = False
  , optShowHelp     = False
  , optOutput       = Nothing
  , optStopOnLexer  = False
  , optStopOnParser = False
  }

-- Description of the different command-line options
options :: [OptDescr (Options -> Options)]
options =
  [
  -- No arguments
    Option  ['v'] ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True }))
      "chatty output"
  , Option  ['d'] ["debug", "debugging"]
      (NoArg (\opts -> opts { optDebugging = True }))
      "chatty output"

  , Option ['V'] ["version"]
      (NoArg (\opts -> opts { optShowVersion = True }))
      "show version number"
  , Option ['h', '?'] ["help"]
      (NoArg (\opts -> opts { optShowHelp = True }))
      "show help"

  , Option ['l'] ["lexer", "tokens"]
      (NoArg (\opts -> opts { optStopOnLexer = True }))
      "stop after running lexer (prints Token list)"
  , Option ['p'] ["parser", "ast"]
      (NoArg (\opts -> opts { optStopOnParser = True }))
      "stop after running parser (prints AST, Symbols and Types)"

  -- Optional argument
  , Option ['o'] ["output"]
      (OptArg ((\f opts -> opts { optOutput = Just f }) . fromMaybe "output") "FILE")
      "output FILE"
  ]



-- Parse command-line flags
handleFlags :: [String] -> IO (Options, [String])
handleFlags argv = case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usage))

usage = usageInfo "Usage: sutori [OPTION...] files..." options
