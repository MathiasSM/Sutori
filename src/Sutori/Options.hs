module Sutori.Options(Options(..), handleFlags) where

import Data.Maybe
import System.Console.GetOpt


-- Options for command-line parsing
data Options = Options
  { optVerbose      :: Bool
  , optShowVersion  :: Bool
  , optShowHelp     :: Bool
  , optOutput       :: Maybe FilePath
  , optInput        :: Maybe FilePath
  , optStopOnLexer  :: Bool
  , optStopOnParser :: Bool
  } deriving Show

defaultOptions = Options
  { optVerbose      = False
  , optShowVersion  = False
  , optShowHelp     = False
  , optOutput       = Nothing
  , optInput        = Nothing
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
  , Option ['i'] ["input"]
      (OptArg ((\f opts -> opts { optInput = Just f }) . fromMaybe "input") "FILE")
      "input FILE"
  ]



-- Parse command-line flags
handleFlags :: [String] -> IO (Options, [String])
handleFlags argv = case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: sutori [OPTION...] files..."
