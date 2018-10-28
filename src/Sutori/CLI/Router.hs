{-|
Description : Defines a 'route' function to decide for an execution mode of the compiler
-}
module Sutori.CLI.Router
( route
) where

import Control.Monad         (when)
import Control.Arrow         (second)
import Data.Version          (showVersion)
import Paths_sutori          (version)
import System.Exit           (exitSuccess, die)
import System.IO             (stderr, stdout, hPrint)

import Sutori.Error          (SutError)
import Sutori.Lexer          (runLexerScan, runLexer)
import Sutori.Logger         (SutLogger(..), SutLog(..), SutShow(showSut))
import Sutori.Monad          (SutState(SutState, mainModule, typesGraph, parserTable))
import Sutori.Options        (Options(..), usage)
import Sutori.Parser         (parseModule)
import Sutori.SymTable       (lookupAllFunctions)

-- | Routes a call to the CLI with passed options and files into the correct mode of operation
route :: (Options, [FilePath]) -> IO ()
route (opt@Options{optDebugging = True}, fs) = showDebuggingInfo opt fs
route (opt, fs)                              = route' (opt, fs)
 where route' :: (Options, [FilePath]) -> IO ()
       route' (opt@Options{optShowHelp = True},     _ ) = showHelp
       route' (opt@Options{optShowVersion = True},  _ ) = showSutoriVersion opt
       route' (opt@Options{optStopOnLexer = True},  fs) = runLexerOnly opt fs
       route' (opt@Options{optStopOnParser = True}, fs) = runFrontendOnly opt fs
       route' (opt,                                 fs) = runFrontendOnly opt fs -- Default: Run everything!


-- Routes (mode of operation "selected" according to the passed flags)
-- ================================================================================================

-- |Shows debugging information (This is a "prefix" route: Others might follow)
showDebuggingInfo :: Options -> [FilePath] -> IO ()
showDebuggingInfo opts fs = do
  print (showSut opts)
  putStrLn "Files:"
  mapM_ (putStrLn . ("  "++)) fs
  putStrLn ""


-- |Shows usage information and exits
showHelp :: IO ()
showHelp = do
  putStrLn usage
  exitSuccess

-- |Shows the current sutori version and exits
showSutoriVersion :: Options -> IO ()
showSutoriVersion _ = do
  putStrLn $ showVersion version
  exitSuccess

-- |Runs the lexer and outputs the list of read tokens
--
-- TODO: Check state for error code and fail
runLexerOnly :: Options -> [FilePath] -> IO ()
runLexerOnly opt@Options{ optOutput = output' } inputFiles = do
  input <- readInput inputFiles
  let result = runLexerScan opt input

  reportResult printTokens result
    where printTokens ((tks, _), SutLogger{logInfo = info, logError = err}) = do
            mapM_ (print.showSut) tks
            printInfos info
            printErrors err

-- |Runs the parser/lexer monad without code generation
--
-- TODO: Check state for error code and fail
runFrontendOnly :: Options -> [FilePath] -> IO ()
runFrontendOnly opt@Options{ optOutput = output', optVerbose = v } inputFiles = do
  input <- readInput inputFiles
  let result = runLexer opt input parseModule

  reportResult f result
    where f ((e, s), SutLogger{logInfo = infos, logError = errs}) = do
            when v $ printInfo (showSut (typesGraph s))
            when v $ printTitledInfo "Main AST" (showSut $ mainModule s)
            when v $ printTitledInfos "Functions" (map showSut $ lookupAllFunctions 0 $ parserTable s)
            printTitledErrors "There were errors while processing the input" errs



-- Utilities
-- ================================================================================================

-- |Outputs the result of a SutMonad run (lexer, parser of otherwise)
reportResult :: (a -> IO()) -> Either (SutError, SutLog) a -> IO()
reportResult f (Left e)  = printErrors [e]
reportResult f (Right r) = f r

-- |Reads the first input file from a list
readInput :: [FilePath] -> IO String
readInput []    = readFile "/dev/stdin"
readInput (f:_) = readFile f



-- |Prints an error log
printError :: (SutError, SutLog) -> IO ()
printError = hPrint stderr . snd

-- |Prints a series of error logs
printErrors :: [(SutError, SutLog)] -> IO ()
printErrors [] = return ()
printErrors l  = mapM_ printError l



-- |Prints an info log
printInfo :: SutLog -> IO ()
printInfo l =  print l >> putStrLn ""

-- |Print a series of info log
printInfos :: [SutLog] -> IO ()
printInfos [] = return ()
printInfos l  = mapM_ printInfo l >> putStrLn ""



-- |Prints an info log under a title
printTitledInfo :: String -> SutLog -> IO ()
printTitledInfo title log = printInfo (SutLogNode title [log])

-- |Print a series of info log
printTitledInfos :: String -> [SutLog] -> IO ()
printTitledInfos title logs = printInfo (SutLogNode title logs)

-- |Print a series of error logs, with a title
printTitledErrors :: String -> [(SutError, SutLog)] -> IO ()
printTitledErrors title errs = printError (fst $ head errs, SutLogNode title (map snd errs))
