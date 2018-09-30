module Sutori.Router
( route
) where

import Paths_sutori          (version)
import Data.Version          (showVersion)

import Control.Monad.Except  ()

import System.Exit           (exitSuccess, die)

import Sutori.Options        (Options(..), usage)
import Sutori.Options.Logger ()
import Sutori.Lexer          (runLexerScan, runLexer)
import Sutori.Logger         (SutLogger(..), SutLog, SutError, SutShow(showSut))
import Sutori.Parser         (sutoriParser)


route :: (Options, [FilePath]) -> IO ()
route (opt@Options{optDebugging = True}, fs) = showDebuggingInfo opt fs
route (opt, fs)                              = route' (opt, fs)

route' :: (Options, [FilePath]) -> IO ()
route' (opt@Options{optShowHelp = True},     _ ) = showHelp
route' (opt@Options{optShowVersion = True},  _ ) = showSutoriVersion opt
route' (opt@Options{optStopOnLexer = True},  fs) = runLexerOnly opt fs
route' (opt@Options{optStopOnParser = True}, fs) = runFrontendOnly opt fs
route' (opt,                                 fs) = runFrontendOnly opt fs -- Default: Run everything!



-- Routes ("selected" according to the passed flags)
-- ================================================================================================

-- Shows debugging information (This is a "prefix" route: Others might follow)
showDebuggingInfo :: Options -> [FilePath] -> IO ()
showDebuggingInfo opts fs = do
  print (showSut opts)

  putStrLn "Files:"
  mapM_ (putStrLn . ("  "++)) fs
  putStrLn ""


-- Shows usage information and exits
showHelp :: IO ()
showHelp = do
  putStrLn usage
  exitSuccess

-- Shows the current sutori version and exits
showSutoriVersion :: Options -> IO ()
showSutoriVersion _ = do
  putStrLn $ showVersion version
  exitSuccess

-- Runs the lexer and outputs the list of read tokens
runLexerOnly :: Options -> [FilePath] -> IO ()
runLexerOnly opt@Options{ optOutput = output' } inputFiles = do
  input <- readInput inputFiles
  let result = runLexerScan opt input

  reportResult printTokens result
    where printTokens (tks, SutLogger{logInfo = info, logError = err}) = do
            mapM_ (print.showSut) tks
            mapM_ print info
            mapM_ printError err

-- Runs the parser/lexer monad without code generation
runFrontendOnly :: Options -> [FilePath] -> IO ()
runFrontendOnly opt@Options{ optOutput = output' } inputFiles = do
  input <- readInput inputFiles
  let result = runLexer opt input sutoriParser

  reportResult printTokens result
    where printTokens (e, SutLogger{logInfo = info, logError = err}) = do
            print e
            mapM_ print info
            mapM_ printError err



-- Utilities
-- ================================================================================================
reportResult :: (a -> IO()) -> Either (SutError, SutLog) a -> IO()
reportResult f (Left e)  = printError e
reportResult f (Right r) = f r

readInput :: [FilePath] -> IO String
readInput []    = readFile "/dev/stdin"
readInput (f:_) = readFile f

printError :: (SutError, SutLog) -> IO ()
printError (code, log) = print log
