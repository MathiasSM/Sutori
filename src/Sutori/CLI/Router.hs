{-|
Description : Defines a 'route' function to decide for an execution mode of the compiler
-}
module Sutori.CLI.Router
( route
) where

import Control.Monad         (when, unless)
import Control.Arrow         (second)
import Data.Version          (showVersion)
import Paths_sutori          (version)
import System.Exit           (exitSuccess, die)
import System.IO             (stderr, stdout, hPrint)

import Sutori.Error          (SutError)
import Sutori.Lexer          (runLexerScan, runLexer, lexerLoop)
import Sutori.Logger         (SutLogger(..), SutLog(..), SutShow(showSut))
import Sutori.Monad          (SutMonad, SutState(SutState, mainModule, typesGraph, parserTable, tacTable))
import Sutori.Options        (Options(..), usage)
import Sutori.Parser         (parseModule)
import Sutori.SymTable       (lookupAllFunctions)
import Sutori.TAC            ()

-- | Routes a call to the CLI with passed options and files into the correct mode of operation
route :: (Options, [FilePath]) -> IO ()
route (opt@Options{optDebug = True}, fs) = showDebugInfo opt fs
route (opt, fs)                          = route' (opt, fs)
 where route' :: (Options, [FilePath]) -> IO ()
       route' (opt@Options{optShowHelp = True},    _)  = showHelp opt
       route' (opt@Options{optShowVersion = True}, _)  = showSutoriVersion opt
       route' (opt@Options{optStopOnLexer = True}, fs) = runLexerOnly opt fs
       route' (opt@Options{optStopOnAST = True}, fs)   = runASTOnly opt fs
       route' (opt@Options{optStopOnTAC = True}, fs)   = runTACOnly opt fs
       route' (opt,                              fs)   = runAll opt fs -- Default: Run everything!


-- Routes (mode of operation "selected" according to the passed flags)
-- ================================================================================================

-- |Shows debugging information (This is a "prefix" route: Others might follow)
showDebugInfo :: Options -> [FilePath] -> IO ()
showDebugInfo opts fs = do
  print (showSut opts)
  putStrLn "Files:"
  mapM_ (putStrLn . ("  "++)) fs
  putStrLn ""


-- |Shows usage information and exits
showHelp :: Options -> IO ()
showHelp _ = do
  putStrLn usage
  exitSuccess

-- |Shows the current sutori version and exits
showSutoriVersion :: Options -> IO ()
showSutoriVersion _ = do
  putStrLn $ showVersion version
  exitSuccess

runOnFile :: SutMonad a -> Options -> [FilePath] -> IO (Either (SutError, SutLog) ((a, SutState), SutLogger))
runOnFile f opt@Options{ optOutput = output' } inputFiles = do
  input <- readInput inputFiles
  return $ runLexer opt input f


-- |Runs the lexer and outputs the list of read tokens
--
-- TODO: Check state for error code and fail
runLexerOnly :: Options -> [FilePath] -> IO ()
runLexerOnly opt input = runOnFile lexerLoop opt input >>= \result ->
  reportResult printTokens result
    where printTokens ((tks, _), SutLogger{logInfo = info, logError = err}) = do
            mapM_ (print.showSut) tks
            printInfos info
            printErrors err

-- |Runs the parser/lexer monad without code generation, generates AST
--
-- Checks types, symbols consistency, scopes, etc. Can print AST on verbose.
--
-- TODO: Check state for error code and fail
runASTOnly :: Options -> [FilePath] -> IO ()
runASTOnly opt@Options{ optVerbose = v } input = runOnFile parseModule opt input >>= \result ->
  reportResult f result
    where f ((e, s), SutLogger{logInfo = infos, logError = errs}) = do
            when v $ printInfo (showSut (typesGraph s))
            when v $ printTitledInfo "Main AST" (showSut $ mainModule s)
            when v $ printTitledInfos "Functions" (map showSut $ lookupAllFunctions 0 $ parserTable s)
            unless (null errs) $ printTitledErrors "There were errors while processing the input" errs

-- |Runs up to code generation
--
-- This won't generate an AST, even on verbose output.
--
-- TODO: Check state for error code and fail
runTACOnly :: Options -> [FilePath] -> IO ()
runTACOnly opt@Options{ optVerbose = v } input = runOnFile parseModule opt input >>= \result ->
  reportResult f result
    where f ((e, s), SutLogger{logInfo = infos, logError = errs}) = do
            when v $ printTitledInfo "Intermediate Code TAC" (showSut $ tacTable s)
            unless (null errs) $ printTitledErrors "There were errors while processing the input" errs

-- |Runs the whole compiler to generate machine code
--
-- TODO: Check state for error code and fail
runAll :: Options -> [FilePath] -> IO ()
runAll opt@Options{ optVerbose = v } input = runOnFile parseModule opt input >>= \result ->
  reportResult f result
    where f ((e, s), SutLogger{logInfo = infos, logError = errs}) = do
            when v $ printInfo (showSut $ tacTable s)
            unless (null errs) $ printTitledErrors "There were errors while processing the input" errs



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
