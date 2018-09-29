module Main where

import System.Environment
import Control.Monad
import Data.Either

import Sutori.Options
import Sutori.Options.Logger
import Sutori.Logger
-- import Sutori.Lexer
-- import Sutori.Parser
-- import Sutori.AST
-- import Sutori.Monad



-- Sutori runner
main = do
  argv             <- getArgs
  (options, files) <- handleFlags argv
  print (showSut options)
  --
  -- let sr = runAlexScan s
  -- when (isLeft sr) $ error $ extractL sr
  -- let ws =  filter isInvalid (extractR sr)
  --     ls = extractR sr
  -- when (not null ws) $ do putStrLn "Error Lexicografico, Alex isn't Happy:(\nTokens Invalidos:"
  --                         mapM_ print ws
  -- case head args of
  --     "-l" -> mapM_ (putStrLn. printToken) ls
  --     "-p" -> do let (ast,_,_) = getTuple (calc ls) emptyState
  --                printSource 0 ast
  --     _ -> do let tuple = getTuple (calc ls) emptyState
  --             case tuple of
  --               (ast,sym,OurLog "") -> do putStrLn "AST: "
  --                                         printSource 0 ast
  --                                         putStrLn "SYMTABLE: "
  --                                         printSymTable sym
  --                                         putStrLn "0 Errors."
  --               _ -> printLog tuple
  -- where printLog (_,_,log) = print log
  --       extractL (Left e) = e
  --       extractR (Right l) = l
