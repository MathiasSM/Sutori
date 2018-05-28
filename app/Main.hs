module Main where

import Lexer
import Parser
import AST
import System.Environment
import Control.Monad
import OurMonad
import Data.Maybe
import Data.Either

main = do
  args <- getArgs
  s <- readFile $ (args !! 1)
  let sr = runAlexScan s
  when (isLeft sr) $ error $ extractL sr
  let ws =  filter isInvalid (extractR sr)
      ls = (extractR sr)
  when (length ws /= 0) $ do putStrLn "Error Lexicografico, Alex isn't Happy:(\nTokens Invalidos:"
                             mapM_ print ws
  case (head args) of
      "-l" -> mapM_ (putStrLn. printToken) ls
      "-p" -> do let (ast,_,_) = getTuple (calc ls) emptyState
                 printSource 0 ast
      otherwise -> do let tuple = getTuple (calc ls) emptyState
                      case tuple of
                        (ast,sym,OurLog "") -> do putStrLn "AST: "
                                                  printSource 0 ast
                                                  putStrLn "SYMTABLE: "
                                                  printSymTable sym
                                                  putStrLn "0 Errors."
                        otherwise -> printLog tuple
  where printLog (_,_,log) = print $ log
        extractL (Left e) = e
        extractR (Right l) = l
