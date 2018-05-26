module Main where

import Lexer
import Parser
import AST
import System.Environment
import Control.Monad
import OurMonad
import Data.Maybe

main = do
  s <- getContents
  let sr = runAlexScan s
  case sr of
    Left st  -> error st

    Right ls -> case filter isInvalid ls of
                  [] ->  print $ printLog $ getTuple (calc ls) emptyState
                  _  -> do putStrLn "Error Lexicografico, Alex isn't Happy:(\nTokens Invalidos:"
                           mapM_ print (filter isInvalid ls)
  where printLog (_,_,log) = log 
