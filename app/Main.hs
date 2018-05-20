module Main where

import System.Environment
import Control.Monad
import Data.Maybe

import Lexer
import Parser
import AST

main = do
  s <- getContents
  let sr = runAlexScan s
  case sr of
    Left st  -> error st
    Right ls -> case filter isInvalid ls of
                  [] -> printSource 0 $ calc ls
                  _  -> do putStrLn "Error Lexicografico, Alex isn't Happy:(\nTokens Invalidos:"
                           mapM_ print (filter isInvalid ls)
