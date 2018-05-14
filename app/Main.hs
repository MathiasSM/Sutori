module Main where

import Lexer
import Parser
import System.Environment
import Control.Monad
import Data.Maybe

main = do
  s <- getContents
  let sr = runAlexScan s
  case sr of
    Left st  -> error st
    Right ls -> case filter isInvalid ls of
                    [] -> mapM_ (putStrLn. printToken) ls
                    _  -> do putStrLn "Error Lexicografico, Alex isn't Happy:(\nTokens Invalidos:"
                             mapM_ (putStrLn. printToken) (filter isInvalid ls)
