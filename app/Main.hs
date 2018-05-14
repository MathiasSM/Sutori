module Main where

import Lexer
import System.Environment
import Control.Monad
import Data.Maybe

main = do
  s <- getContents
  let sr = runAlexScan s
  case sr of
    Left st  -> error st

    Right ls -> case filter isInvalid ls of
                  [] -> do mapM_ print ls
                           calc ls
                  _  -> do putStrLn "Error Lexicografico, Alex isn't Happy:(\nTokens Invalidos:"
                           mapM_ print (filter isInvalid ls)
