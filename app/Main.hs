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
    Right ls -> mapM_ (putStrLn. printToken) ls
