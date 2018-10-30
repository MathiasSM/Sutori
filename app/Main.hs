module Main where

import System.Environment (getArgs)
import Sutori.CLI         (route)
import Sutori.Options     (handleFlags)

-- Sutori runner
main :: IO ()
main = getArgs >>= handleFlags >>= route
