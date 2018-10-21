module Main where

import System.Environment (getArgs)
import Sutori.CLI         (route)
import Sutori.Options     (handleFlags)

-- Sutori runner
main = getArgs >>= handleFlags >>= route
