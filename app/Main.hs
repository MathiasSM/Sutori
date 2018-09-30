module Main where

import System.Environment (getArgs)
import Sutori.Options     (handleFlags)
import Sutori.Router      (route)

-- Sutori runner
main = getArgs >>= handleFlags >>= route
