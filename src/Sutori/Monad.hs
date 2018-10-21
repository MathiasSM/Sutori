{-|
  Description : Defines the Compiler Monad, the compilation state and general functions
-}
module Sutori.Monad
( SutMonad
, SutState(..)
, runSutMonad
, parserCurrentScope
, setErrorCode
, initialSutoriState
, insertScope
, removeScope
) where

import Sutori.Monad.State
import Sutori.Monad.Monad
import Sutori.Monad.Actions
import Sutori.Monad.Logger
