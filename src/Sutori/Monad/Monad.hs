{-|
  Description : The compilation monad
-}
module Sutori.Monad.Monad
( SutMonad
, runSutMonad
) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Sutori.Error.Error (SutError)
import Sutori.Logger      (SutLogger, SutLog)

import Sutori.Monad.State


-- |The Sutori monad. Composes state, logging and exception handling
type SutMonad a = StateT SutState (WriterT SutLogger (Except (SutError, SutLog))) a

-- |Run the monad with a given action
runSutMonad :: SutMonad a -> SutState -> Except (SutError, SutLog) ((a, SutState), SutLogger)
runSutMonad f a = runWriterT $ runStateT f a
