{-|
  Description : The compilation monad
-}
module Sutori.Monad.Monad
( SutMonad
, runSutMonad
) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Sutori.Error.Error (SutError)
import Sutori.Logger      (SutLogger, SutLog)
import Sutori.Options     (Options)

import Sutori.Monad.State


-- |The Sutori monad. Composes state, logging and exception handling
type SutMonad a = ReaderT Options (StateT SutState (WriterT SutLogger (Except (SutError, SutLog)))) a

-- |Run the monad with a given action
runSutMonad :: SutMonad a -> Options -> SutState -> Except (SutError, SutLog) ((a, SutState), SutLogger)
runSutMonad f opt s = runWriterT (runStateT (runReaderT f opt) s)
