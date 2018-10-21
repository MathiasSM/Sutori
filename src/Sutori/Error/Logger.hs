{-|
Description : Provides 'ShowSut' instances for "Sutori.Errors"
-}
module Sutori.Error.Logger() where

import Sutori.Logger  (SutShow(showSut), SutLog(SutLogLeave))

import Sutori.Error.Error

-- |A 'SutError' can be printed nicely
instance SutShow SutError where
  showSut = SutLogLeave . show
