{-|
  Description: Intermediate (TAC) code generation rules
-}

module Sutori.TAC.CodeGen where

import Control.Monad.State (get, put)

import Sutori.AST
import Sutori.Monad

import Sutori.TAC.TAC

-- |Generates a new (unique) temporal register/address for TAC
newtemp :: SutMonad TACAddress
newtemp = do
  s@SutState{ tempNext = temp } <- get
  put s{ tempNext = temp + 1 }
  return $ TACTemp temp
