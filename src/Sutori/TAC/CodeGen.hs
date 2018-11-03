{-|
  Description: Intermediate (TAC) code generation rules
-}

module Sutori.TAC.CodeGen where

import Control.Monad.State (get, put)

import Sutori.Monad

import Sutori.TAC.TAC

-- |Appends to the TAC table a new triplet, references it on the instructions TAC table
addTAC :: TAC -> SutMonad TACAddress
addTAC tac = do
  s@SutState{ tacNext = i, tacTable = TACTable{ tacInstructions = is, tacTriplets = tacs } } <- get
  put s{ tacNext = i + 1, tacTable = TACTable{tacInstructions = i:is, tacTriplets = tac:tacs} }
  return $ TACID i
