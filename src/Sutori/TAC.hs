{-|
  Description: Intermediate representation of the code: Three TACAddress Code
-}
module Sutori.TAC
( TACTable(..)
, TACAddress(..)
, TACType(..)
, TAC(..)
, addTAC
, genCode
) where

import Sutori.TAC.TAC
import Sutori.TAC.CodeGen
import Sutori.TAC.Logger ()
