module Sutori.MIPS.CodeGen where

import Sutori.TAC.TAC
import Sutori.TAC.ControlFlow
import Sutori.Monad

import Sutori.MIPS.MIPS

-- | Given a TAC instruction, outputs an equivalent sequence of MIPS instructions
genCode :: TAC -> SutMonad [MIPS]
genCode _ = return []

-- | Given a TAC instruction, gives a list of registers to use for the equivalent MIPS instruction
getReg :: TAC -> [Reg]
getReg _ = []
