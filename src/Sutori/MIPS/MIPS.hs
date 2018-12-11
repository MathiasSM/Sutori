module Sutori.MIPS.MIPS where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Sutori.TAC.TAC

import Sutori.AST (SutID)
import Sutori.SymTable (Scope)

-- | MIPS Available instructions
data MIPS
  = MLabel String
  | MLoadWord      Reg MemAddr
  | MLoadByte      Reg MemAddr
  | MLoadImmediate Reg Int
  | MLoadAddress   Reg MemAddr
  | MStoreWord     Reg MemAddr
  | MStoreByte     Reg MemAddr
  | MAdd           Reg Reg Reg
  | MSub           Reg Reg Reg
  | MMul           Reg Reg Reg
  | MRem           Reg Reg Reg
  | MDiv           Reg Reg Reg
  | MDivS          Reg Reg Reg
  | MAnd           Reg Reg Reg
  | MOr            Reg Reg Reg
  | MEq            Reg Reg Reg
  | MNe            Reg Reg Reg
  | MGt            Reg Reg Reg
  | MGe            Reg Reg Reg
  | MLt            Reg Reg Reg
  | MLe            Reg Reg Reg
  | MNot           Reg Reg
  | MNeg           Reg Reg
  | MMove          Reg Reg
  | MJump          String
  | MJumpReg       Reg
  | MJumpAndLink   String
  | MBeq           Reg Reg String
  | MNoop
  | MSyscall

-- | A memory address for memory instructions
data MemAddr = MemReg Reg Offset        -- ^Some offset from an address kept in a register
             | MemLabel String Offset   -- ^A label somewhere in the code

-- | MIPS available registers
data Reg
  = T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 -- ^TX: temporal values (caller-saved)
  | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7           -- ^SX: saved values (calle-saved)
  | A0 | A1 | A2 | A3 -- ^AX: function arguments (first few, then we stack)
  | V0 | V1           -- ^VX: function return values
  | GP        -- ^Global Pointer
  | FP        -- ^Frame Pointer
  | SP        -- ^Stack Pointer
  | RA        -- ^Return Address
  | RZ        -- ^Always Zero (useful)
  deriving (Eq, Ord)

-- | Defines descriptors for registers, so we keep track of which variables live on each register
type RegDescriptors = Map.Map Reg (Set.Set TACAddress)

-- | Defines descriptors for variables, so we keep track of which registers have their current value
type VarDescriptors = Map.Map TACAddress (Set.Set Reg)
