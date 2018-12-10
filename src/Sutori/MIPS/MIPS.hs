module Sutori.MIPS.MIPS where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Sutori.TAC.TAC

import Sutori.AST (SutID)
import Sutori.SymTable (Scope)

-- | MIPS Available instructions
data MIPS
  = Label String
  | LoadWord      Reg MemAddr
  | LoadByte      Reg MemAddr
  | LoadImmediate Reg MemAddr
  | LoadAddress   Reg MemAddr
  | StoreWord     Reg MemAddr
  | StoreByte     Reg MemAddr
  | Add           Reg Reg Reg
  | Sub           Reg Reg Reg
  | Mult          Reg Reg
  | Div           Reg Reg
  | MoveFromHi    Reg
  | MoveFromLo    Reg
  | Move          Reg Reg
  | Jump          String
  | JumpReg       Reg
  | JumpAndLink   String
  | BranchNEq     Reg Reg String
  | Syscall

-- | A memory address for memory instructions
data MemAddr = MemReg Reg Offset  -- ^Some offset from an address kept in a register
             | MemLabel String    -- ^A label somewhere in the code

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

-- | Defines descriptors for registers, so we keep track of which variables live on each register
type RegsDescriptor = Map.Map Reg (Set.Set (SutID,Scope))

-- | Defines descriptors for variables, so we keep track of which registers have their current value
type VarsDescriptor = Map.Map (SutID, Scope) (Set.Set Reg)
