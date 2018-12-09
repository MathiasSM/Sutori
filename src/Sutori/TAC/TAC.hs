{-|
  Description: Intermediate representation of the code: Three TACAddress Code
-}
module Sutori.TAC.TAC
( TACTable(..)
, TACAddress(..)
, TACType(..)
, TAC(..)
, SutSys(..)
) where

import Sutori.AST   (SutID, SutLiteral, SutOperator)

-- |The actual generated code is a list of instruction triplets and a list of pointers to them
data TACTable = TACTable
  { tacInstructions :: [Int]   -- ^Table of pointers to triplets
  , tacTriplets     :: [TAC] } -- ^Table of triplets

-- |TACAddress for TAC Instructions
data TACAddress
  = TACName (SutID, Int)  -- ^An actual ID from the source code. Identified using 'SutID' and 'Scope'. Used for variables
  | TACLit SutLiteral     -- ^A literal value either explicit in the source code or calculated on the run.
  | TACID Int             -- ^The TAC number that calculated the relevant expression.
  | TACLabel Int          -- ^An incremental label to some code.

-- |TAC Instruction
data TAC
  -- |An instruction
  = TAC
    { tacType :: TACType               -- ^Operation to perform
    , tac1    :: Maybe TACAddress      -- ^First address. Usually first operand.
    , tac2    :: Maybe TACAddress      -- ^Second address. Usually second operand.
    }
  | Label    Int    -- ^A numbered label to output literally
  | FunLabel SutID  -- ^A string label to output literally

-- |A TAC Instruction to be appended to the generated intermediate code
data TACType
  = Basic { tacOp :: SutOperator } -- ^For simple expressions.
  | SysCall { tacSys :: SutSys }   -- ^Perform a syscall (IO, Mem, else)
  | Copy                           -- ^Copy operation, from a address to another
  | Addr                           -- ^Address for an indirection
  | Jump                           -- ^Unconditional jump, just where to.
  | JumpUnless                     -- ^Condition jump
  | Pointed                        -- ^Assign an address from a indirect pointer.
  | Param                          -- ^Stack a function parameter
  | Call                           -- ^Call a function of so many (already stacked) parameters
  | Return                         -- ^Return from a function (possibly a value).

-- |Represents a kind of possible system call
data SutSys
  = SysRead   -- ^IO Read operation
  | SysPrint  -- ^IO Print operation
  | SysAlloc  -- ^Memory allocation operation
  | SysFree   -- ^Memory freeing operation
