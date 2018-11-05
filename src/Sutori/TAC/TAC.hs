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

-- The actual generated code is a list of instruction triplets and a list of pointers to them
data TACTable = TACTable
  { tacInstructions :: [Int]   -- ^Table of pointers to triplets
  , tacTriplets     :: [TAC] } -- ^Table of triplets

-- |TACAddress for TAC Instructions
data TACAddress
  = TACName (SutID, Int)  -- ^An actual ID from the source code. Identified using 'SutID' and 'Scope'.
  | TACLit SutLiteral     -- ^A literal either explicit in the source code or calculated on the run.
  | TACID Int             -- ^The TAC number that calculated the relevant expression
  | TACLabel Int          -- ^The label to some code

-- |TAC Instruction
data TAC
  = TAC                              -- ^An instruction
    { tacType :: TACType               -- ^Operation to perform
    , tac1    :: Maybe TACAddress      -- ^First address. Usually first operand.
    , tac2    :: Maybe TACAddress }    -- ^Second address. Usually second operand.
  | Label Int                        -- ^A label to output literally

-- |A TAC Instruction to be appended to the generated intermediate code
data TACType
  = Basic             -- ^For simple expressions.
    { tacOp :: SutOperator }
  | Copy              -- ^Copy operation, from a address to another
  | Jump              -- ^Unconditional jump, just where to.
  | JumpUnless        -- ^Condition jump
  | Array             -- ^Assign a address to an indexed value.
  | Pointed           -- ^Assign a address from a indirect pointer.
  | Param             -- ^Stack a function parameter
  | Call              -- ^Call a function of so many (already stacked) parameters
  | Return            -- ^Return from a function (possibly a value).
  | SysCall           -- ^Perform a syscall (IO, Mem, else)
    { tacSys :: SutSys }

-- |Represents a kind of possible system call
data SutSys
  = SysRead   -- ^IO Read operation
  | SysPrint  -- ^IO Print operation
  | SysAlloc  -- ^Memory allocation operation
  | SysFree   -- ^Memory freeing operation
