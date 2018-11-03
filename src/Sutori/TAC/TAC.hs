{-|
  Description: Intermediate representation of the code: Three TACAddress Code
-}
module Sutori.TAC.TAC
( TACTable(..)
, TACAddress(..)
, TACType(..)
, TAC(..)
) where

import Sutori.AST   (SutID, SutLiteral, SutOperator)

-- The actual generated code is a list of instruction triplets and a list of pointers to them
data TACTable = TACTable
  { tacInstructions :: [Int]   -- ^Table of pointers to triplets
  , tacTriplets     :: [TAC] } -- ^Table of triplets

-- |TACAddress for TAC Instructions
data TACAddress
  = TACName SutID     -- ^An actual 'SutID' from the source code.
  | TACLit SutLiteral -- ^A literal either explicit in the source code or calculated on the run.
  | TACTemp Int       -- ^A temporal register
  | TACID Int         -- ^The TAC number the calculated the relevant expression

-- |TAC Instruction
data TAC = TAC
  { tacType :: TACType               -- ^Operation to perform
  , tac1    :: Maybe TACAddress      -- ^First address. Usually first operand.
  , tac2    :: Maybe TACAddress }    -- ^Second address. Usually second operand.

-- |A TAC Instruction to be appended to the generated intermediate code
data TACType
  = Basic             -- ^For simple expressions.
    { tacOp :: SutOperator }
  | Copy              -- ^Copy operation, from a address to another
  | Jump              -- ^Unconditional jump, just where to.
  | JumpIf            -- ^Condition jump
  | FromArray         -- ^Assign an indexed value to a address
  | ToArray           -- ^Assign a address to an indexed value.
  | FromPointed       -- ^Assign a address from a indirect pointer.
  | ToPointed         -- ^Assign a pointed address to a address.
  | Param             -- ^Stack a function parameter
  | Call              -- ^Call a function of so many (already stacked) parameters
  | Return            -- ^Return from a function (possibly a value).
