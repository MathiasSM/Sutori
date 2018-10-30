{-|
Description : Defines the data types for nodes of the AST
-}
module Sutori.AST.Nodes
( SutID
, SutAST
, SutModule(..)
, SutInstruction(..)
, SutExpression(..)
, SutLiteral(..)
, SutConstructor(..)
, SutOperator(..)
) where

import Sutori.Utils (SutID)
import Sutori.Types (SutType)


-- |A 'SutBlock' is a list of instructions
type SutAST = [SutInstruction]

-- |A Sutori Module has a name and a SutAST
data SutModule = SutModule SutID SutAST
  deriving (Show, Eq)


-- |A 'SutIntruction' represents an instruction/action in the (imperative) story
data SutInstruction
  = InstExpression SutExpression                         -- ^ Some expressions can be understood an an instruction on its own
  | ReadVal        SutID SutExpression                   -- ^ The action of someone asking for the value of an expression
  | ReturnVal      SutExpression                         -- ^ The action of finishing a story with an expression
  | Selection      SutID SutExpression SutAST SutAST     -- ^ A selection between two blocks of code given a true/false condition
  | IterationU     SutID SutExpression SutAST            -- ^ An unbounded iteration while a condition persists
  | IterationB     SutID SutExpression SutAST            -- ^ A bounded iteration of a story to be repeated a number of times
  | FreePointer    SutID SutExpression                   -- ^ The action of freeing a direction (so it points nowhere)
  | PrintVal       SutID SutExpression                   -- ^ The action of a person saying the value of an expression (into the console)
  | Break                                                -- ^ Stops the innermost iteration loop (skips the rest of the iteratons)
  | Continue                                             -- ^ Skips the current iteration (stays in the loop, skips to next iteration)
  deriving (Show, Eq)


-- |A SutExpression
data SutExpression = ArrayGet        SutType SutExpression  SutExpression     -- ^ Indexing of an array to get an element
                   | BinaryOp        SutType SutOperator    SutExpression   SutExpression -- ^ A binary operation
                   | UnaryOp         SutType SutOperator    SutExpression     -- ^ An unary operation
                   | SutCall         SutType SutID          [SutExpression]   -- ^ A function call
                   | CreatePointer   SutType SutID                            -- ^ A direction (pointer) creation
                   | ExprConstructor SutType SutConstructor                   -- ^ A construct (machines, chains, etc...)
                   | ExprID          SutType SutID                            -- ^ An ID as an expression
                   | ExprLiteral     SutType SutLiteral                       -- ^ A literal
                   | Dereference     SutType SutExpression                    -- ^ Dereference of a direction (pointer)
                   | MemberGet       SutType SutExpression  SutID             -- ^ Access to a member of a structured value
  deriving (Show, Eq)

-- |A 'SutLiteral' represents a literal expression
data SutLiteral = SutString String    -- ^ Phrase (String)
                | SutInt    Int       -- ^ Bag (Int)
                | SutFloat  Float     -- ^ Wallet (Float)
                | SutChar   String    -- ^ Letter (Char)
                | SutBool   Bool      -- ^ Light (Bool)
  deriving (Show, Eq)

-- |Complex data structure constructors
data SutConstructor = SutArray [SutExpression]           -- ^ "Literal" Chains (Srrays)
                    | SutStruct [(SutID, SutExpression)] -- ^ "Literal" Machines (Structs)
  deriving (Show, Eq)

-- |Sutori operators
data SutOperator = SutOpPos     -- ^ Unary plus/make positive
                 | SutOpNeg     -- ^ Unary minus/negate
                 | SutOpNot     -- ^ Unary not
                 | SutOpDer     -- ^ Unary dereference

                 | SutOpAdd     -- ^ Binary addition
                 | SutOpSub     -- ^ Binary substraction
                 | SutOpMul     -- ^ Binary multiplication
                 | SutOpDiv     -- ^ Binary division
                 | SutOpIntDiv  -- ^ Binary integer division
                 | SutOpMod     -- ^ Binary modulo
                 | SutOpPow     -- ^ Binary power
                 | SutOpAnd     -- ^ Binary and
                 | SutOpOr      -- ^ Binary or
                 | SutOpEqual   -- ^ Binary equal
                 | SutOpNotEq   -- ^ Binary not equal
                 | SutOpGEq     -- ^ Binary greater or equal than
                 | SutOpLEq     -- ^ Binary less or equal than
                 | SutOpGreater -- ^ Binary greater than
                 | SutOpLess    -- ^ Binary less than
                 | SutOpAssign  -- ^ Binary assignment
                 | SutOpIndex   -- ^ Binary indexation
                 | SutOpMember  -- ^ Binary member get
  deriving (Show, Eq)
