{-|
Description : Defines the data types for nodes of the AST, along with a couple of extractor functions
-}
module Sutori.AST
( SutID
, SutBlock
, SutModule(..)
, SutInstruction(..)
, SutExpression(..)
, SutLiteral(..)
, SutConstructor(..)
, SutOperator(..)
, expressionType
, withPrimitiveType
, asTypeError
) where

import Data.Maybe

import Sutori.Utils              (SutID)
import Sutori.Types.Constructors (SutType(SutPrimitiveType))
import Sutori.Types.Primitives   (SutPrimitive(SutTypeError))

-- |A 'SutBlock' is a list of instructions
type SutBlock = [SutInstruction]

-- |A Sutori Module has a name and a SutBlock
data SutModule = SutModule SutID SutBlock


-- |A 'SutIntruction' represents an instruction/action in the (imperative) story
data SutInstruction
  = InstAssignment SutExpression                         -- ^ An assignment can be understood an an instruction on its own
  | ReadVal        SutExpression                         -- ^ The action of someone asking for the value of an expression
  | ReturnVal      SutExpression                         -- ^ The action of finishing a story with an expression
  | Selection      SutID SutExpression SutBlock SutBlock -- ^ A selection between two blocks of code given a true/false condition
  | IterationU     SutID SutExpression SutBlock          -- ^ An unbounded iteration while a condition persists
  | IterationB     SutID SutExpression SutBlock          -- ^ A bounded iteration of a story to be repeated a number of times
  | FreePointer    SutID SutExpression                   -- ^ The action of freeing a direction (so it points nowhere)
  | PrintVal       SutID SutExpression                   -- ^ The action of a person saying the value of an expression (into the console)


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

-- |A 'SutLiteral' represents a literal expression
data SutLiteral = SutString String    -- ^ Phrase (String)
                | SutInt    Int       -- ^ Bag (Int)
                | SutFloat  Float     -- ^ Wallet (Float)
                | SutChar   String    -- ^ Letter (Char)
                | SutBool   Bool      -- ^ Light (Bool)

-- |Complex data structure constructors
data SutConstructor = SutArray [SutExpression]           -- ^ "Literal" Chains (Srrays)
                    | SutStruct [(SutID, SutExpression)] -- ^ "Literal" Machines (Structs)

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


-- |Extracts the type from any expression
expressionType :: SutExpression -> SutType
expressionType (ArrayGet t _ _)      = t
expressionType (BinaryOp t _ _ _)    = t
expressionType (UnaryOp t _ _)       = t
expressionType (SutCall t _ _)       = t
expressionType (CreatePointer t _)   = t
expressionType (ExprConstructor t _) = t
expressionType (ExprID t _)          = t
expressionType (ExprLiteral t _)     = t
expressionType (Dereference t _)     = t
expressionType (MemberGet t _ _ )    = t

-- |Clones the expression overriding the type for the given primitive type
-- This is used for type coersion, mostly
withPrimitiveType :: SutPrimitive -> SutExpression -> SutExpression
withPrimitiveType p (ArrayGet _ e1 e2)    = ArrayGet (SutPrimitiveType p) e1 e2
withPrimitiveType p (BinaryOp _ o e1 e2)  = BinaryOp (SutPrimitiveType p) o e1 e2
withPrimitiveType p (UnaryOp _ o e)       = UnaryOp (SutPrimitiveType p) o e
withPrimitiveType p (SutCall _ id ps)     = SutCall (SutPrimitiveType p) id ps
withPrimitiveType p (CreatePointer _ e)   = CreatePointer (SutPrimitiveType p) e
withPrimitiveType p (ExprConstructor _ c) = ExprConstructor (SutPrimitiveType p) c
withPrimitiveType p (ExprID _ id)         = ExprID (SutPrimitiveType p) id
withPrimitiveType p (ExprLiteral _ l)     = ExprLiteral (SutPrimitiveType p) l
withPrimitiveType p (Dereference _ e)     = Dereference (SutPrimitiveType p) e
withPrimitiveType p (MemberGet _ e id)    = MemberGet (SutPrimitiveType p) e id

-- |Clones the expression but sets the type as a TypeError
asTypeError :: SutExpression -> SutExpression
asTypeError = withPrimitiveType SutTypeError
