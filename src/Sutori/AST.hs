{-|
Description: Defines an API for accessing the different data types
             for nodes of the AST, along with a couple of functions
             to work with them.
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

import Sutori.AST.Logger
import Sutori.AST.Nodes
import Sutori.AST.Utils
