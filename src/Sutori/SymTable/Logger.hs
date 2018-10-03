{-|
Description : Provides 'ShowSut' instances for "Sutori.SymTable"
-}
module Sutori.SymTable.Logger() where

import Sutori.AST.Logger
import Sutori.Logger     (SutShow(showSut), SutLog(SutLogNode, SutLogLeave))
import Sutori.SymTable

-- |A symbol category can be printed nicely
instance SutShow SutSymCategory where
  showSut CatModule    = SutLogLeave "Category: Module"
  showSut CatFunction  = SutLogLeave "Category: Function"
  showSut CatPerson    = SutLogLeave "Category: Person"
  showSut CatVariable  = SutLogLeave "Category: Variable"
  showSut CatParameter = SutLogLeave "Category: Parameter"
  showSut CatType      = SutLogLeave "Category: Type"
  showSut CatMember    = SutLogLeave "Category: Member"

-- |A parameter kind can be printed nicely
instance SutShow SutParamKind where
  showSut SutRef = SutLogLeave "Passed by: Reference"
  showSut SutVal = SutLogLeave "Passed by: Value"

-- |The extra payload of a symbol can be printed nicely
instance SutShow SutSymOther where
  showSut (SymAST ps b)    = let params = SutLogNode "Parameters: " (map showSut ps)
                                 block  = SutLogNode "AST: " (map showSut b)
                              in SutLogNode "Function definition:" [params, block]
  showSut (SymParamKind k) = showSut k
  showSut (SymTypeDef t)   = SutLogLeave  $ "Type definition: " ++ show t
  showSut  SymNothing      = SutLogLeave "-"

-- |A sutori symbol in the table can be printed nicely
instance SutShow SutSymbol where
  showSut s = let etype = SutLogLeave $ "Type: " ++ show (symType s)
                  cat   = showSut $ symCat s
                  scope = SutLogLeave $ "Scope: " ++ show (symScope s)
                  other = showSut (symOther s)
               in SutLogNode ("Symbol: " ++ symID s) [cat, etype, scope, other]

-- |A parameter can be printed nicely
instance SutShow SutParam where
  showSut p = let kind  = showSut (paramKind p)
                  etype = SutLogLeave $ "Type: " ++ show (paramType p)
               in SutLogNode ("Parameter: " ++ paramID p) [etype, kind]
