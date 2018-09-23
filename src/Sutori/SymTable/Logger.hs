module Sutori.SymTable.Logger() where

import Sutori.Logger(SutShow(showSut), SutLog(SutLogNode, SutLogLeave))
import Sutori.SymTable
import Sutori.AST.Logger

instance SutShow SutSymCategory where
  showSut CatModule    = SutLogLeave "Category: Module"
  showSut CatFunction  = SutLogLeave "Category: Function"
  showSut CatPerson    = SutLogLeave "Category: Person"
  showSut CatVariable  = SutLogLeave "Category: Variable"
  showSut CatParameter = SutLogLeave "Category: Parameter"
  showSut CatType      = SutLogLeave "Category: Type"


instance SutShow SutParamKind where
  showSut SutRef = SutLogLeave "Passed by: Reference"
  showSut SutVal = SutLogLeave "Passed by: Value"

instance SutShow SutSymOther where
  showSut (SymAST ps b)    = let params = SutLogNode "Parameters: " (map showSut ps)
                                 block  = SutLogNode "AST: " (map showSut b)
                              in SutLogNode "Function definition:" [params, block]
  showSut (SymParamKind k) = showSut k
  showSut (SymTypeDef t)   = SutLogLeave  $ "Type definition: " ++ show t
  showSut  SymNothing      = SutLogLeave "-"


instance SutShow SutSymbol where
  showSut s = let etype = SutLogLeave $ "Type: " ++ show (symType s)
                  cat   = showSut $ symCat s
                  scope = SutLogLeave $ "Scope: " ++ show (symScope s)
                  other = showSut (symOther s)
               in SutLogNode ("Symbol: " ++ symID s) [cat, etype, scope, other]


instance SutShow SutParam where
  showSut p = let kind  = showSut (paramKind p)
                  etype = SutLogLeave $ "Type: " ++ show (paramType p)
               in SutLogNode ("Parameter: " ++ paramID p) [etype, kind]
