{-|
Description : Provides 'ShowSut' instances for "Sutori.SymTable"
-}
module Sutori.SymTable.Logger() where

import Sutori.Logger    (SutShow(showSut), SutLog(SutLogNode, SutLogLeave))
import Sutori.AST       ()

import Sutori.SymTable.Symbol
import Sutori.SymTable.Table


-- | Constructs a log for the symbol data
logSymbol :: SutSymbol a => [a -> SutLog] -> a -> SutLog
logSymbol lfs s = SutLogNode ("Symbol: " ++ symID s ++ " - Scope: " ++ (show . symScope) s) (map (\f -> f s) lfs)

-- | Constructs a log for the symbol Type
logType :: TypedSymbol a => a -> SutLog
logType s = SutLogLeave $ "Type:" ++ show (symType s)

-- | Constructs a log node for the symbol Params
logParams :: ParametricSymbol a => a -> SutLog
logParams s = SutLogNode "Parameters:" (map showSut (symParams s))

-- | Constructs a log node for the symbol AST
logAST :: ASTSymbol a => a -> SutLog
logAST s = SutLogNode "AST:" (map showSut (symAST s))


-- | Module shows its AST
instance SutShow SymModule where
  showSut = logSymbol [logAST]

-- | Function shows its type, params and AST
instance SutShow SymFunction where
  showSut = logSymbol [logType, logParams, logAST]

-- | Person shows only the basic
instance SutShow SymPerson where
  showSut = logSymbol []

-- | Variable shows its type
instance SutShow SymVariable where
  showSut = logSymbol [logType]

-- | Type shows its type (definition)
instance SutShow SymType where
  showSut = logSymbol [logType]


-- |A parameter can be printed nicely
instance SutShow SutParam where
  showSut SutParam{isRef = r, paramType = t, paramID = id} =
    SutLogLeave ("Param: " ++ id ++ "; TypeID: " ++ show t ++ "; isRef: " ++ show r)

instance SutShow SymbolCat where
  showSut CatModule   = SutLogLeave "Module"
  showSut CatPerson   = SutLogLeave "Person"
  showSut CatType     = SutLogLeave "Type"
  showSut CatVariable = SutLogLeave "Variable"
  showSut CatFunction = SutLogLeave "Function"
