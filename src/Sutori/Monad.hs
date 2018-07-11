module Sutori.Monad where

import Data.Maybe
import Data.Semigroup
import Data.Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Zip

import Sutori.AST
import Sutori.Types
import Sutori.Utils
import Sutori.SymTable
import Sutori.Lexer

-- Data types {{{1
type SutParserM a = StateT SutParserState (WriterT SutParserLog (Either SutParserError)) a

-- SymTable {{{2

data SutParserState = SutParserState {
  getSymTable :: SymTable,
  getStack    :: [Scope],
  getIdScope  :: Scope,
  getSet      :: Set.Set Scope
} deriving (Eq,Show)
getCurScope = head . getStack
emptyParserState = SutParserState (SymTable Map.empty) [0] 0 (Set.insert 0 Set.empty)

newtype SutParserError = SutParserError Int

newtype SutParserLog = SutParserLog {getErrorsLog :: String}

instance Monoid SutParserLog where
  mempty = SutParserLog ""
  mappend (SutParserLog a) (SutParserLog b) = SutParserLog (a++b)

instance Show SutParserLog where
  show (SutParserLog a) = "[ERROR]\n"++a++"\n"



getTuple f a = getTuple $ getRight $ runSutParserM f a `catchError` parseError
    where getRight (Right x) = x
          getTuple((ast,st),log) = (ast,st,log)
          parseError (SutParserError pos) = error $ "\nError in line "++show pos


-- Regular Actions
---------------------------------------------------------------------------------------------------
runSutParserM :: SutParserM a -> SutParserState -> Either SutParserError ((a, SutParserState), SutParserLog)
runSutParserM f a = runWriterT $ runStateT f a

printSymTable :: SutParserState -> IO ()
printSymTable mp = mapM_ print (Map.elems (getHash $ getSymTable mp))

addScope :: SutParserM ()
addScope = do
    oldState <- get
    let newIdScope = 1 + getIdScope oldState
        newSet = Set.insert newIdScope (getSet oldState)
        newStack = newIdScope:getStack oldState
    put $ oldState { getStack = newStack, getIdScope = newIdScope, getSet = newSet}

removeLastScope :: SutParserM ()
removeLastScope = do
    oldState <- get
    let oldStack = getStack oldState
        newStack = tail oldStack
        newSet = Set.delete (head oldStack) (getSet oldState)
    put $ oldState { getStack = newStack, getSet = newSet }



-- Insertion and modification of symbols
---------------------------------------------------------------------------------------------------
insertInitial :: AlexPosn -> SutParserM ()
insertInitial pos = mapM_ (uncurry (insertType pos)) predefinedTypeNames

insertVars :: AlexPosn -> SutType -> [SutID] -> SutParserM ()
insertVars pos t ids = let what = "Variable" in do
    oldState <- get
    let curScope = getCurScope oldState
        newSymTable = insert (getSymTable oldState) curScope VarSym t NoOther ids
    checkToBeNew pos what curScope ids
    put $ oldState { getSymTable = newSymTable }

insertPerson :: AlexPosn -> [SutID] -> SutParserM ()
insertPerson ids = let what = "Person" in do
    oldState <- get
    let curScope = getCurScope oldState
        newSymTable = insert (getSymTable oldState) curScope PersonSym SutTypeVoid NoOther ids
    checkToBeNew pos what curScope ids
    put $ oldState { getSymTable = newSymTable }

insertType :: AlexPosn -> SutID -> SutType -> SutParserM ()
insertType id t = let what = "Type" in do
    oldState <- get
    checkRepeated pos what [id] >>= checkAlreadyDefined what (head $ getStack oldState)
    let newSymTable = insert (getSymTable oldState) (head $ getStack oldState) TypeSym SutTypeVoid (TypeDef t) [id]
    put $ oldState { getSymTable = newSymTable }

insertFunction :: AlexPosn -> SutID -> SutParserM ()
insertFunction pos id = let what = "Function" in do
    oldState <- get
    let newCurScope = 1 + getCurScope oldState
        oldStack = getStack oldState
        newSymTable = insert (getSymTable oldState) (head oldStack) FunctionSym SutTypeVoid NoOther [id]
        newSet = Set.insert newCurScope (getSet oldState)
        newStack = newCurScope:oldStack
    checkToBeNew pos what newCurScope [id]
    put $ oldState { getSymTable = newSymTable, getStack = newStack, getIdScope = newCurScope, getSet = newSet}

insertFunctionParams :: AlexPosn -> [SutParam] -> SutParserM ()
insertFunctionParams pos ps = let what = "Parameter" in do
    oldState <- get
    let newSymTable = insertParam (getSymTable oldState) (head $ getStack oldState) ParamSym ps
    checkRepeated pos what (map thd ps) >>= checkAlreadyDefined pos what (getCurScope oldState)
    put $ oldState { getSymTable = newSymTable }
    where thd (_,_,a) = a

modifyFunction :: SutID -> SutBlock -> [SutParam] -> SutType -> SutParserM ()
modifyFunction id funBlock params t = do
    oldState <- get
    let oldSymbols = lookupId (getSymTable oldState) id
        oldSymbol = head oldSymbols
        newSymbol = Symbol id FunctionSym (getScope oldSymbol) t (FunctionAST funBlock params)
        newSymbols = map (\x -> if x /= oldSymbol then x else newSymbol) oldSymbols
        newSymTable = SymTable $ updateSymList (getHash $ getSymTable oldState) id newSymbols
    put $ oldState { getSymTable = newSymTable }



-- Checks
---------------------------------------------------------------------------------------------------
checkRepeated :: AlexPosn -> String -> [SutID] -> SutParserM [SutID]
checkRepeated pos what ids = do
    let rep = repeated ids
        unq = List.nub ids
    unless (null rep) $ mapM_ logListError rep
    return unq
    where logListError = logAlreadyDefined pos "declaration" what

checkAlreadyDefined :: AlexPosn -> String -> Scope -> [SutID] -> SutParserM ()
checkAlreadyDefined pos what scope ids = do
    oldState <- get
    let setsIn = lookupsInScope (getSymTable oldState) scope ids
        someIn = not $ all null setsIn
        thoseIn = filter null setsIn
    when someIn $ mapM_ (logScopeError.head) thoseIn
    where logScopeError s = logAlreadyDefined pos "scope" what $ getId s

checkToBeNew :: AlexPosn -> String -> Scope -> [SutID] -> SutParserM ()
checkToBeNew pos what scope ids = checkRepeated pos what ids >>= checkAlreadyDefined pos what scope

checkId :: AlexPosn -> SutID -> SymCategory -> SutType -> SutParserM (Maybe Symbol)
checkId pos id cat t = do
    state <- get
    let symbols = lookupId (getSymTable state) id
        activeScopes = getSet state
        good = filter (allf [isInScopes activeScopes, isThisCat, isThisType]) symbols
    when (null good) $ logNotDefined pos (showSut cat) id
    return $ if null good then Nothing else Just $ head good
    where isInScopes scopes sym = Set.member (getScope sym) scopes
          isThisCat sym = getCategory sym == cat
          isThisType sym = getType sym == t

checkParams :: AlexPosn -> Symbol -> [SutParam] -> SutParserM ()
checkParams pos fs args = do
  let actualTypes = map (\(_,t,_) -> t) args
      formalTypes = getParams $ getOther fs
      matches = zip formalTypes actualTypes
      badMatches = filter (\((_,tp,_), t) -> tp /= t) matches
      badNumber = length actualTypes /= length formalTypes
      badTypes = not $ null badMatches
  when badNumber $ logBadParamNumber pos (getId fs) (length formalTypes) (length actualTypes)
  when badTypes $ mapM_ (uncurry $ logBadParamType pos (getId fs)) badMatches

checkIndexType :: AlexPosn -> SutType -> SutParserM SutType
checkIndexType pos t = do
    let newType = typesLCA SutTypeInt t
    when (newType == SutTypeError) $ logIndexNeedsInt pos t
    return newType



-- Error logs
---------------------------------------------------------------------------------------------------
logAlreadyDefined :: AlexPosn -> String -> String -> SutID -> SutParserM ()
logAlreadyDefined pos place thing id = tell $ SutParserLog $
  "\n"++showAlex++": "++thing++" '"++id++"' already defined in the same "++place++"."

logNotDefined :: AlexPosn -> String -> SutID -> SutParserM ()
logNotDefined thing id = tell $ SutParserLog $
  "\n"++showAlex++": "++thing++" '"++id++"' not defined in any active scope."

logBadParamNumber :: AlexPosn -> SutID -> Int -> Int -> SutParserM ()
logBadParamNumber pos id formal actual = let howmany = if actual > formal then "more" else "fewer" in
  tell $ SutParserLog $
    "\n"++showAlex pos++"Function '"++id++"' needs "++howmany++" arguments ("++show formal++" instead of "++show actual++")."

logBadParamType :: AlexPosn -> SutID -> SutParam -> SutType -> SutParserM ()
logBadParamType pos fid (ref, t, id) t' = tell $ SutParserLog $
  "\n"++showAlex pos++": Function '"++fid++"' parameter '"++id++"' has type (1) (you used (2))"++
  "\n\t(1)\t"++showSut t++
  "\n\t(2)\t"++showSut t'

logIndexNeedsInt :: AlexPosn -> SutType -> SutParserM ()
logIndexNeedsInt pos t = tell $ SutParserLog $
  "\n"++showAlex pos++":Chain types (arrays) need integer-convertibles indexes. Type (1) is not convertible to int."++
  "\n\t(1)\t"++showSut t
