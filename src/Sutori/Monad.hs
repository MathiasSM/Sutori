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
emptyParserState =
  SutParserState (SymTable Map.empty) [0] 0 (Set.insert 0 Set.empty)

newtype SutParserError = SutParserError Int

newtype SutParserLog = SutParserLog {getErrorsLog :: String}

instance Monoid SutParserLog where
  mempty = SutParserLog ""
  mappend (SutParserLog a) (SutParserLog b) = SutParserLog (a++b)

instance Show SutParserLog where
  show (SutParserLog a) = "[ERROR]\n"++a++"\n"



getTuple f a = getTuple $ getRight $ runSutParserM f a `catchError` parseError
 where
  getRight (Right x) = x
  getTuple ((ast, st), log) = (ast, st, log)
  parseError (SutParserError pos) = error $ "\nError in line " ++ show pos


-- Regular Actions
---------------------------------------------------------------------------------------------------
runSutParserM
  :: SutParserM a
  -> SutParserState
  -> Either SutParserError ((a, SutParserState), SutParserLog)
runSutParserM f a = runWriterT $ runStateT f a

printSymTable :: SutParserState -> IO ()
printSymTable mp = mapM_ print (Map.elems (getHash $ getSymTable mp))

addScope :: SutParserM ()
addScope = do
  oldState <- get
  let newIdScope = 1 + getIdScope oldState
      newSet     = Set.insert newIdScope (getSet oldState)
      newStack   = newIdScope : getStack oldState
  put $ oldState { getStack   = newStack
                 , getIdScope = newIdScope
                 , getSet     = newSet
                 }

removeLastScope :: SutParserM ()
removeLastScope = do
  oldState <- get
  let oldStack = getStack oldState
      newStack = tail oldStack
      newSet   = Set.delete (head oldStack) (getSet oldState)
  put $ oldState { getStack = newStack, getSet = newSet }



-- Insertion and modification of symbols
---------------------------------------------------------------------------------------------------
insertInitial :: SutParserM ()
insertInitial = mapM_ insertType' predefinedTypes

insertType' :: (SutID, SutType) -> SutParserM ()
insertType' (id, t) =
  let what = "Type"
  in  do
        oldState <- get
        let tk          = fakeToken (SutTkId id)
            newSymTable = insert (getSymTable oldState)
                                 (head $ getStack oldState)
                                 TypeSym
                                 SutTypeVoid
                                 (TypeDef t)
                                 [tk]
        put $ oldState { getSymTable = newSymTable }

insertVars :: SutType -> [SutToken] -> SutParserM ()
insertVars t tks =
  let what = "Variable"
  in  do
        oldState <- get
        let curScope = getCurScope oldState
            newSymTable =
              insert (getSymTable oldState) curScope VarSym t NoOther tks
        checkToBeNew what curScope tks
        put $ oldState { getSymTable = newSymTable }

insertPerson :: [SutToken] -> SutParserM ()
insertPerson tks =
  let what = "Person"
  in  do
        oldState <- get
        let curScope    = getCurScope oldState
            newSymTable = insert (getSymTable oldState)
                                 curScope
                                 PersonSym
                                 SutTypeVoid
                                 NoOther
                                 tks
        checkToBeNew what curScope tks
        put $ oldState { getSymTable = newSymTable }

insertType :: SutToken -> SutType -> SutParserM ()
insertType tk t =
  let what = "Type"
  in  do
        oldState <- get
        checkRepeated what [tk]
          >>= checkAlreadyDefined what (head $ getStack oldState)
        let newSymTable = insert (getSymTable oldState)
                                 (head $ getStack oldState)
                                 TypeSym
                                 SutTypeVoid
                                 (TypeDef t)
                                 [tk]
        put $ oldState { getSymTable = newSymTable }

insertFunction :: SutToken -> SutParserM ()
insertFunction tk =
  let what = "Function"
  in  do
        oldState <- get
        let newCurScope = 1 + getCurScope oldState
            oldStack    = getStack oldState
            newSymTable = insert (getSymTable oldState)
                                 (head oldStack)
                                 FunctionSym
                                 SutTypeVoid
                                 NoOther
                                 [tk]
            newSet   = Set.insert newCurScope (getSet oldState)
            newStack = newCurScope : oldStack
        checkToBeNew what newCurScope [tk]
        put $ oldState { getSymTable = newSymTable
                       , getStack    = newStack
                       , getIdScope  = newCurScope
                       , getSet      = newSet
                       }

insertFunctionParams :: [SutParamTk] -> SutParserM ()
insertFunctionParams ps =
  let what = "Parameter"
  in  do
        oldState <- get
        let newSymTable = insertParam (getSymTable oldState)
                                      (head $ getStack oldState)
                                      ParamSym
                                      ps
        checkRepeated what (map thd ps)
          >>= checkAlreadyDefined what (getCurScope oldState)
        put $ oldState { getSymTable = newSymTable }
  where thd (_, _, a) = a

modifyFunction :: SutID -> SutBlock -> [SutParamTk] -> SutType -> SutParserM ()
modifyFunction id funBlock ps t = do
  oldState <- get
  let
    oldSymbols = lookupId (getSymTable oldState) id
    oldSymbol  = head oldSymbols
    newSymbol  = Symbol (getSymToken oldSymbol)
                        FunctionSym
                        (getScope oldSymbol)
                        t
                        (FunctionAST funBlock ps)
    newSymbols  = map (\x -> if x /= oldSymbol then x else newSymbol) oldSymbols
    newSymTable = SymTable $ updateSymList (getHash $ getSymTable oldState)
                                           (getSymToken oldSymbol)
                                           newSymbols
  put $ oldState { getSymTable = newSymTable }



-- Checks
---------------------------------------------------------------------------------------------------
checkRepeated :: String -> [SutToken] -> SutParserM [SutToken]
checkRepeated what tks = do
  let rep = repeatedBy gse detokenize tks
      unq = List.nubBy (eqf detokenize) tks
  unless (null rep) $ mapM_ logListError rep
  return unq
 where
  logListError = logAlreadyDefined "declaration" what
  gse          = getString . getToken
  eqf :: (Eq b) => (a -> b) -> a -> a -> Bool
  eqf f a b = f a == f b
  detokenize (SutToken a b) = getString b

checkAlreadyDefined :: String -> Scope -> [SutToken] -> SutParserM ()
checkAlreadyDefined what scope tks = do
  oldState <- get
  let ids     = map (getString . getToken) tks
      setsIn  = lookupsInScope (getSymTable oldState) scope tks
      someIn  = not $ all null setsIn
      thoseIn = filter (null . snd) $ zip tks setsIn
  when someIn $ mapM_ (logScopeError . fst) thoseIn
  where logScopeError = logAlreadyDefined "scope" what

checkToBeNew :: String -> Scope -> [SutToken] -> SutParserM ()
checkToBeNew what scope tks =
  checkRepeated what tks >>= checkAlreadyDefined what scope

checkToBeNewNow :: String -> [SutToken] -> SutParserM ()
checkToBeNewNow what tks =
  get >>= \st -> checkToBeNew what (getCurScope st) tks

checkId :: SutToken -> SymCategory -> SutType -> SutParserM (Maybe Symbol)
checkId tk cat t = do
  state <- get
  let
    symbols      = lookupId (getSymTable state) $ getString $ getToken tk
    activeScopes = getSet state
    good =
      filter (allf [isInScopes activeScopes, isThisCat, isThisType]) symbols
  when (null good) $ logNotDefined (showSut cat) tk
  return $ if null good then Nothing else Just $ head good
 where
  isInScopes scopes sym = Set.member (getScope sym) scopes
  isThisCat sym = getCategory sym == cat
  isThisType sym = getType sym == t

checkId' :: SutToken -> SymCategory -> SutParserM (Maybe Symbol)
checkId' tk cat = do
  state <- get
  let symbols      = lookupId (getSymTable state) $ getString $ getToken tk
      activeScopes = getSet state
      good         = filter (allf [isInScopes activeScopes, isThisCat]) symbols
  when (null good) $ logNotDefined (showSut cat) tk
  return $ if null good then Nothing else Just $ head good
 where
  isInScopes scopes sym = Set.member (getScope sym) scopes
  isThisCat sym = getCategory sym == cat

checkIsPointer :: SutToken -> SymCategory -> SutParserM (Maybe Symbol)
checkIsPointer tk cat = do
  symbol <- checkId' tk cat
  return $ if isJust symbol && (isPointer . getType . fromJust) symbol
    then symbol
    else Nothing
 where
  isPointer (SutTypePointer _) = True
  isPointer _                  = False

checkNewVars :: SutType -> [(SutToken, SutType)] -> SutParserM ()
checkNewVars t ts = do
  let badMatches = filter ((/= t) . snd) ts
      badTypes   = not $ null badMatches
  when badTypes $ mapM_ (uncurry $ logBadVarType t) badMatches

checkParams :: Symbol -> [SutParamTk] -> SutParserM ()
checkParams fs args = do
  let actualTypes = map (\(_, t, _) -> t) args
      formalTypes = getParams $ getOther fs
      matches     = zip formalTypes actualTypes
      badMatches  = filter (\((_, tp, _), t) -> tp /= t) matches
      badNumber   = length actualTypes /= length formalTypes
      badTypes    = not $ null badMatches
  when badNumber $ logBadParamNumber (getSymToken fs)
                                     (length formalTypes)
                                     (length actualTypes)
  when badTypes $ mapM_ (uncurry $ logBadParamType (getSymToken fs)) badMatches

checkIndexType :: SutToken -> SutType -> SutParserM ()
checkIndexType tk t = do
  let t' = typesLCA SutTypeInt t
  unless (t' == SutTypeInt) $ logIndexNeedsInt tk t

checkConditionalType :: SutToken -> SutType -> SutParserM ()
checkConditionalType tk t = do
  let t' = toTypeBool t
  unless (t' == SutTypeBool) $ logConditionalNeedsBool tk t



-- Parser Actions
-- ------------------------------------------------------------------------------------------------
variableDeclaration vtk te list = do
  checkId' vtk PersonSym
  checkNewVars
    te
    (map (\(tk, Just e) -> (tk, getExpressionType e))
         (filter (isJust . snd) list)
    )
  insertVars te (map fst list)
  return (map mapAssigns (filter (isJust . snd) list))
 where
  mapAssigns (_, Just e) = SutInstExpression (axp e)
  axp = SutBinaryOp te SutOpAssign $ SutExprID te (getString $ getToken vtk)




-- Error logs
---------------------------------------------------------------------------------------------------
logAlreadyDefined :: String -> String -> SutToken -> SutParserM ()
logAlreadyDefined place thing (SutToken pos tkc) =
  tell
    $  SutParserLog
    $  "\n"
    ++ showSut pos
    ++ ": error: "
    ++ thing
    ++ " '"
    ++ getString tkc
    ++ "' already defined in the same "
    ++ place
    ++ "."

logNotDefined :: String -> SutToken -> SutParserM ()
logNotDefined thing (SutToken pos tkc) =
  tell
    $  SutParserLog
    $  "\n"
    ++ showSut pos
    ++ ": error: "
    ++ thing
    ++ " '"
    ++ getString tkc
    ++ "' not defined in any active scope."

logBadParamNumber :: SutToken -> Int -> Int -> SutParserM ()
logBadParamNumber (SutToken pos tkc) formal actual =
  let howmany = if actual > formal then "more" else "fewer"
  in  tell
      $  SutParserLog
      $  "\n"
      ++ showSut pos
      ++ ": error: "
      ++ "Function '"
      ++ getString tkc
      ++ "' needs "
      ++ howmany
      ++ " arguments ("
      ++ show formal
      ++ " instead of "
      ++ show actual
      ++ ")."

logBadVarType :: SutType -> SutToken -> SutType -> SutParserM ()
logBadVarType t1 (SutToken pos tkc) t2 =
  tell
    $  SutParserLog
    $  "\n"
    ++ showSut pos
    ++ ": error: "
    ++ "Declaration for variable '"
    ++ getString tkc
    ++ "' is of type (1), but value is of type (2)"
    ++ "\n\t(1)\t"
    ++ showSut t1
    ++ "\n\t(2)\t"
    ++ showSut t2

logBadParamType :: SutToken -> SutParamTk -> SutType -> SutParserM ()
logBadParamType (SutToken pos1 fkc) (ref, t, SutToken _ tkc) t' =
  tell
    $  SutParserLog
    $  "\n"
    ++ showSut pos1
    ++ ": error: "
    ++ "Function '"
    ++ getString fkc
    ++ "' parameter '"
    ++ getString tkc
    ++ "' has type (1) (you used (2))"
    ++ "\n\t(1)\t"
    ++ showSut t
    ++ "\n\t(2)\t"
    ++ showSut t'

logIndexNeedsInt :: SutToken -> SutType -> SutParserM ()
logIndexNeedsInt (SutToken pos _) t =
  tell
    $  SutParserLog
    $  "\n"
    ++ showSut pos
    ++ ": error: "
    ++ "Indexes have to be integers. Type (1) is not convertible to int."
    ++ "\n\t(1)\t"
    ++ showSut t

logConditionalNeedsBool :: SutToken -> SutType -> SutParserM ()
logConditionalNeedsBool (SutToken pos _) t =
  tell
    $  SutParserLog
    $  "\n"
    ++ showSut pos
    ++ ": error: "
    ++ "Conditionals for flow control need to be booleans. Type (1) is not convertible to bool."
    ++ "\n\t(1)\t"
    ++ showSut t
