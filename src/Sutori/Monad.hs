module Sutori.Monad where

import Data.Word (Word8)
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

import Sutori.Types(SutTypeID, SutPrimitive(SutTypeVoid), primitives, TypeGraph, initialTypeGraphState)
import Sutori.SymTable(SymTable, Scope, SutSymCategory(CatType), SutSymOther(SymTypeDef), insert)
import Sutori.Lexer.Posn(SutPosn, initialPosn)
import Sutori.Logger(SutLogger)


-- Different errors the sutori compiler can turn up with
data SutErrorCode = SutNoError | SutErrorLexer | SutErrorParser | SutError



-- Monadic Lexer/Parser current state.
data SutState = SutState {
  lexerPosn       :: !SutPosn,     -- position at current input location
  lexerInput      :: String,       -- the current input
  lexerChar       :: !Char,        -- the character before the input
  lexerStateCode  :: !Int,         -- the current startcode
  lexerBytes      :: ![Word8],     -- the current bytes read
  lexerDepth      :: Int,
  lexerString     :: String,
  lexerStringOn   :: Bool,
  parserTable     :: SymTable,      -- The symtable
  parserStack     :: [Scope],       -- The scopes stack
  parserScopes    :: Set.Set Scope, -- The set of open scopes
  parserNextScope :: Scope,         -- The next scope ID to open
  typesGraph      :: TypeGraph,     -- The constructed type graph
  typesNextID     :: SutTypeID      -- The next type ID to be introduced
}
initialSutoriState = SutState {
  lexerPosn       = initialPosn,
  lexerInput      = "",
  lexerChar       = '\n',
  lexerBytes      = [],
  lexerStateCode  = 0,
  lexerDepth      = 0,
  lexerString     = "",
  lexerStringOn   = False,
  parserTable     = Map.empty,
  parserStack     = [0],
  parserScopes    = Set.insert 0 Set.empty,
  parserNextScope = 0,
  typesGraph      = fst initialTypeGraphState,
  typesNextID     = snd initialTypeGraphState
}

-- Sutori monad: Composes state and logging
type SutMonad a = StateT SutState (WriterT SutLogger (Either SutErrorCode)) a

-- sutoriError :: String -> SutMonad a
-- sutoriError message = SutMonad $ const $ Left message


-- getTuple f a = getTuple $ getRight $ runMonad f a `catchError` parseError
--     where getRight (Right x) = x
--           getTuple((ast,st),log) = (ast,st,log)
--           parseError (SutLogger pos) = error $ "\nError in line "++show pos


-- Regular Actions
---------------------------------------------------------------------------------------------------
runSutMonad :: SutMonad a -> SutState -> Either SutErrorCode ((a, SutState), SutLogger)
runSutMonad f a = runWriterT $ runStateT f a

-- Inserts a new scope into the parsing
insertScope :: SutMonad ()
insertScope = do
  oldState@SutState{parserNextScope = newScope, parserScopes = scopes, parserStack = stack} <- get
  let newSet = Set.insert newScope scopes
      newStack = newScope : stack
  put $ oldState { parserStack = newStack, parserNextScope = newScope + 1, parserScopes = newSet}

-- Removes last scope from the parsing
removeScope :: SutMonad ()
removeScope = do
  oldState@SutState{parserStack = stack, parserScopes = scopes} <- get
  let newSet = Set.delete (head stack) scopes
      newStack = tail stack
  put $ oldState { parserStack = newStack, parserScopes = newSet}

-- Get the current open scope
parserCurrentScope :: SutState -> Scope
parserCurrentScope = head . parserStack



-- -- Insertion and modification of symbols
--
-- ---------------------------------------------------------------------------------------------------
-- insertInitial :: SutMonad ()
-- insertInitial = mapM_ insertType primitiveTypes
--
-- insertType :: (SutID, SutType) -> SutMonad ()
-- insertType (id, t) = do
--   oldState <- get
--   let curScope = getCurrentScope oldState
--       oldTable = parserTable oldState
--       newTable = insert oldTable curScope CatType SutTypeVoid (SymTypeDef t) [id]
--   put $ oldState { parserTable = newTable }
--
--
-- insertVars :: SutType -> [SutToken] -> SutMonad ()
-- insertVars t tks = let what = "Variable" in do
--     oldState <- get
--     let curScope = getCurScope oldState
--         newSymTable = insert (getSymTable oldState) curScope VarSym t NoOther tks
--     checkToBeNew what curScope tks
--     put $ oldState { getSymTable = newSymTable }
--
-- insertPerson :: [SutToken] -> SutMonad ()
-- insertPerson tks = let what = "Person" in do
--     oldState <- get
--     let curScope = getCurScope oldState
--         newSymTable = insert (getSymTable oldState) curScope PersonSym SutTypeVoid NoOther tks
--     checkToBeNew what curScope tks
--     put $ oldState { getSymTable = newSymTable }
--
-- insertType :: SutToken -> SutType -> SutMonad ()
-- insertType tk t = let what = "Type" in do
--   in  do
--         oldState <- get
--         let curScope = getCurScope oldState
--             newSymTable =
--               insert (getSymTable oldState) curScope VarSym t NoOther tks
--         checkToBeNew what curScope tks
--         put $ oldState { getSymTable = newSymTable }
--
-- insertPerson :: [SutToken] -> SutMonad ()
-- insertPerson tks =
--   let what = "Person"
--   in  do
--         oldState <- get
--         let curScope    = getCurScope oldState
--             newSymTable = insert (getSymTable oldState)
--                                  curScope
--                                  PersonSym
--                                  SutTypeVoid
--                                  NoOther
--                                  tks
--         checkToBeNew what curScope tks
--         put $ oldState { getSymTable = newSymTable }
--
-- insertType :: SutToken -> SutType -> SutMonad ()
-- insertType tk t =
--   let what = "Type"
--   in  do
--         oldState <- get
--         checkRepeated what [tk]
--           >>= checkAlreadyDefined what (head $ getStack oldState)
--         let newSymTable = insert (getSymTable oldState)
--                                  (head $ getStack oldState)
--                                  TypeSym
--                                  SutTypeVoid
--                                  (TypeDef t)
--                                  [tk]
--         put $ oldState { getSymTable = newSymTable }
--
-- insertFunction :: SutToken -> SutMonad ()
-- insertFunction tk =
--   let what = "Function"
--   in  do
--         oldState <- get
--         let newCurScope = 1 + getCurScope oldState
--             oldStack    = getStack oldState
--             newSymTable = insert (getSymTable oldState)
--                                  (head oldStack)
--                                  FunctionSym
--                                  SutTypeVoid
--                                  NoOther
--                                  [tk]
--             newSet   = Set.insert newCurScope (getSet oldState)
--             newStack = newCurScope : oldStack
--         checkToBeNew what newCurScope [tk]
--         put $ oldState { getSymTable = newSymTable
--                        , getStack    = newStack
--                        , getIdScope  = newCurScope
--                        , getSet      = newSet
--                        }
--
-- insertFunctionParams :: [SutParamTk] -> SutMonad ()
-- insertFunctionParams ps =
--   let what = "Parameter"
--   in  do
--         oldState <- get
--         let newSymTable = insertParam (getSymTable oldState)
--                                       (head $ getStack oldState)
--                                       ParamSym
--                                       ps
--         checkRepeated what (map thd ps)
--           >>= checkAlreadyDefined what (getCurScope oldState)
--         put $ oldState { getSymTable = newSymTable }
--   where thd (_, _, a) = a
--
-- modifyFunction :: SutID -> SutBlock -> [SutParamTk] -> SutType -> SutMonad ()
-- modifyFunction id funBlock ps t = do
--   oldState <- get
--   let
--     oldSymbols = lookupId (getSymTable oldState) id
--     oldSymbol  = head oldSymbols
--     newSymbol  = Symbol (getSymToken oldSymbol)
--                         FunctionSym
--                         (getScope oldSymbol)
--                         t
--                         (FunctionAST funBlock ps)
--     newSymbols  = map (\x -> if x /= oldSymbol then x else newSymbol) oldSymbols
--     newSymTable = SymTable $ updateSymList (getHash $ getSymTable oldState)
--                                            (getSymToken oldSymbol)
--                                            newSymbols
--   put $ oldState { getSymTable = newSymTable }
--
--
--
-- -- Checks
-- ---------------------------------------------------------------------------------------------------
-- checkRepeated :: String -> [SutToken] -> SutMonad [SutToken]
-- checkRepeated what tks = do
--   let rep = repeatedBy gse detokenize tks
--       unq = List.nubBy (eqf detokenize) tks
--   unless (null rep) $ mapM_ logListError rep
--   return unq
--  where
--   logListError = logAlreadyDefined "declaration" what
--   gse          = getString . getToken
--   eqf :: (Eq b) => (a -> b) -> a -> a -> Bool
--   eqf f a b = f a == f b
--   detokenize (SutToken a b) = getString b
--
-- checkAlreadyDefined :: String -> Scope -> [SutToken] -> SutMonad ()
-- checkAlreadyDefined what scope tks = do
--   oldState <- get
--   let ids     = map (getString . getToken) tks
--       setsIn  = lookupsInScope (getSymTable oldState) scope tks
--       someIn  = not $ all null setsIn
--       thoseIn = filter (null . snd) $ zip tks setsIn
--   when someIn $ mapM_ (logScopeError . fst) thoseIn
--   where logScopeError = logAlreadyDefined "scope" what
--
-- checkToBeNew :: String -> Scope -> [SutToken] -> SutMonad ()
-- checkToBeNew what scope tks =
--   checkRepeated what tks >>= checkAlreadyDefined what scope
--
-- checkToBeNewNow :: String -> [SutToken] -> SutMonad ()
-- checkToBeNewNow what tks =
--   get >>= \st -> checkToBeNew what (getCurScope st) tks
--
-- checkId :: SutToken -> SymCategory -> SutType -> SutMonad (Maybe Symbol)
-- checkId tk cat t = do
--   state <- get
--   let
--     symbols      = lookupId (getSymTable state) $ getString $ getToken tk
--     activeScopes = getSet state
--     good =
--       filter (allf [isInScopes activeScopes, isThisCat, isThisType]) symbols
--   when (null good) $ logNotDefined (showSut cat) tk
--   return $ if null good then Nothing else Just $ head good
--  where
--   isInScopes scopes sym = Set.member (getScope sym) scopes
--   isThisCat sym = getCategory sym == cat
--   isThisType sym = getType sym == t
--
-- checkId' :: SutToken -> SymCategory -> SutMonad (Maybe Symbol)
-- checkId' tk cat = do
--   state <- get
--   let symbols      = lookupId (getSymTable state) $ getString $ getToken tk
--       activeScopes = getSet state
--       good         = filter (allf [isInScopes activeScopes, isThisCat]) symbols
--   when (null good) $ logNotDefined (showSut cat) tk
--   return $ if null good then Nothing else Just $ head good
--  where
--   isInScopes scopes sym = Set.member (getScope sym) scopes
--   isThisCat sym = getCategory sym == cat
--
-- checkIsPointer :: SutToken -> SymCategory -> SutMonad (Maybe Symbol)
-- checkIsPointer tk cat = do
--   symbol <- checkId' tk cat
--   return $ if isJust symbol && (isPointer . getType . fromJust) symbol
--     then symbol
--     else Nothing
--   "\n\t(2)\t"++showSut t2
--
-- logBadParamType :: SutToken -> SutParamTk -> SutType -> SutMonad ()
-- logBadParamType (SutToken pos1 fkc) (ref, t, SutToken _ tkc) t' = tell $ SutLogger $
--   "\n"++showSut pos1++": error: "++"Function '"++getString fkc++"' parameter '"++getString tkc++"' has type (1) (you used (2))"++
--   "\n\t(1)\t"++showSut t++
--   "\n\t(2)\t"++showSut t'
--
-- logIndexNeedsInt :: SutToken -> SutType -> SutMonad ()
-- logIndexNeedsInt (SutToken pos _) t = tell $ SutLogger $
--   "\n"++showSut pos++": error: "++"Indexes have to be integers. Type (1) is not convertible to int."++
--   "\n\t(1)\t"++showSut t
--
-- logConditionalNeedsBool :: SutToken -> SutType -> SutMonad ()
-- logConditionalNeedsBool (SutToken pos _) t = tell $ SutLogger $
--   "\n"++showSut pos++": error: "++"Conditionals for flow control need to be booleans. Type (1) is not convertible to bool."++
--   "\n\t(1)\t"++showSut t
