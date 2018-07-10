module Sutori.Monad where

import Data.Maybe
import Data.List
import Data.Semigroup
import Data.Either
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



-- SutParserM Actions {{{1

runSutParserM :: SutParserM a -> SutParserState -> Either SutParserError ((a, SutParserState), SutParserLog)
runSutParserM f a = runWriterT $ runStateT f a


getTuple f a = getTuple $ getRight $ runSutParserM f a `catchError` parseError
    where getRight (Right x) = x
          getTuple((ast,st),log) = (ast,st,log)
          parseError (SutParserError pos) = error $ "\nError in line "++show pos

extract Nothing = []
extract (Just a) = a


filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = filter (p . length) . group . sort

repeated :: Ord a => [a] -> [a]
repeated = map head . filterByLength (>1)

-- Inserts
---------------------------------------------------------------------------------------------------
insertVars :: SutType -> [SutID] -> SutParserM ()
insertVars t ids = do
    oldState <- get
    let nubids = nub ids
        curScope = getCurScope oldState
        newSymTable = insert (getSymTable oldState) curScope VarSym t NoOther
    notGood <- or <$> mapM (lookupVarInScope curScope) nubids
    when (length ids /= length nubids) $ mapM_ logListError (repeated sl) -- ??
    when (notGood) $ mapM_ logScopeError <$> filter <$> fmap (zip nubids) inSymTable snd
    put $ oldState { getSymTable = newSymTable }
    where logScopeError (vs, _)      = logAlreadyDefined "scope" "Variable" vs
          logListError vs            = logAlreadyDefined "declaration" "Variable" vs

insertPerson :: [SutID] -> SutParserM ()
insertPerson ids = do
    oldState <- get
    let curScope = getCurScope oldState
        newSymTable = insert (getSymTable oldState) curScope PersonSym SutTypeVoid NoOther
        nubids = nub ids
    notGood <- or <$> mapM (lookupVarInScope curScope) nubids
    when (length ids /= length nubids) $ mapM_ logListError (repeated sl)
    when (notGood) $ mapM_ logScopeError <$> filter <$> fmap (zip nubids) inSymTable snd
    put $ oldState { getSymTable = newSymTable }
    where logScopeError (vs, _)  = logAlreadyDefined "scope" "Person" vs
          logListError vs        = logAlreadyDefined "declaration" "Person" vs

insertFunction :: SutID -> SutParserM ()
insertFunction id = do
    oldState <- get
    let curScope = 1 + getCurScope oldState
        oldTable = getHash oldSymTable
        newSymTable = insert (getSymTable oldState) (head $ getStack oldState) FunctionSym SutTypeVoid NoOther
        newSet = Set.insert curScope (getSet oldState)
        newStack = curScope:oldStack
    notGood <- lookupVarInScope (head $ getStack oldState) s
    when (notGood) $ logAlreadyDefined "scope" "Function" id
    put $ oldState { getSymTable = newSymTable, getStack = newStack, getIdScope = curScope, getSet = newSet}


insertFunctionParams :: [SutParam] -> SutParserM ()
insertFunctionParams ps = do
    oldState <- get
    let newSymTable = insertToTable oldState addToMap ids
        oldList = map (\(_, pid, _) -> pid) ps
        newList = nub oldList
    notGood <- or <$> mapM (lookupVarInScope curScope) nubids
    when (length ids /= length newList) $ mapM_ logListError (repeated sl)
    when (notGood) $ mapM_ logScopeError <$> filter <$> fmap (zip nubids) inSymTable snd
    updateSymTable oldState newSymTable
    where logScopeError (vs, _)  = logAlreadyDefined "Person" "scope" vs
          logListError vs        = logAlreadyDefined "Person" "declaration" vs
          addToMap curScope mp s = let newSymbol = Symbol s PersonSym curScope Nothing NoOther
                                       newList = newSymbol:(extract $ Map.lookup s mp)
                                    in Map.insert s newList mp
    let oldSymTable = getSymTable oldState
        oldHash = getHash oldSymTable
        curScope = head $ getStack oldState
        newSymTable = SymTable $ foldl (addToMap curScope) oldHash l
        sl = map (\(_,s,_) -> s) l
        newl = nub sl
    when (length l /= length newl) $
        mapM_ (\vs -> tell $ SutParserLog $ "Id '"++vs++"' definido dos veces en la misma lista de parametros de la funci'on\n") (repeated sl)
    updateSymTable oldState newSymTable
    return $ LFDP l
    where addToMap curScope mp (tf,sf,f) = let newSymbol = Symbol sf Param curScope (Just tf) (ReferenceOp f)
                                             newList = newSymbol:(extract $ Map.lookup sf mp)
                                             newMap = Map.insert sf newList mp
                                         in newMap

insertType :: Declaration -> String -> SutParserM Declaration
insertType TDT s = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        curScope = head $ getStack oldState
        oldHash = getHash oldSymTable
        newSymbol = Symbol s TypeSym curScope Nothing NoOther
        newList = newSymbol:(extract $ Map.lookup s oldHash)
        newSymTable = SymTable $ Map.insert s newList oldHash
    updateSymTable oldState newSymTable
    return TDT







lookupVarInScope :: Int -> String -> SutParserM Bool
lookupVarInScope sc s = do
    oldState <- get
    let hash = getHash.getSymTable $ oldState
        symb = map getScope (extract $ Map.lookup s hash)
        xs = filter (==sc) symb
        ans = case xs of
            [] -> False
            _ -> True
    return ans


removeLastScope :: SutParserM ()
removeLastScope = do
    oldState <- get
    let oldStack = getStack oldState
        newStack = tail oldStack
        newSet = Set.delete (oldStack !! 0) (getSet oldState)
    put $ oldState { getStack = newStack, getSet = newSet }


checkId :: String -> Category -> SutParserM (Maybe Symbol)
checkId s cat = do
    state <- get
    let hash = getHash $ getSymTable state
        set = getSet state
        listSymb = Map.lookup s hash
        good = filter (\sy -> (Set.member (getScope sy) set) && (getCategory sy == cat))  (extract listSymb)
    when (length good == 0) $
        tell $ SutParserLog $ (show cat)++" '"++s++"' no definido anteriormente\n"
    return $ head' good

head' [] = Nothing
head' (x:xs) = Just x

getType' Nothing = Just TE
getType' (Just s) = getType s

checkType :: Type -> SutParserM ()
checkType (TID s) = do
    state <- get
    let hash = getHash $ getSymTable state
        set = getSet state
        listSymb = Map.lookup s hash
        good = filter (\sy -> (Set.member (getScope sy) set) && (getCategory sy == TypeSym))  (extract listSymb)
    when (length good == 0) $
        tell $ SutParserLog $ "Tipo '"++s++"' no definido anteriormente\n"

checkType _ = return ()

addInstructionScope :: SutParserM ()
addInstructionScope = do
    oldState <- get
    let oldStack = getStack oldState
        idScope = getIdScope oldState
        curScope = idScope + 1
        newSet = Set.insert curScope (getSet oldState)
        newStack = curScope:oldStack
    put $ oldState { getStack = newStack, getIdScope = curScope, getSet = newSet}

modifyFunction :: String -> Lists -> Lists -> Type -> SutParserM ()
modifyFunction s bf ps t = do
    oldSymbol <- checkId s Function
    oldState <- get
    let oldSymTable = getSymTable oldState
        oldHash = getHash oldSymTable
        oldList = extract $ Map.lookup s oldHash
        funcSymbol = Symbol s Function (getScope (fromJust oldSymbol)) (Just t) (FunctionAST bf ps)
        newList = foldr (\x acc -> (if (x/=(fromJust oldSymbol)) then x else funcSymbol):acc ) [] oldList
        newSymTable = SymTable $ Map.insert s newList oldHash
    put $ oldState { getSymTable = newSymTable }

checkParams :: Lists -> Maybe Symbol -> SutParserM ()
checkParams _ Nothing = return ()
checkParams (LFCP l) (Just s) = do
    let actualTypes = map getExpressionType l
        formalTypes = f (getParams $ getOther s)
        list = zip actualTypes formalTypes
        bad = filter (\(x,y) -> x/=y ) list
    when (length actualTypes /= length formalTypes) $
        tell $ SutParserLog $ "N'umero de par'ametros de la funci'on '"++(getId s)++"' incorrecto.\n"
    when (length bad /= 0) $
        tell $ SutParserLog $ "Ti[p de parametro de la funci'on '"++(getId s)++"' incorrecto.\n"
    where f (LFDP l) = map (\(x,_,_) -> x) l


getNumericType :: String -> Expression -> Expression -> SutParserM Type
getNumericType sim e1 e2 = do
    let type1 = getExpressionType e1
        type2 = getExpressionType e2
        finalType = joinTypes type1 type2
    when (finalType == TE) $
        tell $ SutParserLog $ "Operaci'on num'erica "++sim++" no definida para tipos: \n"
    return $ finalType
    where joinTypes TI TI = TI
          joinTypes TI TC = TI
          joinTypes TI TB = TI
          joinTypes TI TF = TF

          joinTypes TC TI = TI
          joinTypes TC TF = TF
          joinTypes TC TB = TI
          joinTypes TC TC = TI

          joinTypes TF TI = TF
          joinTypes TF TC = TF
          joinTypes TF TF = TF
          joinTypes TF TB = TF

          joinTypes TB TB = TI
          joinTypes TB TI = TI
          joinTypes TB TC = TI
          joinTypes TB TF = TF

          joinTypes _ _ = TE

getLogicalType :: String -> Expression -> Expression -> SutParserM Type
getLogicalType sim e1 e2 = do
    let type1 = getExpressionType e1
        type2 = getExpressionType e2
        finalType = joinTypes type1 type2
    when (finalType == TE) $
        tell $ SutParserLog $ "Operaci'on logica"++sim++" no definida para tipos: \n"
    return $ finalType
    where joinTypes TI TI = TB
          joinTypes TI TC = TB
          joinTypes TI TB = TB


          joinTypes TC TI = TB
          joinTypes TC TB = TB
          joinTypes TC TC = TB

          joinTypes TB TB = TB
          joinTypes TB TI = TB
          joinTypes TB TC = TB

          joinTypes _ _ = TE


getComparisonType :: String -> Expression -> Expression -> SutParserM Type
getComparisonType sim e1 e2 = do
    let type1 = getExpressionType e1
        type2 = getExpressionType e2
        finalType = joinTypes type1 type2
    when (finalType == TE) $
        tell $ SutParserLog $ "Operaci'on de comparaci'on "++sim++" no definida para tipos: \n"
    return $ finalType
    where joinTypes TI TI = TB
          joinTypes TI TC = TB
          joinTypes TI TB = TB
          joinTypes TI TF = TB

          joinTypes TC TI = TB
          joinTypes TC TF = TB
          joinTypes TC TB = TB
          joinTypes TC TC = TB

          joinTypes TF TI = TB
          joinTypes TF TC = TB
          joinTypes TF TF = TB
          joinTypes TF TB = TB

          joinTypes TB TB = TB
          joinTypes TB TI = TB
          joinTypes TB TC = TB
          joinTypes TB TF = TB
          joinTypes _ _ = TE

getEqualityType :: String -> Expression -> Expression -> SutParserM Type
getEqualityType sim e1 e2 = do
    let type1 = getExpressionType e1
        type2 = getExpressionType e2
        finalType = joinTypes type1 type2
    when (finalType == TE) $
        tell $ SutParserLog $ "Operaci'on de igualdad "++sim++" no definida para tipos: \n"
    return $ finalType
    where joinTypes TI TC = TB
          joinTypes TI TB = TB
          joinTypes TI TF = TB

          joinTypes TC TI = TB
          joinTypes TC TF = TB
          joinTypes TC TB = TB

          joinTypes TF TI = TB
          joinTypes TF TC = TB
          joinTypes TF TB = TB

          joinTypes TB TI = TB
          joinTypes TB TC = TB
          joinTypes TB TF = TB

          joinTypes TE TE = TE

          joinTypes a b = if (a==b) then TB else TE

checkIndexType :: Expression -> SutParserM ()
checkIndexType e = do
    let t = getExpressionType e
        i = toInt t
    when (i == TE) $
        tell $ SutParserLog $ "Indice no convertible a entero\n"
    where toInt TI = TI
          toInt TC = TI
          toInt TB = TI
          toInt _  = TE

extArrayType :: Expression -> SutParserM Type
extArrayType e = do
    let pt = getExpressionType e
    case pt of
        (TA _ t) -> return t
        TE -> return TE
        _ -> do tell $ SutParserLog $ "Operaci'on de indexaci'on no definida para tipo: \n"
                return TE


getPointerType :: Expression -> SutParserM Type
getPointerType e = do
    let pt = getExpressionType e
    case pt of
        (TP t) -> return t
        TE -> return TE
        _ -> do tell $ SutParserLog $ "Operaci'on de deferencia no definida para tipo: \n"
                return TE




printSymTable :: SutParserState -> IO ()
printSymTable mp = mapM_ print (Map.elems (getHash $ getSymTable mp))


logAlreadyDefined :: SymCategory -> String -> SutID -> SutParserM ()
logAlreadyDefined place thing id = tell $ SutParserLog $ "\n"++printSut thing++" '"++id++"' already defined in the same "++place++"."

logDefinedInScope = logAlreadyDefined "scope"
logDefinedInDeclaration = logAlreadyDefined "declaration"
