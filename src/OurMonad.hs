module OurMonad where
import Data.Maybe 
import Data.List
import Control.Monad.Except
import Data.Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Zip
import AST 


type Stack = [Int]
type Scope = Int
data Category = Module | Function | Person | Var | Param | Type  deriving (Eq,Show) 
data Symbol = Symbol {getId::String, getCategory::Category, getScope::Scope, getType::(Maybe Type), getOther::(Maybe Int)}
data SymTable = SymTable {getHash::Map.Map String [Symbol]}
data OurState = OurState {getSymTable::SymTable, getStack::Stack, getIdScope :: Int, getSet :: Set.Set Int, getError :: [String]}
data OurError = OurError Int

data OurLog = OurLog {getErrorsLog::String}

instance Monoid OurLog where
  mempty = OurLog ""
  mappend (OurLog a) (OurLog b) = OurLog (a++b)

instance Show OurLog where
  show (OurLog a) = "Errores:\n"++a++"\n"

type OurMonad a = StateT OurState (WriterT OurLog (Either OurError)) a

runOurMonad :: OurMonad a -> OurState -> Either OurError ((a, OurState), OurLog)
runOurMonad f a = runWriterT $ runStateT f a

emptyState = OurState (SymTable Map.empty) [1] 1 (Set.insert 1 Set.empty) []

getTuple f a = getTuple $ getRight $ runOurMonad f a `catchError` (\(OurError pos) -> error $ "\nError en linea "++show pos++"\n")
    where
        getRight (Right x) = x
        getTuple((ast,st),log) = (ast,st,log)

extract Nothing = []
extract (Just a) = a

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = filter (p . length) . group . sort

repeated :: Ord a => [a] -> [a]
repeated = map head . filterByLength (>1)

addToSymTable :: Declaration -> OurMonad Declaration
addToSymTable (VDT e t (LDV l)) = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        aScope = head $ getStack oldState 
        oldHash = getHash oldSymTable
        sl = map (\(s,_) -> s) l
        newl = nub sl
        newSymTable = SymTable $ foldl (addToMap aScope) oldHash l
        inSymTable = mapM (lookVarInSymTableInScope aScope) newl
        isFalse = fmap (zip newl) inSymTable >>= return.filter (\(_,x) -> x==True)
    notGood <- or <$> inSymTable
    isFalseL <- isFalse
    when (length l /= length newl) $ 
        mapM_ (\vs -> tell $ OurLog $ "Variable '"++vs++"' definida dos veces en la misma lista de declaraci'on de variables.\n") (repeated sl)
    when (notGood) $ 
        mapM_ (\(vs,_) -> tell $ OurLog $ "Variable '"++vs++"' definida dos veces en el mismo Scope.\n") isFalseL
    put $ oldState { getSymTable = newSymTable }
    return (VDT e t (LDV l))
    where addToMap aScope mp (s,_) = let newSymbol = Symbol s Var aScope (Just t) Nothing
                                         newList = newSymbol:(extract $ Map.lookup s mp)
                                         newMap = Map.insert s newList mp
                                     in newMap


addToSymTable (PDT (LPD l)) = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        aScope = head $ getStack oldState 
        oldHash = getHash oldSymTable
        newSymTable = SymTable $ foldl (addToMap aScope) oldHash l
        newl = nub l
        inSymTable = mapM (lookVarInSymTableInScope aScope) newl
        isFalse = fmap (zip newl) inSymTable >>= return.filter (\(_,x) -> x==True)
    notGood <- or <$> inSymTable
    isFalseL <- isFalse
    when (length l /= length newl) $ 
        mapM_ (\vs -> tell $ OurLog $ "Persona '"++vs++"' definida dos veces en la misma lista de declaraci'on de Personas.\n") (repeated l)
    when (notGood) $ 
        mapM_ (\(vs,_) -> tell $ OurLog $ "Persona '"++vs++"' definida dos veces en el mismo Scope.\n") isFalseL
    put $ oldState { getSymTable = newSymTable }
    return (PDT (LPD l))
    where addToMap aScope mp s = let newSymbol = Symbol s Person aScope Nothing Nothing
                                     newList = newSymbol:(extract $ Map.lookup s mp)
                                     newMap = Map.insert s newList mp
                                 in newMap

lookVarInSymTableInScope :: Int -> String -> OurMonad Bool 
lookVarInSymTableInScope sc s = do 
    oldState <- get  
    let hash = getHash.getSymTable $ oldState
        symb = map getScope (extract $ Map.lookup s hash)
        xs = filter (==sc) symb 
        ans = case xs of 
            [] -> False
            _ -> True
    return ans


{-}



addFuncToSymTable :: String -> Type -> [(Type,String,Int)] -> OurMonad ()
addFuncToSymTable s t l = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        oldHash = getHash oldSymTable
        oldStack = getStack oldState
        idScope = getIdScope oldState
        aScope = idScope + 1 
        funcSymbol = Symbol s Function (head $ getStack oldState) (Just t) (Just $ length l)
        newL = funcSymbol:(extract $ Map.lookup s oldHash)
        hashM = Map.insert s newL oldHash
        newSymTable = SymTable $ foldl (addToMap aScope) hashM l
        newSet = Set.insert aScope (getSet oldState)
        newStack = aScope:oldStack
    put $ oldState { getSymTable = newSymTable, getStack = newStack, getIdScope = aScope, getSet = newSet}
    where addToMap aScope mp (tf,sf,f) = let newSymbol = Symbol sf Param aScope (Just tf) (Just f)
                                             newList = newSymbol:(extract $ Map.lookup sf mp)
                                             newMap = Map.insert sf newList mp
                                         in newMap 

lookPersonInSymTable :: String -> OurMonad Bool 
lookPersonInSymTable s = do 
    oldState <- get  
    let hash = getHash.getSymTable $ oldState
        symb = map getCategory (extract $ Map.lookup s hash)
        xs = filter (==Person) symb 
        ans = case xs of 
            [] -> False
            _ -> True
    return ans

lookVarInSymTable :: String -> OurMonad Bool 
lookVarInSymTable s = do 
    oldState <- get  
    let hash = getHash.getSymTable $ oldState
        symb = map getCategory (extract $ Map.lookup s hash)
        xs = filter (==Var) symb 
        ans = case xs of 
            [] -> False
            _ -> True
    return ans

lookFunctionInSymTable :: String -> OurMonad Bool 
lookFunctionInSymTable s = do 
    oldState <- get  
    let hash = getHash.getSymTable $ oldState
        symb = map getCategory (extract $ Map.lookup s hash)
        xs = filter (==Function) symb 
        ans = case xs of 
            [] -> False
            _ -> True
    return ans

removeLastScope :: OurMonad ()
removeLastScope = do
    oldState <- get
    let oldStack = getStack oldState
        newStack = tail oldStack
        newSet = Set.delete (newStack !! 0) (getSet oldState)
    put $ oldState { getStack = newStack, getSet = newSet }

    -}