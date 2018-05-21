module OurMonad where
import Data.Maybe 
import qualified Data.Map as Map
import Control.Monad.State
import AST 

type Stack = [Int]
type Scope = Int
data Category = Module | Function | Person | Var | Param | Type  deriving (Eq,Show) 
data Symbol = Symbol {getId::String, getCategory::Category, getScope::Scope, getType::(Maybe Type), getOther::(Maybe Int)}
data SymTable = SymTable {getHash::Map.Map String [Symbol]}
data OurState = OurState {getSymTable::SymTable, getStack::Stack, getIdScope :: Int, getActualScope :: Int}
data OurError = OurError Int

type OurMonad a = StateT OurState (Either OurError) a

runOurMonad :: OurMonad a -> OurState -> Either OurError (a, OurState)
runOurMonad f a = runStateT f a

extract Nothing = []
extract (Just a) = a

addToSymTable :: Declaration -> OurMonad ()
addToSymTable (VDT _ t (LDV l)) = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        aScope = getActualScope oldState 
        oldHash = getHash oldSymTable
        newSymTable = SymTable $ foldl (addToMap aScope) oldHash l 
    put $ oldState { getSymTable = newSymTable }
    where addToMap aScope mp (s,_) = let newSymbol = Symbol s Var aScope (Just t) Nothing
                                         newList = newSymbol:(extract $ Map.lookup s mp)
                                         newMap = Map.insert s newList mp
                                     in newMap

addToSymTable (PDT (LPD l)) = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        aScope = getActualScope oldState 
        oldHash = getHash oldSymTable
        newSymTable = SymTable $ foldl (addToMap aScope) oldHash l 
    put $ oldState { getSymTable = newSymTable }
    where addToMap aScope mp s = let newSymbol = Symbol s Person aScope Nothing Nothing
                                     newList = newSymbol:(extract $ Map.lookup s mp)
                                     newMap = Map.insert s newList mp
                                 in newMap

addFuncToSymTable :: String -> Type -> [(Type,String,Int)] -> OurMonad ()
addFuncToSymTable s t l = do
    oldState <- get
    let oldSymTable = getSymTable oldState
        oldHash = getHash oldSymTable
        oldStack = getStack oldState
        idScope = getIdScope oldState
        aScope = idScope + 1 
        funcSymbol = Symbol s Function (getIdScope oldState) (Just t) Nothing
        newL = funcSymbol:(extract $ Map.lookup s oldHash)
        hashM =  Map.insert s newL oldHash
        newSymTable = SymTable $ foldl (addToMap aScope) hashM l
        newStack = aScope:oldStack
    put $ oldState { getSymTable = newSymTable, getStack = newStack, getIdScope = aScope, getActualScope = aScope }
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
        newActual = newStack !! 0
    put $ oldState { getStack = newStack, getActualScope = newActual}