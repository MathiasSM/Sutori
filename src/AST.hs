module AST where
import Control.Monad


data ConstrN = --Construcciones
    PN ConstrN                                      | 
    BN ConstrN                                      |
    BCN {listBCN :: [SN]}                           |
    STN {listSTN :: [(TypeN,String)]}               |
    UTN {listUTN :: [(TypeN,String)]}               |
    LCAN {listLCA :: [ExpN] }                       |
    LCSN {listLCSN :: [(String,ExpN)] }             |
    LDPN {listLDPN :: [String]}                     |
    FPN  {listFPN :: [ExpN]}                        |
    LVN {listLVN :: [(String,ExpN)]}
    deriving Show

data SN = 
    FuncDefN                |
    PDN ConstrN             |
    VDN String ConstrN      |
    InstrucN 
    deriving Show

data TypeN = 
    IntNumN             |
    FloatNumN           |
    CharcN              |
    BooleanN            |
    ArrayN ExpN TypeN   |
    StructN ConstrN     |
    UnionN ConstrN      |
    PointerN TypeN      
    deriving Show

data ExpN = 
    IdN { getString::String, getPos::(Int, Int) }                                   | --Identificador
    TrueN { getPos::(Int, Int) }                                                    | --True
    FalseN { getPos::(Int, Int) }                                                   | --False
    ParentExp  { getExp::ExpN, getPos::(Int, Int) }                                      | --Parentesis
    CompareN { getExp::ExpN, getString::String, getExp1::ExpN, getPos::(Int, Int) }  | --Operacion de comparacion
    NotN { getExp::ExpN, getPos::(Int, Int) }                                       | --Operacion de negacion
    LogicN  { getExp::ExpN, getString::String, getExp1::ExpN, getPos::(Int, Int) }  | --Operacion logica
    FCN  { getString::String, getParamList::ConstrN, getPos::(Int, Int) }          | --Llamado de funcion
    MinusN { getExp::ExpN, getPos::(Int, Int) }                                     | --Operacion unaria de negacion
    BinaryN { getExp::ExpN, getString::String, getExp1::ExpN, getPos::(Int, Int) }    | --Operacion aritmetica
    IntegerLiteralN  { getInteger::Integer, getPos::(Int, Int) }                      |   --Literal numerico 
    FloatLiteralN  { getFloat::Float, getPos::(Int, Int) }                      |   --Literal numerico 
    CharLiteralN  { getString::String, getPos::(Int, Int) }                      |   --Literal numerico 
    StringLiteralN  { getString::String, getPos::(Int, Int) }                      |   --Literal numerico 
    CN ConstrN                                                                      |
    DeferenceN { getExp::ExpN, getPos::(Int, Int) }                                     

    deriving Show

data FuncDefN = --Definicion de funcion
    DFN String TypeN FBN (Int,Int)      |
    PDFN String TypeN ParamList FBN (Int,Int)       
    deriving Show

data FBN = 
    FBCN {listFBCN :: [FuncBlock]} 
    deriving Show
data FuncBlock = 
    SN |
    REN ExpN
    deriving Show

data ParamList = --Lista de parametros
    LPN {listLPN :: [(TypeN,String,Int)]} 
    deriving Show        


data InstrN = --Instrucciones
    GetPropN  ExpN String (Int, Int)    |
    ExprN ExpN                                                          |
    AssignN String ExpN (Int, Int)     |
    IfThenN String ConstrN ExpN (Int, Int) |
    IfThenElseN String ConstrN ExpN ConstrN (Int, Int) |
    WhileN String ExpN ConstrN (Int,Int) |
    ForN ConstrN String ExpN (Int,Int) |
    CreatePointerN String TypeN (Int,Int)|
    FreePointerN String String (Int,Int)
    deriving Show
ident = "|  "

putStrWithIdent n s = (replicateM_ n $ putStr ident) >> putStr s

putStrLnWithIdent n s = (replicateM_ n $ putStr ident) >> putStrLn s

printId n s = putStrLnWithIdent n $ "Identificador: " ++ s

printExpN :: Int -> ExpN -> IO()
printExpN n (IdN s _) = do
    printId n s

printExpN n (TrueN _) = do
    putStrLnWithIdent n "Literal booleano: on"
     
printExpN n (FalseN _) = do
    putStrLnWithIdent n "Literal booleano: off"

printExpN n (ParentExp exp _) = do
    putStrLnWithIdent n "Expresion entre parentesis:"
    printExpN (n+1) exp

printExpN n (CompareN exp s exp1 _) = do
    putStrLnWithIdent n "Operacion de comparacion:"
    putStrLnWithIdent (n+1) $ "Comparador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExpN (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExpN (n+2) exp1


printTypeN :: Int -> TypeN -> IO()
printTypeN n (BooleanN) = do
    putStrLnWithIdent n "Type : boolean"
     
printTypeN n (IntNumN) = do
    putStrLnWithIdent n "Type : Integer"

printTypeN n (CharcN) = do
    putStrLnWithIdent n "Type : Character"

printTypeN n (FloatNumN) = do
    putStrLnWithIdent n "Type : Float"


printConstrN :: Int -> ConstrN -> IO()
printConstrN n (PN block) = do
    putStrLnWithIdent n "Sourse of the program:"

