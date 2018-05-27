module AST where
import Data.Maybe
import Control.Monad

data Source  = PT String Lists            

data Lists = LST { listLST :: [Statement] }               |
             LPD { listLPD :: [String] }                  |
             LFCP { listLFCP :: [Expression] }            |
             LFBT { listLFBT :: [FuntionBC] }             |
             LFDP { listLFDP :: [(Type,String,Int)] }     |
             LDV { listLDV :: [(String,Maybe Expression)] }     |
             LSRT { listLSRT :: [(Type,String)] }         |         
             LUT { listLUT :: [(Type,String)] }


data Expression = IdT String              |
                  LitT Literal            |
                  ConsT Constructor       |
                  FunCT Operation         |
                  OpT Operation           |
                  PET Expression    


data Literal = IT Int           |
               CT String        |
               FT Float         |
               TBT              |
               FBT              |
               ST String

data Constructor = LCAT { listLCAT :: [Expression] }            |
                   LCST { listLCST :: [(String,Expression)] }  

data Operation = PUT Expression                   |
                 MUT Expression                   |
                 NUT Expression                   | 
                 DUT Expression                   |
                 ABT Expression String Expression |                 
                 LBT Expression String Expression |
                 GAT String Expression            |
                 GPT Expression String            |
                 AT String Expression             |
                 FCAT String                      |
                 FCNT String Lists                 


data Declaration = FDT   |
                   FDAT  |
                   PDT   |
                   VDT                           

data FuntionBC = StaT Statement          |
                 RT Expression 

data Type = TI                           |
            TF                           |
            TC                           |
            TB                           |
            TS                           |
            TA Int Type                  |
            TST Lists                    |
            TU Lists                     |
            TP Type                      |
            TID String   

data Statement = InsT Instruction        |
                 DecT Declaration


data Instruction = IFT String Lists Expression         |
                   IFET String Lists Expression Lists  |
                   UIT String Expression Lists         |
                   BIT Lists String Expression         |
                   CPT String Type                     |
                   FPT String String                   |
                   ExprT Expression                    |
                   PRT String Expression 


ident = "|  "

putStrWithIdent n s = (replicateM_ n $ putStr ident) >> putStr s

putStrLnWithIdent n s = (replicateM_ n $ putStr ident) >> putStrLn s


printId n s = putStrLnWithIdent n $ "Identificador: " ++ s

printExp :: Int -> Expression -> IO()
printExp n (IdT s) = do
    printId n s

printExp n (LitT l) = do
    printLit n l

printExp n (OpT o) = do
    printOperation n o

printExp n (PET e) = do
    putStrLnWithIdent n "Expresion entre parentesis:"
    printExp (n+1) e
       
printDecV :: Int -> (Type,String) -> IO()
printDecV n (t,s) = do 
  printType n t
  printId n s

printDecVE :: Int -> (String,Maybe Expression) -> IO()
printDecVE n (s,Nothing) = printId n s

printDecVE n (s,Just exp) = do 
  printId n s
  printExp n exp

printSource :: Int -> Source -> IO()
printSource n (PT s lblock) = do
    putStrLnWithIdent n "Constructor de Programa:"
    putStrLnWithIdent (n+1) ("Nombre del Modulo: " ++ s) 
    printLists (n+2) lblock

printLists :: Int -> Lists -> IO()
printLists n (LST ls) = do 
    putStrLnWithIdent n "Instrucciones del programa: "
    mapM_ (printStatement (n+1)) ls

             
printLists n (LPD ls) = do 
    putStrLnWithIdent n "Lista de declaracion de personas:"
    mapM_ (printId (n+1)) ls

printLists n (LFCP ls) = do 
    putStrLnWithIdent n "Lista de parametros de la funcion a llamar: "
    mapM_ (printExp (n+1)) ls


printLists n (LFBT ls) = do 
    putStrLnWithIdent n "Lista de instruccones de la funcion: "
    mapM_ (printFuntionBC (n+1)) ls

{-
printLists n (LFDP ls) = do 
    putStrLnWithIdent n "Lista de parametros de la funcion: "
    mapM_ (printParamsF (n+1)) ls -}

printLists n (LDV ls) = do 
    putStrLnWithIdent n "Lista de declaracion de variables: "
    mapM_ (printDecVE (n+1)) ls

printLists n (LSRT ls) = do 
    putStrLnWithIdent n "Lista de declaracion de Struct: "
    mapM_ (printDecV (n+1)) ls


printLists n (LUT ls) = do 
    putStrLnWithIdent n "Lista de declaracion de Union: "
    mapM_ (printDecV (n+1)) ls

printLit :: Int -> Literal -> IO()
printLit n (IT num) = putStrLnWithIdent n $ "Literal numerico: " ++ (show num) 
printLit n (CT s) = putStrLnWithIdent n $ "Literal de Caracter : " ++ s
printLit n (FT num) = putStrLnWithIdent n $ "Literal numerico: " ++ (show num) 
printLit n (TBT) = putStrLnWithIdent n $ "Booleano: On"
printLit n (FBT) = putStrLnWithIdent n $ "Booleano: Off"
printLit n (ST s) = putStrLnWithIdent n $ "String : " ++ s

printOperation n (PUT exp) = do
    putStrLnWithIdent n "Mas unario:"
    printExp (n+1) exp
printOperation n (MUT exp) = do
    putStrLnWithIdent n "Menos unario:"
    printExp (n+1) exp
printOperation n (NUT exp) = do
    putStrLnWithIdent n "Negacion booleana:"
    printExp (n+2) exp
printOperation n (DUT exp) = do
    putStrLnWithIdent n "Deferencia unaria:"
    printExp (n+1) exp

printOperation n (ABT exp s exp1) = do
    putStrLnWithIdent n "Operacion de comparacion:"
    putStrLnWithIdent (n+1) $ "Comparador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExp (n+2) exp1
    
printOperation n (LBT exp s exp1) = do
    putStrLnWithIdent n "Operacion binaria logica:"
    putStrLnWithIdent (n+1) $ "Operador: " ++ s
    putStrLnWithIdent (n+1) "Lado izquierdo:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Lado derecho:"
    printExp (n+2) exp1

printOperation n (GAT s exp) = do 
    putStrLnWithIdent n "Get item array:"
    putStrLnWithIdent (n+1) "Identificador:"
    printId (n+1) s
    putStrLnWithIdent (n+1) "Pos a agarrar:"
    printExp (n+1) exp   

printOperation n (AT s exp) = do 
    putStrLnWithIdent n "Asignacion :"
    putStrLnWithIdent (n+1) "Identificador:"
    printId (n+1) s
    putStrLnWithIdent (n+1) "Valor a asignar:"
    printExp (n+1) exp


printFuntionBC :: Int -> FuntionBC -> IO()
printFuntionBC n (StaT st) =   printStatement n st
printFuntionBC n (RT exp) = do
    putStrLnWithIdent n "Instruccion de Return:"
    putStrLnWithIdent (n+1) "Expresion a retornar:"
    printExp (n+1) exp

printType :: Int -> Type -> IO()
printType n (TI) = putStrLnWithIdent n "Tipo Entero"
printType n (TF) = putStrLnWithIdent n "Tipo Flotante"
printType n (TC) = putStrLnWithIdent n "Tipo Caracter"
printType n (TB) = putStrLnWithIdent n "Tipo Booleano"
printType n (TS) = putStrLnWithIdent n "Tipo String"

printStatement :: Int -> Statement -> IO()
printStatement n (InsT i) = printInst n i
printStatement n (DecT d) = printDecl n d

printInst n (IFT s l exp) = do
    putStrLnWithIdent n "Instruccion if-then:"
    putStrLnWithIdent (n+1) "Persona: "
    printId n s
    putStrLnWithIdent (n+1) "Condicion:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Bloque 1 (then):"
    printLists (n+2) l


printInst n (IFET s l exp l1) = do
    putStrLnWithIdent n "Instruccion if-then:"
    putStrLnWithIdent (n+1) "Persona: "
    printId n s
    putStrLnWithIdent (n+1) "Condicion:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Bloque 1 (then):"
    printLists (n+2) l
    putStrLnWithIdent (n+1) "Bloque 2 (else):"
    printLists (n+2) l1

printInst n (UIT s exp l) = do
    putStrLnWithIdent n "Instruccion while-do:"
    putStrLnWithIdent (n+1) "Persona: "
    printId n s
    putStrLnWithIdent (n+1) "Condicion:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Bloque de instrucciones:"
    printLists (n+1) l

printInst n (BIT l s exp) = do
    putStrLnWithIdent n "Instruccion for:"
    putStrLnWithIdent (n+1) "Persona: "
    printId n s
    putStrLnWithIdent (n+1) "Cantidad de iteraciones:"
    printExp (n+2) exp
    putStrLnWithIdent (n+1) "Bloque de instrucciones:"
    printLists (n+1) l

printInst n (ExprT exp) = printExp n exp                   
 
printDecl :: Int -> Declaration -> IO()
printDecl n (FDT) = do
    putStrLnWithIdent n "Definicion de funcion sin argumentos."

printDecl n (FDAT) = do
    putStrLnWithIdent n "Definicion de funcion con argumentos."

printDecl n (PDT) = do 
    putStrLnWithIdent n "Definicion de Personas."

printDecl n (VDT) = do
    putStrLnWithIdent n "Definicion de Variables."

