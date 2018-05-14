{
module Parser where
import Lexer
import AST
}

%name calc
%tokentype { Lexema }

%token
    begin   		{ INI _ } 
    end 			{ END _ }
    fBegin			{ F_INI _ }
    fEnd 			{ F_FIN  _ }
    thereWas 		{ THEREWAS _ }
    broughta 		{ BROUGHTA _ }
    dreamsof 		{ DREAMSOF _ }      
    keeps 			{ KEEPSDREAMINGOF  _ }
    andThat			{ ANDTHAT _ }
    told 			{ TOLDTHESTORY _ }
    comes			{ COMESFROM _ }
    made 			{ MADEA  _ }
    broke			{ BROKEA _ }
    intNum 			{ INT_TYPE  _ }
    floatNum 		{ FLOAT_TYPE  _ }
    charc 			{ CHAR_TYPE  _ }
   	boolean 		{ BOOL_TYPE _ }
    arrayT 			{ ARRAY_TYPE _ }  
    structT 		{ STRUCT_TYPE  _ }
    unionT 			{ UNION_TYPE  _ } 
    pointerT 		{ POINTER_TYPE  _ } 
    and 			{ AND  _ }        
    or 				{ OR    _ }       
    of 				{ OF   _ }        
    with 			{ WITH  _ }       
    either 			{ EITHER _ }      
    to 				{ TO _ }          
    when 			{ WHEN  _ }       
    otherwise 		{ OTHERWISE _ }   
    from 			{ FROM  _ }
    times           { TIMES _ }       
    '...(' 			{ OPEN  _ }       
    ')...' 			{ CLOSE  _ }     
    on 				{ TrueTK _ }      
    off 			{ FalseTK _ }     
    '.' 			{ POINT _ }       
    ',' 			{ COMMA _ }       
    ':' 			{ COLONS  _ }
    ';'  			{ SEMICOLON _ }   
    '!' 			{ Neg  _ }
    '['				{ OpenC _ }
    ']'				{ CloseC _ }   
    '{'				{ OpenL _ }
    '}'				{ CloseL _ }        
    '('				{ ParenOpen _ }   
    ')' 			{ ParenClose _ }  
    '+'				{ Plus _ }        
    '==' 			{ Equal _ }       
    '*' 			{ Product _ }     
    '-' 			{ Minus _ }       
    '%' 			{ Mod _ }         
    '/' 			{ DivExac _ }     
    div 			{ DivFloat _ }
    your 			{ Your _ }    
    '/=' 			{ Dif _ }         
    '=' 			{ Assign _ }      
    '>=' 			{ GreaterEqual _ }
    '->'			{ Arrow _ }
    '<=' 			{ LessEqual _ }   
    '>' 			{ Greater _ }     
    '<' 			{ Less _ }        
    '^' 			{ Pot _ }         
    f 				{ FloatNumber _ $$ }
    n 				{ IntegerNumber _  $$}
    id 				{ Id _ $$ }
    c 				{ Character _  $$ }
    fid 			{ FuncId _ $$ }
    s 				{ String _  $$ }

%left or
%left and
%nonassoc '>' '<' '==' '/=' '>=' '<=' 
%left '+' '-'
%left '*' '/' '%' mod div
%left '^'
%nonassoc '!'
%left '->'


%% 

-- Program
P : begin id B end { PN $1 }

B : '...(' BC ')...' { BN $2 }

-- List of Blocks
BC  : {-empty-}         { BCN [] }
    | S '.' BC          { BCN $ $1 : listBCN $2}

-- Statement 
S : DF  { SN $1 }
  | PD  { SN $1 }
  | VD  { SN $1 }
  | I   { SN $1 }

-- Function Block
FB : '...(' FBC ')...' { FBN $1 }

-- Function Block Content
FBC : S '.' FBC   				{ FBCN $ $1 : ListFBCN $3 } 
	| RF '.' FBC 				{ FBCN $ $1 : ListFBCN $3 }
	| {-empty-}  				{ FBCN [] }

-- Return Function
RF : andThat E comes { REN $1 }

-- Function Declaration
DF  : '(' fBegin fid ',' thereWas T FB fEnd ')' { DFN (tokenString $3) $6 $7 }
	| '(' fBegin fid ',' thereWas T '(' from LP ')' FB fEnd ')' { PDFN (tokenString $3) $6 $9 $11 }

-- List of params    
LP : T id             { LPN [($1, (tokenString $2),1)] }
   | T your id         { LPN [($1, (tokenString $3),0)] }
   | T id ',' LP      { LPN $ ($1, (tokenString $2),1) : listLPN $4 }
   | T your id ',' LP      { LPN $ ($1, (tokenString $3),1)) : listLPN $5 }

--Types 
T : intNum 						{ IntNumN }
  | floatNum 					{ FloatNumN }
  | charc 						{ CharcN }
  | boolean 					{ BooleanN }
  |	arrayT '(' of n T ')'		{ ArrayN $4 $5 }
  | structT '(' with ST ')'		{ StructN $4 }
  | unionT '(' either UT ')'    { UnionN $4 }
  | pointerT '(' to T ')'       { PointerN $4 }

--Struct Type
ST : T id 	 		{ STN [($1, (tokenString $2))]} 
   | T id and ST 	{ STN $ ($1, (tokenString $2)) : listSTN $4 }

--Union Type
UT : T id 	 		{ UTN [($1, (tokenString $2))] } 
   | T id or UT 	{ UTN $ ($1, (tokenString $2)) : listUTN $4 }

-- Person Declaration
PD : thereWas LPD   { PDN $ 1 }

-- List of Person Declaration
LPD : id 		  { LDPN [tokenString $1] }  		  
	| id ',' LPD  { LPDN $ (tokenString $1) : listLDPN $3 }
	| id and LPD  { LPDN $ (tokenString $1) : listLDPN $3 }

-- Variable Declaration
VD : id broughta T ':' LV  { VDN $3 $5 }

-- List of Variable Declaration
LV : id '=' E  			 { LVN [(tokenString $1,$3)] }
   | id 	   			 { LVN [(tokenString $1,undefined)] }
   | id '=' E ',' LV   { LVN $ (tokenString $1,$3) : listLVN $5 } 
   | id E ',' LV       { LVN $ (tokenString $1,undefined) : listLVN $4 }

-- List of Instructions
I : E     									{ ExprN $1 }
  | E '->' id   		 					{ GetPropN $1 $3 }
  | id '=' E 			 					{ Assignment $1 $3 }
  | id dreamsof B when E 					{ IfThenN $1 $3 $5 }
  | id dreamsof B when E ';' otherwise B    { IfThenElseN $1 $3 $5 $8 }
  | id keeps E B 							{ WhileN $1 $3 $4 }
  | B id told E times                       { ForN $2 $1 $4 } 
  | id made T 								{ CreatePointerN $1 $3 }
  | id broke id  	                        { FreePointerN $1 $3 }

-- Constructor Array 
LCA : E 			{ LCAN $ [$1] }
	| E ',' LCA     { LCAN $ $1: listLCA $3 }

-- Constructor Struct
LCS : id ':' E 				{ LCSN $ [($1,$3)] }
	| id ':' E ',' LCS      { LCSN $ ($1,$3): listLCA $5 }

-- Function call parameters xd  
FP : E   		{ FPN [$1] }
   | E ',' FP 	{ FPN $ $1: ListFPN $3 }

-- Expressions
E : id 					 { IdN (tokenString $1) (tokenPos $1) }
  | '[' LCA ']' 		 { CN $2 }
  | '{' LCS '}' 		 { CN $2 }
  | fid 				 { FCN $1 [] }
  | fid '(' with FP ')'  { FCN $1 $4 }
  | '!' E  				 { NotN $2 }
  | '-' E  				 { MinusN $2 }
  |	'*' E   	 		{ DeferenceN $2 }
  | E '+' E 			 { BinaryN $1 "+" $3 }
  | E '-' E 			 { BinaryN $1 "-" $3 }
  | E '*' E 			 { BinaryN $1 "*" $3 }
  | E '/' E 			 { BinaryN $1 "/" $3 }
  | E div E 			 { BinaryN $1 "div" $3 }
  | E '%' E 			 { BinaryN $1 "%" $3 }
  | E '^' E 			 { BinaryN $1 "^" $3 }
  |	E and E     		 { CompareN $1 "and" $3 } 
  |	E or E      		 { CompareN $1 "or" $3 }
  |	E '==' E    		 { CompareN $1 "==" $3 }
  |	E '/=' E    		 { CompareN $1 "/=" $3 }
  |	E '>=' E    		 { CompareN $1 ">=" $3 }
  |	E '<=' E    		 { CompareN $1 "<=" $3 }
  |	E '>' E     		 { CompareN $1 ">" $3 }
  |	E '<' E     		 { CompareN $1 "<" $3 }
  | id '[' E ']' 		 { GetArrayItem $1 $3 }
  | '(' E ')' 			 { ParentExp $2 }
  | n 					 { NumberLiteralN (tokenString $1) (tokenPos $1) }
  |	f 					 { NumberLiteralN (tokenString $1) (tokenPos $1) }
  | c 					 { NumberLiteralN (tokenString $1) (tokenPos $1) }
  | s 					 { NumberLiteralN (tokenString $1) (tokenPos $1) }
  | on 					 { TrueN }
  |	off 				 { FalseN }


{

main:: IO ()
main = interact (show.runCalc)

trans :: [Token] -> [Lexema]
trans [Token (EOF _)] = []
trans ((Token p (FloatNumber f)):toks) = (FloatNumber p f):(trans toks)
trans ((Token p (IntegerNumber f)):toks) = (IntegerNumber p f):(trans toks)
trans ((Token p (Id f)):toks) = (Id p f):(trans toks)
trans ((Token p (FuncId f)):toks) = (FuncId p f):(trans toks)
trans ((Token p (String f)):toks) = (String p f):(trans toks)
trans ((Token p cl):toks) = (cl p):(trans toks) 


runCalc :: String -> Exp
runCalc = calc . trans .  alexScanTokens

data Lexema =
   	INI                    { tkPos :: (Int,Int)} |
    FIN                    { tkPos :: (Int,Int)} |

  deriving (Eq)

data 

happyError :: [Lexema] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
	where
	lcn = 	case tks of
		  [] -> "end of file"
		  tk:_ -> "line " ++ show l ++ ", column " ++ show c
			where
			AlexPn _ l c = token_posn tk
}
