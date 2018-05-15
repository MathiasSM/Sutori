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
    fBegin			{ F_INI $$ }
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
    and 			{ AND  $$ }        
    or 				{ OR   $$ }       
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
    on 				{ TrueTK $$ }      
    off 			{ FalseTK $$ }     
    '.' 			{ POINT $$ }       
    ',' 			{ COMMA $$ }       
    ':' 			{ COLONS  $$ }
    ';'  			{ SEMICOLON $$ }   
    '!' 			{ Neg  $$ }
    '['				{ OpenC $$ }
    ']'				{ CloseC $$ }   
    '{'				{ OpenL $$ }
    '}'				{ CloseL $$ }        
    '('				{ ParenOpen $$ }   
    ')' 			{ ParenClose _ }  
    '+'				{ Plus $$ }        
    '==' 			{ Equal $$ }       
    '*' 			{ Product $$ }     
    '-' 			{ Minus $$ }       
    '%' 			{ Mod $$ }         
    '/' 			{ DivExac $$ }     
    div 			{ DivFloat $$ }
    your 			{ Your $$ }    
    '/=' 			{ Dif $$ }         
    '=' 			{ Assign $$ }      
    '>=' 			{ GreaterEqual $$ }
    '->'			{ Arrow $$ }
    '<=' 			{ LessEqual $$ }   
    '>' 			{ Greater $$ }     
    '<' 			{ Less $$ }        
    '^' 			{ Pot $$ }         
    f 				{ FloatNumber _ _ }
    n 				{ IntegerNumber _  _}
    id 				{ Id _ _ }
    c 				{ Character _  _ }
    fid 			{ FuncId _ _ }
    s 				{ StringTK _  $$ }

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
DF  : '(' fBegin fid ',' thereWas T FB fEnd ')' { DFN (tokenValue $3) $6 $7 $1 }
	| '(' fBegin fid ',' thereWas T '(' from LP ')' FB fEnd ')' { PDFN (tokenValue $3) $6 $9 $11 $1 }

-- List of params    
LP : T id             { LPN [($1, (tokenValue $2),1)] }
   | T your id         { LPN [($1, (tokenValue $3),0)] }
   | T id ',' LP      { LPN $ ($1, (tokenValue $2),1) : listLPN $4 }
   | T your id ',' LP      { LPN $ ($1, (tokenValue $3),0)) : listLPN $5 }

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
ST : T id 	 		{ STN [($1, (tokenValue $2))]} 
   | T id and ST 	{ STN $ ($1, (tokenValue $2)) : listSTN $4 }

--Union Type
UT : T id 	 		{ UTN [($1, (tokenValue $2))] } 
   | T id or UT 	{ UTN $ ($1, (tokenValue $2)) : listUTN $4 }

-- Person Declaration
PD : thereWas LPD   { PDN $ 1 }

-- List of Person Declaration
LPD : id 		  { LDPN [tokenValue $1] }  		  
	| id ',' LPD  { LPDN $ (tokenValue $1) : listLDPN $3 }
	| id and LPD  { LPDN $ (tokenValue $1) : listLDPN $3 }

-- Variable Declaration
VD : id broughta T ':' LV  { VDN $3 $5 }

-- List of Variable Declaration
LV : id '=' E  			 { LVN [(tokenValue $1,$3)] }
   | id 	   			 { LVN [(tokenValue $1,undefined)] }
   | id '=' E ',' LV   { LVN $ (tokenValue $1,$3) : listLVN $5 } 
   | id E ',' LV       { LVN $ (tokenValue $1,undefined) : listLVN $4 }

-- List of Instructions
I : E     									{ ExprN $1 }
  | E '->' id   		 					{ GetPropN $1 (tokenValue $3) $2 }
  | id '=' E 			 					{ AssignN $1 $3 (tokenPos $1) }
  | id dreamsof B when E 					{ IfThenN $1 $3 $5 (tokenPos $1) }
  | id dreamsof B when E ';' otherwise B    { IfThenElseN $1 $3 $5 $8 (tokenPos $1) }
  | id keeps E B 							{ WhileN $1 $3 $4 (tokenPos $1) }
  | B id told E times                       { ForN $1 $2 $4 (tokenPos $2)} 
  | id made T 								{ CreatePointerN $1 $3 (tokenPos $1)}
  | id broke id  	                        { FreePointerN $1 $3 (tokenPos $1) }

-- Constructor Array 
LCA : E 			{ LCAN $ [$1] }
	| E ',' LCA     { LCAN $ $1: listLCA $3 }

-- Constructor Struct
LCS : id ':' E 				{ LCSN $ [(tokenValue $1,$3)] }
	| id ':' E ',' LCS      { LCSN $ (tokenValue $1,$3): listLCA $5 }

-- Function call parameters xd  
FP : E   		{ FPN [$1] }
   | E ',' FP 	{ FPN $ $1: listFPN $3 }

-- Expressions
E : id 					 { IdN (tokenValue $1) (tokenPos $1) }
  | '[' LCA ']' 		 { CN $2 }
  | '{' LCS '}' 		 { CN $2 }
  | fid 				 { FCN (tokenValue $1) [] (tokenPos $1) }
  | fid '(' with FP ')'  { FCN (tokenValue $1) $4 (tokenPos $1) }
  | '!' E  				 { NotN $2 $1 }
  | '-' E  				 { MinusN $2 $1 }
  |	'*' E   	 		{ DeferenceN $2 $1 }
  | E '+' E 			 { BinaryN $1 "+" $3 $2 }
  | E '-' E 			 { BinaryN $1 "-" $3 $2 }
  | E '*' E 			 { BinaryN $1 "*" $3 $2 }
  | E '/' E 			 { BinaryN $1 "/" $3 $2 }
  | E div E 			 { BinaryN $1 "div" $3 $2}
  | E '%' E 			 { BinaryN $1 "%" $3 $2 }
  | E '^' E 			 { BinaryN $1 "^" $3 $2}
  |	E and E     		 { LogicN $1 "and" $3 $2 } 
  |	E or E      		 { LogicN $1 "or" $3 $2}
  |	E '==' E    		 { CompareN $1 "==" $3 $2}
  |	E '/=' E    		 { CompareN $1 "/=" $3 $2 }
  |	E '>=' E    		 { CompareN $1 ">=" $3 $2}
  |	E '<=' E    		 { CompareN $1 "<=" $3 $2}
  |	E '>' E     		 { CompareN $1 ">" $3 $2}
  |	E '<' E     		 { CompareN $1 "<" $3 $2}
  | id '[' E ']' 		 { GetArrayItem (tokenValue $1) $3 }
  | '(' E ')' 			 { ParentExp $2 $1 }
  | n 					 { IntegerLiteralN (tokenValue $1) (tokenPos $1) }
  |	f 					 { FloatLiteralN (tokenValue $1) (tokenPos $1) }
  | c 					 { CharLiteralN (tokenValue $1) (tokenPos $1) }
  | s 					 { StringLiteralN (tokenValue $1) (tokenPos $1) }
  | on 					 { TrueN (tokenPos $1) }
  |	off 				 { FalseN (tokenPos $1) }


{

main:: IO ()
main = interact (show.runCalc)

trans :: [Token] -> [Lexema]
trans [Token (EOF _)] = []
trans ((Token p (FloatNumber f)):toks) = (FloatNumber (np p) f):(trans toks)
trans ((Token p (IntegerNumber f)):toks) = (IntegerNumber (np p) f):(trans toks)
trans ((Token p (Id f)):toks) = (Id (np p) f):(trans toks)
trans ((Token p (FuncId f)):toks) = (FuncId (np p) f):(trans toks)
trans ((Token p (String f)):toks) = (String (np p) f):(trans toks)
trans ((Token p cl):toks) = (cl (np p)):(trans toks) 
	where np (AlexPn _ f c) = (f,c)


runCalc :: String -> Exp
runCalc = calc . trans .  alexScanTokens

data Lexema =
    INI 					{  tokenPos :: (Int, Int) } 
	END 					{  tokenPos :: (Int, Int) }
   	F_INI 					{  tokenPos :: (Int, Int) }
  	F_FIN  					{  tokenPos :: (Int, Int) }
    THEREWAS 				{  tokenPos :: (Int, Int) }
    BROUGHTA 				{  tokenPos :: (Int, Int) }
    DREAMSOF 				{  tokenPos :: (Int, Int) }      
   	KEEPSDREAMINGOF  		{  tokenPos :: (Int, Int) }
    ANDTHAT 				{  tokenPos :: (Int, Int) }
  	TOLDTHESTORY 			{  tokenPos :: (Int, Int) }
  	COMESFROM 				{  tokenPos :: (Int, Int) }
  	MADEA  					{  tokenPos :: (Int, Int) }
  	BROKEA 					{  tokenPos :: (Int, Int) }
    INT_TYPE  				{  tokenPos :: (Int, Int) }
    FLOAT_TYPE  			{  tokenPos :: (Int, Int) }
   	CHAR_TYPE  				{  tokenPos :: (Int, Int) }
    BOOL_TYPE 				{  tokenPos :: (Int, Int) }
    ARRAY_TYPE 				{  tokenPos :: (Int, Int) }  
    STRUCT_TYPE  		    {  tokenPos :: (Int, Int) }
    UNION_TYPE  			{  tokenPos :: (Int, Int) } 
    POINTER_TYPE  			{  tokenPos :: (Int, Int) } 
 	AND  					{  tokenPos :: (Int, Int) }        
 	OR    					{  tokenPos :: (Int, Int) }       
 	OF   					{  tokenPos :: (Int, Int) }        
  	WITH  					{  tokenPos :: (Int, Int) }       
    EITHER 					{  tokenPos :: (Int, Int) }      
 	TO 						{  tokenPos :: (Int, Int) }          
  	WHEN  					{  tokenPos :: (Int, Int) }       
    OTHERWISE 				{  tokenPos :: (Int, Int) }   
 	FROM  					{  tokenPos :: (Int, Int) }
    TIMES 					{  tokenPos :: (Int, Int) }       
   	OPEN  					{  tokenPos :: (Int, Int) }       
   	CLOSE  					{  tokenPos :: (Int, Int) }     
 	TrueTK 					{  tokenPos :: (Int, Int) }      
 	FalseTK 				{  tokenPos :: (Int, Int) }     
 	POINT 					{  tokenPos :: (Int, Int) }       
 	COMMA 					{  tokenPos :: (Int, Int) }       
 	COLONS  				{  tokenPos :: (Int, Int) }
  	SEMICOLON 				{  tokenPos :: (Int, Int) }   
 	Neg  					{  tokenPos :: (Int, Int) }
 	OpenC 					{  tokenPos :: (Int, Int) }
 	CloseC 					{  tokenPos :: (Int, Int) }   
 	OpenL 					{  tokenPos :: (Int, Int) }
 	CloseL 					{  tokenPos :: (Int, Int) }        
 	ParenOpen 				{  tokenPos :: (Int, Int) }   
 	ParenClose 				{  tokenPos :: (Int, Int) }  
 	Plus 					{  tokenPos :: (Int, Int) }        
  	Equal 					{  tokenPos :: (Int, Int) }       
 	Product 				{  tokenPos :: (Int, Int) }     
 	Minus 					{  tokenPos :: (Int, Int) }       
 	Mod 					{  tokenPos :: (Int, Int) }         
 	DivExac 				{  tokenPos :: (Int, Int) }     
 	DivFloat 				{  tokenPos :: (Int, Int) }
  	Your 					{  tokenPos :: (Int, Int) }    
  	Dif 					{  tokenPos :: (Int, Int) }         
 	Assign 					{  tokenPos :: (Int, Int) }      
  	GreaterEqual 			{  tokenPos :: (Int, Int) }
 	Arrow 					{  tokenPos :: (Int, Int) }
  	LessEqual 				{  tokenPos :: (Int, Int) }   
 	Greater 				{  tokenPos :: (Int, Int) }     
 	Less 					{  tokenPos :: (Int, Int) }        
 	Pot 					{  tokenPos :: (Int, Int) }         
	FloatNumber 			{ tokenPos :: (Int, Int), tokenValue :: Float } 
	IntegerNumber 			{ tokenPos :: (Int, Int), tokenValue :: Int } 
 	Id 						{ tokenPos :: (Int, Int), tokenValue :: String }
	Character 				{ tokenPos :: (Int, Int), tokenValue :: String }
 	FuncId 					{ tokenPos :: (Int, Int), tokenValue :: String }
	String 					{ tokenPos :: (Int, Int), tokenValue :: String }

  deriving (Eq)


happyError :: [Lexema] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
	where
	lcn = 	case tks of
		  [] -> "end of file"
		  tk:_ -> "line " ++ show l ++ ", column " ++ show c
			where
			AlexPn _ l c = token_posn tk
}
