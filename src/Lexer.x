{
module Lexer(Token(..), printToken, isInvalid, runAlexScan, AlexUserState(..), AlexPosn(..)) where

import Control.Monad
import Data.Maybe
import Numeric ( readDec )
import Data.Char ( chr )

}

%wrapper "monadUserState"

$digit = 0-9            -- digits
$Alpha = [a-zA-Z]       -- all alphabetic characters
$alpha = [a-z]          -- minus alphabetic characters
$ALPHA = [A-Z]          -- mayus alphabetic characters
$sim = [\=\+\-\*\/\/\=\%\=\=\<\>\>\=\<\=]

tokens :-

<0>                "Once upon a time in"                { mkL INI }
<0>                "and they lived happily ever after"  { mkL FIN }
<0>                "Once upon some other time in"       { mkL F_INI }
<0>                "or that is what they say"           { mkL F_FIN }
<0>                "There was"                          { mkL THEREWAS }
<0>                "And that's where"                   { mkL ANDTHAT }
<0>                "brought a"                          { mkL BROUGHTA }
<0>                "comes from"                         { mkL COMESFROM }
<0>                "dreams of"                          { mkL DREAMSOF }
<0>                "keeps dreaming of"                  { mkL KEEPSDREAMINGOF }
<0>                "told the story"                     { mkL TOLDTHESTORY }
<0>                "made a"                             { mkL MADEA }
<0>                "broke a"                            { mkL BROKEA }
<0>                $white+                              ;
<0>                "--".*                               ;
<0>                "bag"                                { mkL INT_TYPE }
<0>                "wallet"                             { mkL FLOAT_TYPE }
<0>                "your"                               { mkL YOUR }
<0>                "book"                               { mkL CHAR_TYPE }
<0>                "lightbulb"                          { mkL BOOL_TYPE }
<0>                "chain"                              { mkL ARRAY_TYPE }
<0>                "machine"                            { mkL STRUCT_TYPE }
<0>                "thing"                              { mkL UNION_TYPE }
<0>                "phrase"                             { mkL STRING_TYPE }
<0>                "direction"                          { mkL POINTER_TYPE }
<0>                "return"                             { mkL Return }
<0>                "a"                                  { mkL A }
<0>                "and"                                { mkL AND }
<0>                "or"                                 { mkL OR }
<0>                "of"                                 { mkL OF }
<0>                "with"                               { mkL WITH }
<0>                "either"                             { mkL EITHER }
<0>                "to"                                 { mkL TO }
<0>                "times"                              { mkL TIMES }
<0>                "when"                               { mkL WHEN }
<0>                "otherwise"                          { mkL OTHERWISE }
<0>                "from"                               { mkL FROM }
<0>                \.\.\.\(                             { mkL OPEN }
<0>                \)\.\.\.                             { mkL CLOSE }
<0>                \.\.\.                               { mkL TPOINTS }
<0>                on                                   { mkL TrueTK }
<0>                off                                  { mkL FalseTK }
<0>                \.                                   { mkL POINT }
<0>                \,                                   { mkL COMMA }
<0>                \:                                   { mkL COLONS }
<0>                \;                                   { mkL SEMICOLON }
<0>                \[                                   { mkL OpenC }
<0>                \]                                   { mkL CloseC }
<0>                \{                                   { mkL OpenL }
<0>                \}                                   { mkL CloseL }
<0>                \$                                   { mkL DOLLAR }
<0>                \?                                   { mkL INTER }
<0>                \!                                   { mkL Neg  }
<0>                \(                                   { mkL ParenOpen }
<0>                \)                                   { mkL ParenClose }
<0>                \-\>                                 { mkL Arrow }
<0>                \+                                   { mkL Plus }
<0>                \=\=                                 { mkL Equal }
<0>                \=                                   { mkL Assign }
<0>                \*                                   { mkL Product }
<0>                \-                                   { mkL Minus }
<0>                \%                                   { mkL Mod }
<0>                \/                                   { mkL DivExac }
<0>                div                                  { mkL DivFloat }
<0>                \/\=                                 { mkL Dif }
<0>                \>\=                                 { mkL GreaterEqual }
<0>                \<\=                                 { mkL LessEqual }
<0>                \>                                   { mkL Greater }
<0>                \<                                   { mkL Less }
<0>                \^                                   { mkL Pot }
<0>                "/*"                                 { enterNewComment `andBegin` state_comment }
<state_comment>    "/*"                                 { embedComment }
<state_comment>    "*/"                                 { unembedComment }
<state_comment>    .                                    ;
<state_comment>    \n                                   { skip }
<0>                \"                                   { enterNewString `andBegin` state_string }
<state_string>     \\n                                  { addCharToString '\n' }
<state_string>     \\t                                  { addCharToString '\t' }
<state_string>     \\\^[@-_]                            { addControlToString }
<state_string>     \\$digit$digit$digit                 { addAsciiToString }
<state_string>     \\\"                                 { addCharToString '\"' }
<state_string>     \\\\                                 { addCharToString '\\' }
<state_string>     \\[\ \n\t\f\r\b\v]+\\                ;
<state_string>     \\                                   { \_ _ -> lexerError "Illegal escape sequence" }
<state_string>     \"                                   { leaveString `andBegin` state_initial }
<state_string>     .                                    { addCurrentToString }
<state_string>     \n                                   { skip }
<0>                \n                                   { skip }
<0>                \'[a-z]\'                            { getCharTok  }
<0>                $digit+(\.[$digit]+)                 { getFloatNumber }
<0>                $digit+                              { getIntegerNumber }
<0>                [a-zA-Z][a-zA-Z\_]*                  { getId }
<0>                [A-Z][A-Z\_0-9]*                     { getFuncId }
<0>                [$digit \_]+                         { getError }
<0>                .                                    { getError }

{
state_initial :: Int
state_initial = 0
data Token = Token AlexPosn TokenClass
instance Show Token where
  show (Token _ EOF)   = "Token EOF"
  show (Token p cl) = "Token class = " ++ show cl ++ showap p
    where
      showap pp = " posn = " ++ showPosn pp


printToken tk = show tk
showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = "(" ++ show line ++ "," ++ show col ++ ")"
getFloatNumber (p, _, _, str) len = return (Token p (FloatNumber (read $ take len str)))
getIntegerNumber (p, _, _, str) len = return (Token p (IntegerNumber (read $ take len str)))
getId (p, _, _, str) len = return (Token p (Id (take len str)))
getCharTok (p, _, _, str) len = return (Token p (Character (take len str)))
getFuncId (p, _, _, str) len = return (Token p (FuncId (take len str)))
mkL :: TokenClass -> AlexInput -> Int -> Alex Token
mkL c (p, _, _, str) len = return (Token p c)

data TokenClass =
    EOF                    |
    INI                    |
    FIN                    |
    ANDTHAT                |
    F_INI                  |
    F_FIN                  |
    THEREWAS               |
    BROUGHTA               |
    YOUR                   |
    DREAMSOF               |
    COMESFROM              |
    KEEPSDREAMINGOF        |
    TOLDTHESTORY           |
    MADEA                  |
    BROKEA                 |
    INT_TYPE               |
    FLOAT_TYPE             |
    CHAR_TYPE              |
    BOOL_TYPE              |
    ARRAY_TYPE             |
    STRUCT_TYPE            |
    UNION_TYPE             |
    STRING_TYPE            |
    POINTER_TYPE           |
    Return                 |
    TIMES                  |
    OpenC                  |
    CloseC                 |
    A                      |
    AND                    |
    OR                     |
    OF                     |
    WITH                   |
    EITHER                 |
    TO                     |
    WHEN                   |
    OTHERWISE              |
    FROM                   |
    OPEN                   |
    SEMICOLON              |
    CLOSE                  |
    TPOINTS                |
    TrueTK                 |
    FalseTK                |
    POINT                  |
    COMMA                  |
    COLONS                 |
    DOLLAR                 |
    INTER                  |
    Neg                    |
    OpenL                  |
    CloseL                 |
    ParenOpen              |
    ParenClose             |
    Plus                   |
    Arrow                  |
    Equal                  |
    Product                |
    Minus                  |
    Mod                    |
    DivExac                |
    DivFloat               |
    Dif                    |
    Assign                 |
    GreaterEqual           |
    LessEqual              |
    Greater                |
    Less                   |
    Pot                    |
    FloatNumber Float      |
    IntegerNumber  Int     |
    Id String              |
    Character String       |
    FuncId String          |
    String String          |
    InvalidToken  String
  deriving (Eq,Show)


type Action = AlexInput -> Int -> Alex Token
getError :: Action
getError (p, _, _, input) len =
    do setLexerError True
       return (Token p (InvalidToken s))
    where s = take len input
-- actions
enterNewComment, embedComment, unembedComment :: Action
enterNewString, leaveString, addCurrentToString, addAsciiToString, addControlToString :: Action
enterNewComment input len =
    do setLexerCommentDepth 1
       skip input len
embedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip input len
unembedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       skip input len
enterNewString _     _   =
    do setLexerStringState True
       setLexerStringValue ""
       alexMonadScan
addCharToString :: Char -> Action
addCharToString c _     _   =
    do addCharToLexerStringValue c
       alexMonadScan
addCurrentToString i@(_, _, _, input) len = addCharToString c i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to addCurrentToString''"
-- if we are given the special form '\nnn'
addAsciiToString i@(_, _, _, input) len = if (v < 256)
                                          then addCharToString c i len
                                          else lexerError ("Invalid ascii value : " ++ input)
  where
    s = if (len == 4)
           then drop 1 input
           else error "Invalid call to 'addAsciiToString'"
    r = readDec s
    v = if (length r == 1)
           then fst (head r)
           else error "Invalid call to 'addAsciiToString'"
    c = chr v
-- if we are given the special form '\^A'
addControlToString i@(_, _, _, input) len = addCharToString c' i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to 'addControlToString'"
    v = ord c
    c' = if (v >= 64)
            then chr (v - 64)
            else error "Invalid call to 'addControlToString'"
leaveString (p, _, _, input) len =
    do s <- getLexerStringValue
       setLexerStringState False
       return (Token p (String (reverse s)))
data AlexUserState = AlexUserState
                   {
                         lexerErrorTok :: Bool
                       , lexerCommentDepth  :: Int
                       , lexerStringState   :: Bool
                       , lexerStringValue   :: String
                   }
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   {
                         lexerErrorTok = False
                       , lexerCommentDepth  = 0
                       , lexerStringState   = False
                       , lexerStringValue   = ""
                   }
getLexerError :: Alex Bool
getLexerError = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerErrorTok ust)
setLexerError :: Bool -> Alex ()
setLexerError ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerErrorTok=ss}}, ())
getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)
setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())
getLexerStringState :: Alex Bool
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState ust)
setLexerStringState :: Bool -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringState=ss}}, ())
getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)
setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}}, ())
addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=c:lexerStringValue (alex_ust s)}}, ())
alexEOF :: Alex Token
alexEOF = return (Token undefined EOF)
scanner :: String -> Either String [Token]
scanner str = let loop = do (t, m) <- alexComplementError alexMonadScan
                            when (isJust m) (alexError (fromJust m))
                            let tok@(Token p cl) = t
                            if (cl == EOF)
                               then do f1 <- getLexerStringState
                                       d2 <- getLexerCommentDepth
                                       if ((not f1) && (d2 == 0))
                                          then return [tok]
                                          else if (f1)
                                               then alexError "String not closed at end of file"
                                               else alexError "Comment not closed at end of file"
                               else do toks <- loop
                                       return (tok : toks)
              in  runAlex str loop
isInvalid (Token p (InvalidToken s)) = True
isInvalid _ = False
alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) = Alex (\s -> case al s of
                                                 Right (s', x) -> Right (s', (x, Nothing))
                                                 Left  message -> Right (s, (undefined, Just message)))
lexerError :: String -> Alex a
lexerError msg =
    do (p, c, _, inp) <- alexGetInput
       let inp1 = filter (/= '\r') (takeWhile (/='\n') inp)
       let inp2 = if (length inp1 > 30)
                     then trim (take 30 inp1)
                     else trim inp1
       let disp = if (null inp)
                     then " at end of file"
                     else if (null inp2)
                             then " before end of line"
                             else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
       let disp3 = if (null msg)
                      then "Lexer error"
                      else trim msg
       alexError (disp3 ++ " at " ++ showPosn p ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
runAlexScan s = scanner s

}