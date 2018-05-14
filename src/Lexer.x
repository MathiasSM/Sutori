{
module Lexer(Token(..), TokenClass(..), isInvalid, runAlexScan, AlexUserState(..), AlexPosn(..)) where

import Control.Monad
import Data.Maybe
import Numeric ( readDec )
import Data.Char ( chr )

}

%wrapper "monadUserState"

$digit = [0-9]          -- digits

tokens :-

<0>                $white+                              ;
<0>                "--".*                               ;

<0>                "...("                               { mkL BLOCK_OPEN }
<0>                ")..."                               { mkL BLOCK_CLOSE }

<0>                "Once upon a time in"                { mkL PROGRAM_INI     }
<0>                "and they lived happily ever after"  { mkL PROGRAM_FIN     }
<0>                "Once upon some other time in"       { mkL FUNCTION_INI    }
<0>                "or that is what they say"           { mkL FUNCTION_FIN    }

<0>                "And that's where"                   { mkL S_Andthatswhere }
<0>                "There was"                          { mkL S_Therewas      }
<0>                "broke a"                            { mkL S_brokea }
<0>                "brought a"                          { mkL S_broughta }
<0>                "comes from"                         { mkL S_comesfrom }
<0>                "dreams of"                          { mkL S_dreamsof }
<0>                "keeps dreaming of"                  { mkL S_keepsdreamingof }
<0>                "made a"                             { mkL S_madea }
<0>                "made of"                            { mkL S_madeof }
<0>                "there was a"                        { mkL S_therewasa     }
<0>                "told that story"                    { mkL S_toldthatstory }

<0>                "bag"                                { mkL TYPE_INT }
<0>                "wallet"                             { mkL TYPE_FLOAT }
<0>                "book"                               { mkL TYPE_CHAR }
<0>                "lightbulb"                          { mkL TYPE_BOOL }
<0>                "chain"                              { mkL TYPE_ARRAY }
<0>                "machine"                            { mkL TYPE_STRUCT }
<0>                "thing"                              { mkL TYPE_UNION }
<0>                "phrase"                             { mkL TYPE_STRING }
<0>                "direction"                          { mkL TYPE_POINTER }

<0>                "on"                                 { mkL TRUE }
<0>                "off"                                { mkL FALSE }
<0>                \'[a-z]\'                            { getTkChar  }
<0>                $digit+(\.$digit+)                   { getTkFloat }
<0>                $digit+                              { getTkInteger }

<0>                "("                                  { mkL OPEN_PAREN }
<0>                "["                                  { mkL OPEN_BRACKETS }
<0>                "{"                                  { mkL OPEN_BRACES }
<0>                ")"                                  { mkL CLOSE_PAREN }
<0>                "]"                                  { mkL CLOSE_BRACKETS }
<0>                "}"                                  { mkL CLOSE_BRACES }

<0>                "..."                                { mkL ELLIPSIS }
<0>                "."                                  { mkL PERIOD }
<0>                ","                                  { mkL COMMA }
<0>                ":"                                  { mkL COLON }
<0>                ";"                                  { mkL SEMICOLON }
<0>                "?"                                  { mkL INTERROGATION }
<0>                "!"                                  { mkL EXCLAMATION  }
<0>                "->"                                 { mkL ARROW_RIGHT }
<0>                "+"                                  { mkL PLUS }
<0>                "-"                                  { mkL MINUS }
<0>                "=="                                 { mkL EQUAL }
<0>                "="                                  { mkL ASSIGNMENT }
<0>                "*"                                  { mkL ASTERISK }
<0>                "%"                                  { mkL PERCENT }
<0>                "/"                                  { mkL SLASH }
<0>                "div"                                { mkL DIV }
<0>                "/="                                 { mkL NOT_EQUAL }
<0>                ">="                                 { mkL GREATER_EQUAL }
<0>                "<="                                 { mkL LESS_EQUAL }
<0>                ">"                                  { mkL GREATER }
<0>                "<"                                  { mkL LESS }
<0>                "^"                                  { mkL POWER }
<0>                "and"                                { mkL AND }
<0>                "or"                                 { mkL OR }

<0>                "with"                               { mkL WITH }
<0>                "your"                               { mkL YOUR }
<0>                "of"                                 { mkL OF }
<0>                "either"                             { mkL EITHER }
<0>                "to"                                 { mkL TO }

<0>                "when"                               { mkL WHEN }
<0>                "otherwise"                          { mkL OTHERWISE }
<0>                "times"                              { mkL TIMES }

<0>                "--|"                                { enterNewComment `andBegin` state_comment }
<state_comment>    "--|"                                { embedComment }
<state_comment>    "|--"                                { unembedComment }
<state_comment>    .                                    ;
<state_comment>    \n                                   { skip }

<0>                \"                                   { enterNewString `andBegin` state_string }
<state_string>     \"                                   { leaveString `andBegin` state_initial }
<state_string>     "\n"                                 { addCharToString '\n' }
<state_string>     "\t"                                 { addCharToString '\t' }
<state_string>     \\\^[@-_]                            { addControlToString }
<state_string>     \\$digit$digit$digit                 { addAsciiToString }
<state_string>     \\\"                                 { addCharToString '\"' }
<state_string>     \\\\                                 { addCharToString '\\' }
<state_string>     \\[\ \n\t\f\r\b\v]+\\                ;
<state_string>     \\                                   { \_ _ -> lexerError "Illegal escape sequence" }
<state_string>     .                                    { addCurrentToString }
<state_string>     \n                                   { skip }
<0>                \n                                   { skip }

<0>                [A-Z][A-Z\_0-9]*                     { getTkFuncId }
<0>                [a-zA-Z][a-zA-Z\-\_]*                { getTkId }

<0>                [$digit \_]+                         { getError }
<0>                .                                    { getError }


{

-- Data definitions
type Action = AlexInput -> Int -> Alex Token

data Token = Token AlexPosn TokenClass

instance Show Token where
  show (Token _ EOF)   = "Token EOF"
  show (Token p cl) = "Token class = " ++ show cl ++ " posn = " ++ showPosn p

data TokenClass =
    EOF                 |
    BLOCK_OPEN          |
    BLOCK_CLOSE         |
    PROGRAM_INI         |
    PROGRAM_FIN         |
    FUNCTION_INI        |
    FUNCTION_FIN        |
    S_Andthatswhere     |
    S_Therewas          |
    S_brokea            |
    S_broughta          |
    S_comesfrom         |
    S_dreamsof           |
    S_keepsdreamingof   |
    S_madea             |
    S_madeof            |
    S_therewasa         |
    S_toldthatstory     |
    TYPE_INT            |
    TYPE_FLOAT          |
    TYPE_CHAR           |
    TYPE_BOOL           |
    TYPE_ARRAY          |
    TYPE_STRUCT         |
    TYPE_UNION          |
    TYPE_STRING         |
    TYPE_POINTER        |
    TRUE                |
    FALSE               |
    OPEN_PAREN          |
    OPEN_BRACKETS       |
    OPEN_BRACES         |
    CLOSE_PAREN         |
    CLOSE_BRACKETS      |
    CLOSE_BRACES        |
    ELLIPSIS            |
    PERIOD              |
    COMMA               |
    COLON               |
    SEMICOLON           |
    INTERROGATION       |
    EXCLAMATION         |
    ARROW_RIGHT         |
    PLUS                |
    MINUS               |
    EQUAL               |
    ASSIGNMENT          |
    ASTERISK            |
    PERCENT             |
    SLASH               |
    DIV                 |
    NOT_EQUAL           |
    GREATER_EQUAL       |
    LESS_EQUAL          |
    GREATER             |
    LESS                |
    POWER               |
    AND                 |
    OR                  |
    WITH                |
    YOUR                |
    OF                  |
    EITHER              |
    TO                  |
    WHEN                |
    OTHERWISE           |
    TIMES               |
    InvalidToken String |
    Character String    |
    Float' Float        |
    Integer' Int        |
    String' String      |
    FunctionID String   |
    ID String
  deriving (Eq,Show)


-- Behavior
state_initial :: Int
state_initial = 0

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = "(" ++ show line ++ "," ++ show col ++ ")"


-- Token getters
getTkChar (p, _, _, str) len     = return (Token p (Character (take len str)))
getTkFloat (p, _, _, str) len    = return (Token p (Float' (read $ take len str)))
getTkInteger (p, _, _, str) len  = return (Token p (Integer' (read $ take len str)))

getTkId (p, _, _, str) len       = return (Token p (ID (take len str)))
getTkFuncId (p, _, _, str) len   = return (Token p (FunctionID (take len str)))

getError :: Action
getError (p, _, _, input) len =
    do setLexerError True
       return (Token p (InvalidToken s))
    where s = take len input


-- Token generator
mkL :: TokenClass -> AlexInput -> Int -> Alex Token
mkL c (p, _, _, str) len = return (Token p c)


-- Actions

-- Comment-related actions
enterNewComment :: Action
enterNewComment input len =
    do setLexerCommentDepth 1
       skip input len
embedComment :: Action
embedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip input len
unembedComment :: Action
unembedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       skip input len

-- String-related actions
enterNewString :: Action
enterNewString _ _ =
    do setLexerStringState True
       setLexerStringValue ""
       alexMonadScan
leaveString :: Action
leaveString (p, _, _, input) len =
    do s <- getLexerStringValue
       setLexerStringState False
       return (Token p (String' (reverse s)))

addCharToString :: Char -> Action
addCharToString c _ _ =
    do addCharToLexerStringValue c
       alexMonadScan

addCurrentToString :: Action
addCurrentToString i@(_, _, _, input) len = addCharToString c i len
  where
    c = if (len == 1)
        then head input
        else error "Invalid call to addCurrentToString''"

-- Special form '\nnn'
addAsciiToString :: Action
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


-- Special form '\^A'
addControlToString :: Action
addControlToString i@(_, _, _, input) len = addCharToString c' i len
  where
    c  = if (len == 1)
         then head input
         else error "Invalid call to 'addControlToString'"
    v  = ord c
    c' = if (v >= 64)
         then chr (v - 64)
         else error "Invalid call to 'addControlToString'"



-- The mighty monadUserState
data AlexUserState = AlexUserState
                   {
                         lexerErrorTok      :: Bool
                       , lexerCommentDepth  :: Int
                       , lexerStringState   :: Bool
                       , lexerStringValue   :: String
                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   {
                         lexerErrorTok      = False
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

alexEOF :: Alex Token
alexEOF = return (Token undefined EOF)

runAlexScan s = scanner s

}
