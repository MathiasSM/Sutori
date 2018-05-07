{
module Lexer(Token(..), printToken, runAlexScan, AlexUserState(..), AlexPosn(..)) where

import Control.Monad
import Data.Maybe
}

%wrapper "monadUserState"

$digit = 0-9            -- digits
$Alpha = [a-zA-Z]       -- all alphabetic characters
$alpha = [a-z]          -- minus alphabetic characters
$ALPHA = [A-Z]          -- mayus alphabetic characters
$sim = [\=\+\-\*\/\/\=\%\=\=\<\>\>\=\<\=]

tokens :-

    "Once upon a time in"                { mkL INI }
    "and they lived happily ever after"  { mkL FIN }
    "Once upon some other time in"       { mkL F_INI }
    "or that is what they say"           { mkL F_FIN }
    "There was"                          { mkL THEREWAS }
    "brought a"                          { mkL BROUGHTA }
    "dreams of"                          { mkL DREAMSOF }
    "keeps dreaming of"                  { mkL KEEPSDREAMINGOF }
    "told the story"                     { mkL TOLDTHESTORY }
    "made a"                             { mkL MADEA }
    $white+                              ;
    "bag"                                { mkL INT_TYPE }
    "wallet"                             { mkL FLOAT_TYPE }
    "book"                               { mkL CHAR_TYPE }
    "lightbulb"                          { mkL BOOL_TYPE }
    "chain"                              { mkL ARRAY_TYPE }
    "machine"                            { mkL STRUCT_TYPE }
    "thing"                              { mkL UNION_TYPE }
    "direction"                          { mkL POINTER_TYPE }
    "return"                             { mkL Return }
    "a"                                  { mkL A }
    "and"                                { mkL AND }
    "or"                                 { mkL OR }
    "of"                                 { mkL OF }
    "with"                               { mkL WITH }
    "either"                             { mkL EITHER }
    "to"                                 { mkL TO }
    "when"                               { mkL WHEN }
    "otherwise"                          { mkL OTHERWISE }
    "from"                               { mkL FROM }
    \.\.\.\(                             { mkL OPEN }
    \)\.\.\.                             { mkL CLOSE }
    \.\.\.                               { mkL TPOINTS }
    true                                 { mkL TrueTK }
    false                                { mkL FalseTK }
    \.                                   { mkL POINT }
    \,                                   { mkL COMMA }
    \:                                   { mkL COLONS }
    \$                                   { mkL DOLLAR }
    \?                                   { mkL INTER }
    \!                                   { mkL EXCL }
    \(                                   { mkL ParenOpen }
    \)                                   { mkL ParenClose }
    \+                                   { mkL Plus }
    \=\=                                 { mkL Equal }
    \*                                   { mkL Product }
    \-                                   { mkL Minus }
    \%                                   { mkL Rest }
    \/                                   { mkL DivExac }
    \/\=                                 { mkL Dif }
    \>\=                                 { mkL GreaterEqual }
    \<\=                                 { mkL LessEqual }
    \>                                   { mkL Greater }
    \<                                   { mkL Less }
    \^                                   { mkL Pot }
    $digit+(\.[$digit]+)                 { getFloatNumber }
    $digit+                              { getIntegerNumber }
    $digit+\.+                           { getError }
    [a-zA-Z][a-zA-Z\_]*                  { getId }
    [a-zA-Z][a-zA-Z\_0-9]*               { getFuncId }
    [$digit \_]+                         { getError }
    .                                    { getError }

{


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
getFuncId (p, _, _, str) len = return (Token p (FuncId (take len str)))

mkL :: TokenClass -> AlexInput -> Int -> Alex Token
mkL c (p, _, _, str) len = return (Token p c)

data TokenClass =
    EOF                    |
    INI                    |
    FIN                    |
    F_INI                  |
    F_FIN                  |
    THEREWAS               |
    BROUGHTA               |
    DREAMSOF               |
    KEEPSDREAMINGOF        |
    TOLDTHESTORY           |
    MADEA                  |
    INT_TYPE               |
    FLOAT_TYPE             |
    CHAR_TYPE              |
    BOOL_TYPE              |
    ARRAY_TYPE             |
    STRUCT_TYPE            |
    UNION_TYPE             |
    POINTER_TYPE           |
    Return                 |
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
    CLOSE                  |
    TPOINTS                |
    TrueTK                 |
    FalseTK                |
    POINT                  |
    COMMA                  |
    COLONS                 |
    DOLLAR                 |
    INTER                  |
    EXCL                   |
    ParenOpen              |
    ParenClose             |
    Plus                   |
    Equal                  |
    Product                |
    Minus                  |
    Rest                   |
    DivExac                |
    Dif                    |
    GreaterEqual           |
    LessEqual              |
    Greater                |
    Less                   |
    Pot                    |
    FloatNumber Float      |
    IntegerNumber  Int     |
    Id String              |
    FuncId String          |
    InvalidToken  String
  deriving (Eq,Show)

type Action = AlexInput -> Int -> Alex Token

getError :: Action
getError (p, _, _, input) len =
    do setLexerError True
       return (Token p (InvalidToken s))
    where s = take len input


data AlexUserState = AlexUserState
                   {
                       lexerError  :: Bool
                   }
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   {
                       lexerError = False
                   }

getLexerError :: Alex Bool
getLexerError = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerError ust)

setLexerError :: Bool -> Alex ()
setLexerError ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerError=ss}}, ())

alexEOF :: Alex Token
alexEOF = return (Token undefined EOF)

scanner :: String -> Either String [Token]
scanner str = let loop = do (t, m) <- alexComplementError alexMonadScan
                            when (isJust m) (alexError (fromJust m))
                            let tok@(Token p cl) = t
                            if (cl == EOF)
                               then do f1 <- getLexerError
                                       if (not f1)
                                          then return [tok]
                                          else alexError $ "Error Lexicografico, Alex isn't Happy:("
                               else do toks <- loop
                                       return (tok : toks)
              in  runAlex str loop


alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) = Alex (\s -> case al s of
                                                 Right (s', x) -> Right (s', (x, Nothing))
                                                 Left  message -> Right (s, (undefined, Just message)))
runAlexScan s = scanner s


}

