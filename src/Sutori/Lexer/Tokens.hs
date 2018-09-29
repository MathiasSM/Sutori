module Sutori.Lexer.Tokens
( SutToken(..)
, isValid
) where

-- Sutori token classes. Can be shown with SutShow
data SutToken = SutTkEOF

              | BLOCK_OPEN
              | BLOCK_CLOSE

              | PROGRAM_INI
              | PROGRAM_FIN
              | FUNCTION_INI
              | FUNCTION_FIN

              | S_andthatswhere
              | S_therewas
              | S_brokea
              | S_broughta
              | S_comesfrom
              | S_dreamsof
              | S_keepsdreamingof
              | S_madea
              | S_madeof
              | S_therewasa
              | S_toldthatstory
              | S_invented
              | S_itsa

              | TYPE_INT
              | TYPE_FLOAT
              | TYPE_CHAR
              | TYPE_BOOL
              | TYPE_ARRAY
              | TYPE_STRUCT
              | TYPE_UNION
              | TYPE_STRING
              | TYPE_POINTER

              | OPEN_PAREN
              | OPEN_BRACKETS
              | OPEN_BRACES
              | CLOSE_PAREN
              | CLOSE_BRACKETS
              | CLOSE_BRACES

              | ELLIPSIS
              | PERIOD
              | COMMA
              | COLON
              | SEMICOLON
              | QUESTIONMARK
              | EXCLAMATION
              | ARROW_RIGHT
              | PLUS
              | MINUS
              | EQUAL
              | ASSIGNMENT
              | ASTERISK
              | PERCENT
              | SLASH
              | DIV
              | NOT_EQUAL
              | GREATER_EQUAL
              | LESS_EQUAL
              | GREATER
              | LESS
              | POWER
              | AND
              | OR
              | WITH
              | YOUR
              | OF
              | EITHER
              | TO
              | WHEN
              | OTHERWISE
              | TIMES

              | SutTkBool   { tokenBool :: Bool }
              | SutTkChar   { tokenChar :: String }
              | SutTkFloat  { tokenFloat :: Float }
              | SutTkInt    { tokenInt :: Int }
              | SutTkString { tokenString :: String }

              | SutTkError  { tokenError :: String }

              | SutTkID     { tokenID :: String }
  deriving (Eq,Show)


-- Says if the given token is valid (not an error) or not
isValid :: SutToken -> Bool
isValid (SutTkError _)  = False
isValid _               = True
