{
module Sutori.Lexer
( lexerScan
, runLexer
) where

import Control.Monad
import Control.Monad.State
import Data.Char( chr, isSpace )
import Data.List
import Data.Maybe
import Numeric( readDec )

import Sutori.Lexer.Internals
import Sutori.Lexer.Posn (SutPosn(SutPosn))
import Sutori.Lexer.Tokens (SutToken(..))
import Sutori.Monad
  ( SutMonad
  , SutState(SutState, lexerInput, lexerStateCode, lexerBytes, lexerString, lexerStringOn, lexerDepth)
  , initialSutoriState
  )
import Sutori.Monad.Logger (logError, SutError(LexicalError))



}

tokens :-

<0>                $white+                   ;

<0>                "...("                    { tokenize BLOCK_OPEN             }
<0>                ")..."                    { tokenize BLOCK_CLOSE            }


<0>                "Once upon a time in"               { tokenize PROGRAM_INI  }
<0>                "and they lived happily ever after" { tokenize PROGRAM_FIN  }
<0>                "Once upon some other time in"      { tokenize FUNCTION_INI }
<0>                "or that is what they say"          { tokenize FUNCTION_FIN }

<0>                "And that's where"        { tokenize S_andthatswhere        }
<0>                "There was"               { tokenize S_therewas             }
<0>                "broke a"                 { tokenize S_brokea               }
<0>                "brought a"               { tokenize S_broughta             }
<0>                "comes from"              { tokenize S_comesfrom            }
<0>                "dreams of"               { tokenize S_dreamsof             }
<0>                "keeps dreaming of"       { tokenize S_keepsdreamingof      }
<0>                "made a"                  { tokenize S_madea                }
<0>                "made of"                 { tokenize S_madeof               }
<0>                "there was a"             { tokenize S_therewasa            }
<0>                "told that story"         { tokenize S_toldthatstory        }
<0>                "invented"                { tokenize S_invented             }
<0>                "it's a"                  { tokenize S_itsa                 }

<0>                "with"                    { tokenize WITH                   }
<0>                "your"                    { tokenize YOUR                   }
<0>                "of"                      { tokenize OF                     }
<0>                "either"                  { tokenize EITHER                 }
<0>                "to"                      { tokenize TO                     }
<0>                "when"                    { tokenize WHEN                   }
<0>                "otherwise"               { tokenize OTHERWISE              }
<0>                "times"                   { tokenize TIMES                  }

<0>                "bag"                     { tokenize TYPE_INT               }
<0>                "wallet"                  { tokenize TYPE_FLOAT             }
<0>                "letter"                  { tokenize TYPE_CHAR              }
<0>                "light"                   { tokenize TYPE_BOOL              }
<0>                "chain"                   { tokenize TYPE_ARRAY             }
<0>                "machine"                 { tokenize TYPE_STRUCT            }
<0>                "thing"                   { tokenize TYPE_UNION             }
<0>                "phrase"                  { tokenize TYPE_STRING            }
<0>                "direction"               { tokenize TYPE_POINTER           }

<0>                "and"                     { tokenize AND                    }
<0>                "or"                      { tokenize OR                     }
<0>                "("                       { tokenize OPEN_PAREN             }
<0>                "["                       { tokenize OPEN_BRACKETS          }
<0>                "{"                       { tokenize OPEN_BRACES            }
<0>                ")"                       { tokenize CLOSE_PAREN            }
<0>                "]"                       { tokenize CLOSE_BRACKETS         }
<0>                "}"                       { tokenize CLOSE_BRACES           }
<0>                "..."                     { tokenize ELLIPSIS               }
<0>                "."                       { tokenize PERIOD                 }
<0>                ","                       { tokenize COMMA                  }
<0>                ":"                       { tokenize COLON                  }
<0>                ";"                       { tokenize SEMICOLON              }
<0>                "?"                       { tokenize QUESTIONMARK           }
<0>                "!"                       { tokenize EXCLAMATION            }
<0>                "->"                      { tokenize ARROW_RIGHT            }
<0>                "+"                       { tokenize PLUS                   }
<0>                "-"                       { tokenize MINUS                  }
<0>                "=="                      { tokenize EQUAL                  }
<0>                "="                       { tokenize ASSIGNMENT             }
<0>                "*"                       { tokenize ASTERISK               }
<0>                "%"                       { tokenize PERCENT                }
<0>                "/"                       { tokenize SLASH                  }
<0>                "div"                     { tokenize DIV                    }
<0>                "/="                      { tokenize NOT_EQUAL              }
<0>                ">="                      { tokenize GREATER_EQUAL          }
<0>                "<="                      { tokenize LESS_EQUAL             }
<0>                ">"                       { tokenize GREATER                }
<0>                "<"                       { tokenize LESS                   }
<0>                "^"                       { tokenize POWER                  }

<0>                "--|"                     { embedComment `andBegin` commentLvl }
<commentLvl>       "--|"                     { embedComment                       }
<commentLvl>       "|--"                     { unembedComment                     }
<commentLvl>       .                         ;
<commentLvl>       \n                        { skip                               }
<0>                "--".*                    ;

<0>                "on"|"off"                { tokenizeBool  }
<0>                \'[a-z]\'                 { tokenizeChar  }
<0>                [0-9]+(\.[0-9]+)          { tokenizeFloat }
<0>                [0-9]+                    { tokenizeInt   }

<0>                \"                        { {-'"'-} enterString `andBegin` stringState }
<stringState>      \"                        { {-'"'-} leaveString `andBegin` 0           }
<stringState>      \\ [ntfrbv\\\'\"]         { {-'"'-} addCurrentToString                 }
<stringState>      \\ $white+                ;
<stringState>      \\ ?                      ;
<stringState>      \\ .                      { addEscapedToString }
<stringState>      .                         { addCurrentToString }

<0>                \n                        { skip               }
<0>                [a-zA-Z] [a-zA-Z\-\_0-9]* { tokenizeID         }
<0>                .                         { tokenizeError      }

{

-- An action for SutMonad to run on a given token
type TokenAction = SutoriInput -> Int -> SutMonad SutToken


-- ## Actions
-- #---------------------------------------------------------------------------

-- just ignore this token and scan another one
skip :: TokenAction
skip _ _ = lexerScan

-- ignore this token, but set the start code to a new value
begin :: Int -> TokenAction
begin code _ _ = do lexerSetSC code; lexerScan

-- perform an action for this token, and set the start code to a new value
andBegin :: TokenAction -> Int -> TokenAction
(action `andBegin` code) input len = do
  lexerSetSC code
  action input len

-- ### Token Getters
tokenize :: SutToken -> TokenAction
tokenize c _ _ = return c

tokenizeChar, tokenizeFloat, tokenizeInt, tokenizeID, tokenizeBool, tokenizeError :: TokenAction
tokenizeChar    (_, _, _, str)   len = return (SutTkChar  (take len str))
tokenizeFloat   (_, _, _, str)   len = return (SutTkFloat (read $ take len str))
tokenizeInt     (_, _, _, str)   len = return (SutTkInt   (read $ take len str))
tokenizeID      (_, _, _, str)   len = return (SutTkID    (take len str))
tokenizeBool    (_, _, _, "on")  len = return (SutTkBool  (True))
tokenizeBool    (_, _, _, "off") len = return (SutTkBool  (False))
tokenizeError   (_, _, _, input) len = do
  -- setLexerError True
  return $ SutTkError $ take len input

-- Set the current state code (startcode for alex)
setStateCode :: Int -> SutMonad ()
setStateCode c = get >>= \s -> put s{lexerStateCode=c}

-- ### Comments

-- Get current comment depth
getLexerCommentDepth :: SutMonad Int
getLexerCommentDepth = get >>= \SutState{lexerDepth=d} -> return d

-- Set current comment depth
setLexerCommentDepth :: Int -> SutMonad ()
setLexerCommentDepth d = get >>= \s -> put s{lexerDepth=d}

-- Go one level deeper
embedComment :: TokenAction
embedComment input len = do
  cd <- getLexerCommentDepth
  setLexerCommentDepth (cd + 1)
  skip input len

-- Go one level up
unembedComment :: TokenAction
unembedComment input len = do
  cd <- getLexerCommentDepth
  setLexerCommentDepth (cd - 1)
  when (cd == 1) (setStateCode 0)
  skip input len

-- ### Strings

-- Enters "string" state
enterString :: TokenAction
enterString _ _ = do
  setLexerStringState True
  setLexerStringValue ""
  lexerScan

-- Leaves "string" state
leaveString :: TokenAction
leaveString (p, _, _, input) len = do
  s <- getLexerStringValue
  setLexerStringState False
  return (SutTkString (reverse s))

-- Adds a given character to the current string
addCharToString :: Char -> TokenAction
addCharToString c _ _ = addCharToLexerStringValue c >> lexerScan
  where addCharToLexerStringValue :: Char -> SutMonad ()
        addCharToLexerStringValue c = get >>= \s@SutState{lexerString=ss} -> put s{lexerString=c:ss}

-- Adds an escaped character to the current string
addEscapedToString :: TokenAction
addEscapedToString i@(_, _, _, input) len = addCharToString (head $ drop 1 input) i len

-- Adds current character to current sring
addCurrentToString :: TokenAction
addCurrentToString i@(_, _, _, input) len = addCharToString (head input) i len

-- Know if currently on a string
getLexerStringState :: SutMonad Bool
getLexerStringState = get >>= \s@SutState{lexerStringOn=ss} -> return ss

-- Set the string state (if entering/leaving string)
setLexerStringState :: Bool -> SutMonad ()
setLexerStringState ss = get >>= \s -> put s{lexerStringOn=ss}

-- Get the currently built string
getLexerStringValue :: SutMonad String
getLexerStringValue = get >>= \s@SutState{lexerString=ss} -> return ss

-- Set the string value
setLexerStringValue :: String -> SutMonad ()
setLexerStringValue ss = get >>= \s -> put s{lexerString=ss}



-- Handling errors
-- #---------------------------------------------------------------------------

-- Log a lexical error
-- lexerError :: SutLog -> SutMonad a


-- Legacy:
-- lexerError :: String -> SutMonad a
-- lexerError msg = do
--     (p, c, _, input) <- alexGetInput
--     let cleanInput   = clean input
--         errorPrefix  = if null msg then "Lexer error" else trim msg
--     alexError (errorPrefix ++ " at " ++ showSut p ++ placeError input cleanInput c)
--   where
--     clean       = shorten . removeBr . trim
--     trim        = dropWhileEnd isSpace . dropWhile isSpace
--     removeBr s  = filter (/= '\r') (takeWhile (/='\n') s)
--     shorten  s  = if (length s > 30)
--                   then trim (take 30 s) ++ "..."
--                   else trim s
--     placeError s1 s2 c  = if (null s1)
--                           then " at end of file"
--                           else if (null s2)
--                                then " before end of line"
--                                else " on char " ++ show c ++ " before : '" ++ s2 ++ "'"
--
-- alexComplementError :: SutMonad a -> SutMonad (a, Maybe String)
-- alexComplementError a = \s -> case al s of
--                                    Right (s', x) -> Right (s', (x, Nothing))
--                                    Left  message -> Right (s, (undefined, Just message))



-- Runner
-- #---------------------------------------------------------------------------
-- getAlexResult = do
--   (t, m) <- alexComplementError lexerScan
--   when (isJust m) (lexerError (fromJust m))
--   return t
--
-- lexerHandleEOF tk = do
--   isString <- getLexerStringState
--   commentDepth <- getLexerCommentDepth
--   if ((not isString) && (commentDepth == 0))
--   then return [tk]
--   else if (isString)
--        then lexerError "String not closed"
--        else lexerError "Comment not closed"
--
-- lexerLoop = do
--   tk@(SutToken p tkc) <- getAlexResult
--   if (tkc == SutTkEOF)
--   then lexerHandleEOF tk
--   else lexerLoop >>= \tks -> return (tk:tks)
--
-- runAlexScan s = runAlex s lexerLoop


-- Scan for the next token
lexerScan :: SutMonad SutToken
lexerScan = do
  SutState{lexerStateCode=sc} <- get
  input <- lexerGetInput
  case alexScan input sc of
    AlexEOF -> return SutTkEOF
    AlexError (_,char,_,_) -> logError LexicalError >> return (SutTkError [char])
    AlexSkip  input' _ -> do
      lexerSetInput input'
      lexerScan
    AlexToken input' len action -> do
      lexerSetInput input'
      action (ignorePendingBytes input) len

runLexer :: String -> (SutState -> Either String (b, a)) -> Either String a
runLexer input f
  = case f initialSutoriState { lexerInput = input }
    of Left msg       -> Left msg
       Right ( _, a ) -> Right a

}
