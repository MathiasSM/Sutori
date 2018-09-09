{
-- # PRE CODE BLOCK (Module and imports)
-- #===========================================================================
module Sutori.Lexer where

import Control.Monad
import Numeric ( readDec )
import Data.Maybe
import Data.Char ( chr, isSpace )
import Data.List

import Sutori.Utils


-- # TOKEN DEFINITIONS
-- #===========================================================================

}

%wrapper "monadUserState"

tokens :-

<0>                $white+                              ;

<0>                "...("                               { getTk BLOCK_OPEN                               }
<0>                ")..."                               { getTk BLOCK_CLOSE                              }


<0>                "Once upon a time in"                { getTk PROGRAM_INI                              }
<0>                "and they lived happily ever after"  { getTk PROGRAM_FIN                              }
<0>                "Once upon some other time in"       { getTk FUNCTION_INI                             }
<0>                "or that is what they say"           { getTk FUNCTION_FIN                             }

<0>                "And that's where"                   { getTk S_andthatswhere                          }
<0>                "There was"                          { getTk S_therewas                               }
<0>                "broke a"                            { getTk S_brokea                                 }
<0>                "brought a"                          { getTk S_broughta                               }
<0>                "comes from"                         { getTk S_comesfrom                              }
<0>                "dreams of"                          { getTk S_dreamsof                               }
<0>                "keeps dreaming of"                  { getTk S_keepsdreamingof                        }
<0>                "made a"                             { getTk S_madea                                  }
<0>                "made of"                            { getTk S_madeof                                 }
<0>                "there was a"                        { getTk S_therewasa                              }
<0>                "told that story"                    { getTk S_toldthatstory                          }
<0>                "invented"                           { getTk S_invented                               }
<0>                "it's a"                             { getTk S_itsa                                   }

<0>                "with"                               { getTk WITH                                     }
<0>                "your"                               { getTk YOUR                                     }
<0>                "of"                                 { getTk OF                                       }
<0>                "either"                             { getTk EITHER                                   }
<0>                "to"                                 { getTk TO                                       }
<0>                "when"                               { getTk WHEN                                     }
<0>                "otherwise"                          { getTk OTHERWISE                                }
<0>                "times"                              { getTk TIMES                                    }

<0>                "bag"                                { getTk TYPE_INT                                 }
<0>                "wallet"                             { getTk TYPE_FLOAT                               }
<0>                "letter"                             { getTk TYPE_CHAR                                }
<0>                "light"                              { getTk TYPE_BOOL                                }
<0>                "chain"                              { getTk TYPE_ARRAY                               }
<0>                "machine"                            { getTk TYPE_STRUCT                              }
<0>                "thing"                              { getTk TYPE_UNION                               }
<0>                "phrase"                             { getTk TYPE_STRING                              }
<0>                "direction"                          { getTk TYPE_POINTER                             }

<0>                "and"                                { getTk AND                                      }
<0>                "or"                                 { getTk OR                                       }
<0>                "("                                  { getTk OPEN_PAREN                               }
<0>                "["                                  { getTk OPEN_BRACKETS                            }
<0>                "{"                                  { getTk OPEN_BRACES                              }
<0>                ")"                                  { getTk CLOSE_PAREN                              }
<0>                "]"                                  { getTk CLOSE_BRACKETS                           }
<0>                "}"                                  { getTk CLOSE_BRACES                             }
<0>                "..."                                { getTk ELLIPSIS                                 }
<0>                "."                                  { getTk PERIOD                                   }
<0>                ","                                  { getTk COMMA                                    }
<0>                ":"                                  { getTk COLON                                    }
<0>                ";"                                  { getTk SEMICOLON                                }
<0>                "?"                                  { getTk QUESTIONMARK                             }
<0>                "!"                                  { getTk EXCLAMATION                              }
<0>                "->"                                 { getTk ARROW_RIGHT                              }
<0>                "+"                                  { getTk PLUS                                     }
<0>                "-"                                  { getTk MINUS                                    }
<0>                "=="                                 { getTk EQUAL                                    }
<0>                "="                                  { getTk ASSIGNMENT                               }
<0>                "*"                                  { getTk ASTERISK                                 }
<0>                "%"                                  { getTk PERCENT                                  }
<0>                "/"                                  { getTk SLASH                                    }
<0>                "div"                                { getTk DIV                                      }
<0>                "/="                                 { getTk NOT_EQUAL                                }
<0>                ">="                                 { getTk GREATER_EQUAL                            }
<0>                "<="                                 { getTk LESS_EQUAL                               }
<0>                ">"                                  { getTk GREATER                                  }
<0>                "<"                                  { getTk LESS                                     }
<0>                "^"                                  { getTk POWER                                    }

<0>                "--|"                                { embedComment `andBegin` state_comment          }
<state_comment>    "--|"                                { embedComment                                   }
<state_comment>    "|--"                                { unembedComment                                 }
<state_comment>    .                                    ;
<state_comment>    \n                                   { skip                                           }
<0>                "--".*                               ;

<0>                "on"|"off"                           { getTkBool                                      }
<0>                \'[a-z]\'                            { getTkChar                                      }
<0>                [0-9]+(\.[0-9]+)                     { getTkFloat                                     }
<0>                [0-9]+                               { getTkInteger                                   }

<0>                \"                                   { {-'"'-} enterString `andBegin` state_string    }
<state_string>     \"                                   { {-'"'-} leaveString `andBegin` 0               }
<state_string>     \\ [ntfrbv\\\'\"]                    { {-'"'-} addCurrentToString                     }
<state_string>     \\ $white+                           ;
<state_string>     \\ ?                                 ;
<state_string>     \\ .                                 { addEscapedToString                             }
<state_string>     .                                    { addCurrentToString                             }

<0>                \n                                   { skip                                           }
<0>                [a-zA-Z] [a-zA-Z\-\_0-9]*            { getTkId                                        }
<0>                .                                    { getTkError                                     }

{
-- # POST CODE BLOCK (Data definitions and functions)
-- #===========================================================================

-- ## DATA DEFINITIONS
-- #---------------------------------------------------------------------------

-- AlexPosn can be shown with SutShow
instance SutShow AlexPosn where
  showSut (AlexPn _ line col) = show line ++ ":" ++ show col

-- An action for Alex to run on a given token
type GetTokenAction = AlexInput -> Int -> Alex SutToken

-- A Sutori Token. Can be shown with SutShow
data SutToken = SutToken { getPosn :: AlexPosn, getToken :: SutTokenClass } deriving (Eq,Show)
instance SutShow SutToken where
  showSut (SutToken _ SutTkEOF) = "Token EOF"
  showSut (SutToken p cl)  = "Token (" ++ showSut cl ++ "): " ++ showSut p


fakeToken :: SutTokenClass -> SutToken
fakeToken tkc = SutToken (AlexPn 0 0 0) tkc

-- Alex EOF token (Sutori EOF)
alexEOF :: Alex SutToken
alexEOF = return $ SutToken undefined SutTkEOF

-- Sutori token classes. Can be shown with SutShow
data SutTokenClass =
    SutTkEOF            |

    BLOCK_OPEN          |
    BLOCK_CLOSE         |

    PROGRAM_INI         |
    PROGRAM_FIN         |
    FUNCTION_INI        |
    FUNCTION_FIN        |

    S_andthatswhere     |
    S_therewas          |
    S_brokea            |
    S_broughta          |
    S_comesfrom         |
    S_dreamsof          |
    S_keepsdreamingof   |
    S_madea             |
    S_madeof            |
    S_therewasa         |
    S_toldthatstory     |
    S_invented          |
    S_itsa              |

    TYPE_INT            |
    TYPE_FLOAT          |
    TYPE_CHAR           |
    TYPE_BOOL           |
    TYPE_ARRAY          |
    TYPE_STRUCT         |
    TYPE_UNION          |
    TYPE_STRING         |
    TYPE_POINTER        |

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
    QUESTIONMARK        |
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

    SutTkBool   { getValueBool :: Bool }     |
    SutTkChar   { getValueChar :: String }   |
    SutTkFloat  { getValueFloat :: Float }   |
    SutTkInt    { getValueInt :: Int }       |
    SutTkString { getString :: String } |

    SutTkError  { getError :: String }  |

    SutTkId     String
  deriving (Eq,Show)

instance SutShow SutTokenClass where
  showSut = show


-- Alex state to keep track of comment depth and string status
data AlexUserState = AlexUserState {
  lexerErrorTk       :: Bool,
  lexerCommentDepth  :: Int,
  lexerStringState   :: Bool,
  lexerStringValue   :: String
}
-- Default values:
alexInitUserState = AlexUserState {
  lexerErrorTk       = False,
  lexerCommentDepth  = 0,
  lexerStringState   = False,
  lexerStringValue   = ""
}



-- ## Actions
-- #---------------------------------------------------------------------------



-- ### Errors
getLexerError :: Alex Bool
getLexerError = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerErrorTk ust)

setLexerError :: Bool -> Alex ()
setLexerError ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerErrorTk=ss}}, ())



-- ### Token Getters
getTk :: SutTokenClass -> GetTokenAction
getTk c (p, _, _, str) len = return (SutToken p c)

getTkChar    (p, _, _, str)   len = return (SutToken p (SutTkChar  (take len str)))
getTkFloat   (p, _, _, str)   len = return (SutToken p (SutTkFloat (read $ take len str)))
getTkInteger (p, _, _, str)   len = return (SutToken p (SutTkInt   (read $ take len str)))
getTkId      (p, _, _, str)   len = return (SutToken p (SutTkId    (take len str)))
getTkBool    (p, _, _, "on")  len = return (SutToken p (SutTkBool  (True)))
getTkBool    (p, _, _, "off") len = return (SutToken p (SutTkBool  (False)))
getTkError   (p, _, _, input) len = setLexerError True >> (return $ SutToken p $ SutTkError $ take len input)



-- ### Comments

-- Get current comment depth
getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

-- Set current comment depth
setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())

-- Go one level deeper
embedComment :: GetTokenAction
embedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip input len

-- Go one level up
unembedComment :: GetTokenAction
unembedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode 0)
       skip input len



-- ### Strings

-- Enters "string" state
enterString :: GetTokenAction
enterString _ _ =
    do setLexerStringState True
       setLexerStringValue ""
       alexMonadScan

-- Leaves "string" state
leaveString :: GetTokenAction
leaveString (p, _, _, input) len =
    do s <- getLexerStringValue
       setLexerStringState False
       return (SutToken p (SutTkString (reverse s)))


-- Adds a given character to the current string
addCharToString :: Char -> GetTokenAction
addCharToString c _ _ = addCharToLexerStringValue c >> alexMonadScan
  where
    addCharToLexerStringValue :: Char -> Alex ()
    addCharToLexerStringValue c = Alex state
    state s = Right (s{ alex_ust=(alex_ust s) {lexerStringValue=c:lexerStringValue (alex_ust s)}}, ()) -- TODO: WTF

-- Adds an escaped character to the current string
addEscapedToString :: GetTokenAction
addEscapedToString i@(_, _, _, input) len = addCharToString (head $ drop 1 input) i len

-- Adds current character to current sring
addCurrentToString :: GetTokenAction
addCurrentToString i@(_, _, _, input) len = addCharToString (head input) i len


getLexerStringState :: Alex Bool
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState ust)

setLexerStringState :: Bool -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringState=ss}}, ())

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}}, ())



-- Handling errors
-- #---------------------------------------------------------------------------

-- Says if the given token is valid (not an error) or not
isValid :: SutToken -> Bool
isValid (SutToken _ (SutTkError _))  = False
isValid _                            = True


lexerError :: String -> Alex a
lexerError msg = do
    (p, c, _, input) <- alexGetInput
    let cleanInput   = clean input
        errorPrefix  = if null msg then "Lexer error" else trim msg
    alexError (errorPrefix ++ " at " ++ showSut p ++ placeError input cleanInput c)
  where
    clean       = shorten . removeBr . trim
    trim        = dropWhileEnd isSpace . dropWhile isSpace
    removeBr s  = filter (/= '\r') (takeWhile (/='\n') s)
    shorten  s  = if (length s > 30)
                  then trim (take 30 s) ++ "..."
                  else trim s
    placeError s1 s2 c  = if (null s1)
                          then " at end of file"
                          else if (null s2)
                               then " before end of line"
                               else " on char " ++ show c ++ " before : '" ++ s2 ++ "'"


alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) = Alex (\s -> case al s of
                                                 Right (s', x) -> Right (s', (x, Nothing))
                                                 Left  message -> Right (s, (undefined, Just message)))



-- Runner
-- #---------------------------------------------------------------------------
getAlexResult = do
  (t, m) <- alexComplementError alexMonadScan
  when (isJust m) (lexerError (fromJust m))
  return t

lexerHandleEOF tk = do
  isString <- getLexerStringState
  commentDepth <- getLexerCommentDepth
  if ((not isString) && (commentDepth == 0))
  then return [tk]
  else if (isString)
       then lexerError "String not closed"
       else lexerError "Comment not closed"

lexerLoop = do
  tk@(SutToken p tkc) <- getAlexResult
  if (tkc == SutTkEOF)
  then lexerHandleEOF tk
  else lexerLoop >>= \tks -> return (tk:tks)

runAlexScan s = runAlex s lexerLoop

}
