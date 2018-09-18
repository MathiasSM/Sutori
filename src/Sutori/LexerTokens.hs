module Sutori.LexerTokens where

import Sutori.Logger(SutShow(showSut), SutLog(SutLogLeave, SutLogNode))
import Sutori.Monad(SutPosn)

-- A Sutori Token. Can be shown with SutShow
data SutToken = SutToken { tokenPosn :: SutPosn, tokenClass :: SutTokenClass } deriving (Eq,Show)
instance SutShow SutToken where
  showSut (SutToken _ SutTkEOF) = SutLogLeave "Token EOF"
  showSut tk  = SutLogNode "Token:" [showSut $ tokenClass tk, showSut $ tokenPosn tk]

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

    SutTkBool   { tokenBool :: Bool }     |
    SutTkChar   { tokenChar :: String }   |
    SutTkFloat  { tokenFloat :: Float }   |
    SutTkInt    { tokenInt :: Int }       |
    SutTkString { tokenString :: String } |

    SutTkError  { tokenError :: String }  |

    SutTkID     { tokenID :: String }
  deriving (Eq,Show)

instance SutShow SutTokenClass where
  showSut = SutLogLeave . show
