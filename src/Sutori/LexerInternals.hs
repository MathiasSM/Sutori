module Sutori.LexerInternals where


import Control.Applicative as App (Applicative (..))
import qualified Control.Monad (ap)

import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Bits

import Sutori.Monad


-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

type Byte = Word8
type SutoriInput = (SutPosn,     -- current position,
                    Char,        -- previous char
                    [Byte],      -- pending bytes on current char
                    String)      -- current input string




-- Lexer input handling
-------------------------
-- Needed by alex for look-back
alexInputPrevChar :: SutoriInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

-- Needed by alex to read
alexGetByte :: SutoriInput -> Maybe (Byte,SutoriInput)
alexGetByte (p,c,b:bs,s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],c:s)  = let p' = lexerMove p c
                                (b:bs) = utf8Encode c
                             in p' `seq`  Just (b, (p', c, bs, s))

ignorePendingBytes :: SutoriInput -> SutoriInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

-- Configuration for tab size
sutTabSize :: Int
sutTabSize = 8

-- Lexer start position
lexerStartPosn :: SutPosn
lexerStartPosn = SutPosn 0 1 1

-- Lexer move position given a character
lexerMove :: SutPosn -> Char -> SutPosn
lexerMove (SutPosn a l c) '\t' = SutPosn (a+1)  l     (((c+sutTabSize-1) `div` sutTabSize)*sutTabSize+1)
lexerMove (SutPosn a l _) '\n' = SutPosn (a+1) (l+1)   1
lexerMove (SutPosn a l c) _    = SutPosn (a+1)  l     (c+1)


-- Read input
lexerGetInput :: SutMonad SutoriInput
lexerGetInput = SutMonad $
  \s@SutState{ lexerPosn=pos, lexerChar=c, lexerBytes=bs, lexerInput=input } ->
    (s, (pos,c,bs,input))

lexerSetInput :: SutoriInput -> SutMonad ()
lexerSetInput (pos,c,bs,input) = SutMonad $
  \s -> case s{ lexerPosn=pos, lexerChar=c, lexerBytes=bs, lexerInput=input } of
          state__@SutState{} -> (state__, ())

lexerGetSC :: SutMonad Int
lexerGetSC = SutMonad $ \s@SutState{lexerSC=sc} -> (s, sc)

lexerSetSC :: Int -> SutMonad ()
lexerSetSC sc = SutMonad $ \s -> (s{lexerSC=sc}, ())


-- Run lexer
-- -------------------------------------------------------

-- Runs the lexer on an input string
runLexer :: String -> SutMonad a -> a
runLexer input (SutMonad f)
   = f SutState { lexerPosn       = lexerStartPosn,
                  lexerInput      = input,
                  lexerChar       = '\n',
                  lexerBytes      = [],
                  lexerSC         = 0,
                  parserTable     = SymTable Map.empty,
                  parserStack     = [0],
                  parserScopes    = Set.insert 0 Set.empty,
                  parserNextScope = 0
                }

-- Runs the lexer scanner
lexerScan = do
  input <- lexerGetInput
  sc <- lexerGetSC
  case alexScan input sc of
    AlexEOF -> alexEOF
    AlexError (posn,_,_,_) -> sutoriError $ "Lexical error at " ++ showSut posn
    AlexSkip  input' _len -> do
        lexerSetInput input'
        lexerScan
    AlexToken input' len action -> do
        lexerSetInput input'
        action (ignorePendingBytes input) len



-- Lexer Actions (productions of lexer tokens)
-- -------------------------------------------------------

type LexerAction result = SutoriInput -> Int -> SutMonad result

-- just ignore this token and scan another one
skip :: LexerAction result
skip _input _len = lexerScan

-- ignore this token, but set the start code to a new value
begin :: Int -> LexerAction result
begin code _input _len = do lexerSetSC code; lexerScan

-- perform an action for this token, and set the start code to a new value
andBegin :: LexerAction result -> Int -> LexerAction result
(action `andBegin` code) input__ len = do
  lexerSetSC code
  action input__ len

token :: (SutoriInput -> Int -> SutToken) -> LexerAction SutToken
token t input__ len = return (t input__ len)
