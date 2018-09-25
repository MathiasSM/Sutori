module Sutori.Lexer.Internals where


import Control.Monad.State
import Control.Applicative as App (Applicative (..))
import qualified Control.Monad (ap)

import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Bits
import qualified Data.Map as Map
import qualified Data.Set as Set

import Sutori.Monad
import Sutori.SymTable

import Sutori.Lexer.Posn(SutPosn(SutPosn), initialPosn)


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

type AlexInput = SutoriInput


-- Lexer input handling
-------------------------
-- Needed by alex for look-back
alexInputPrevChar :: SutoriInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

-- Needed by alex to read
alexGetByte :: SutoriInput -> Maybe (Byte,SutoriInput)
alexGetByte (p,c,b:bs,s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[])  = Nothing
alexGetByte (p,_,[],c:s) = let p' = lexerMove p c
                               (b:bs) = utf8Encode c
                            in p' `seq`  Just (b, (p', c, bs, s))

ignorePendingBytes :: SutoriInput -> SutoriInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

-- Configuration for tab size
sutTabSize :: Int
sutTabSize = 8

-- Lexer move position given a character
lexerMove :: SutPosn -> Char -> SutPosn
lexerMove (SutPosn a l c) '\t' = SutPosn (a+1)  l     (((c+sutTabSize-1) `div` sutTabSize)*sutTabSize+1)
lexerMove (SutPosn a l _) '\n' = SutPosn (a+1) (l+1)   1
lexerMove (SutPosn a l c) _    = SutPosn (a+1)  l     (c+1)


-- Read input
lexerGetInput :: SutMonad SutoriInput
lexerGetInput = do
  SutState{ lexerPosn=pos, lexerChar=c, lexerBytes=bs, lexerInput=input } <- get
  return (pos,c,bs,input)

lexerSetInput :: SutoriInput -> SutMonad ()
lexerSetInput (pos,c,bs,input) = do
  oldState <- get
  put oldState{ lexerPosn=pos, lexerChar=c, lexerBytes=bs, lexerInput=input }

lexerSetSC :: Int -> SutMonad ()
lexerSetSC sc = do
  oldState <- get
  put oldState{ lexerStateCode=sc }
