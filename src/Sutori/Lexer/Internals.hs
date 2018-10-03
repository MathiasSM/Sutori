{-|
Description : Defines internal functions to mimic Alex wrappers while using "SutMonad" as the lexer monad
-}
module Sutori.Lexer.Internals where

import Control.Monad.State (get, put)
import Data.Bits           (shiftR, (.&.))
import Data.Char           (ord)
import Data.Word           (Word8)

import Sutori.Monad
import Sutori.SymTable

import Sutori.Lexer.Posn(SutPosn(SutPosn), initialPosn)


-- |Encode a Haskell String to a list of Word8 values, in UTF8 format.
-- Taken from Alex wrapper templates
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
  where go oc
          | oc <= 0x7f       = [oc]
          | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                              , 0x80 + oc .&. 0x3f ]
          | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                              , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                              , 0x80 + oc .&. 0x3f ]
          | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                              , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                              , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                              , 0x80 + oc .&. 0x3f ]


-- |The type of the scanner's food
-- (Current position, Previous character, pending bytes on current character, current input)
type SutoriInput = (SutPosn,     -- Current position,
                    Char,        -- Previous char
                    [Word8],     -- Pending bytes on current char
                    String)      -- Current input string


-- Lexer input handling
-------------------------
-- |Alias needed by Alex
type AlexInput = SutoriInput

-- |Needed by alex for look-back. We don't use look-back now
alexInputPrevChar :: SutoriInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

-- |Needed by alex to read
alexGetByte :: SutoriInput -> Maybe (Word8, SutoriInput)
alexGetByte (p,c,b:bs,s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[])  = Nothing
alexGetByte (p,_,[],c:s) = let p'     = lexerMove p c
                               (b:bs) = utf8Encode c
                            in p' `seq`  Just (b, (p', c, bs, s))

-- |Ignore the current character's pending bytes
ignorePendingBytes :: SutoriInput -> SutoriInput
ignorePendingBytes (p,c,_,s) = (p,c,[],s)

-- |Hard-coded tab size for positioning
sutTabSize :: Int
sutTabSize = 8

-- |Move the scanner position given a character
lexerMove :: SutPosn -> Char -> SutPosn
lexerMove (SutPosn a l c) '\t' = SutPosn (a+1)  l     (((c+sutTabSize-1) `div` sutTabSize)*sutTabSize+1)
lexerMove (SutPosn a l _) '\n' = SutPosn (a+1) (l+1)   1
lexerMove (SutPosn a l c) _    = SutPosn (a+1)  l     (c+1)

-- |Read input
lexerGetInput :: SutMonad SutoriInput
lexerGetInput = do
  SutState{ lexerPosn=pos, lexerChar=c, lexerBytes=bs, lexerInput=input } <- get
  return (pos,c,bs,input)

-- |Set remaining input
lexerSetInput :: SutoriInput -> SutMonad ()
lexerSetInput (pos,c,bs,input) = do
  oldState <- get
  put oldState{ lexerPosn=pos, lexerChar=c, lexerBytes=bs, lexerInput=input }
