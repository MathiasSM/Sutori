module Sutori.Logger where

import Sutori.Utils

newtype SutParserLog = SutParserLog {parserLog :: String}

instance Monoid SutParserLog where
  mempty = SutParserLog ""
  mappend (SutParserLog a) (SutParserLog b) = SutParserLog (a++b)

instance Show SutParserLog where
  show (SutParserLog a) = a

instance SutShow SutParserLog where
  showSut (SutParserLog a) = "[ERROR]\n"++a++"\n"
