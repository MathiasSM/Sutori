module Sutori.Utils
(
  SutPrint(..)
) where

class SutPrint a where
  printSut :: a -> String
