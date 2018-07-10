module Sutori.Utils
(
  SutShow(..)
) where

class SutShow a where
  showSut :: a -> String
