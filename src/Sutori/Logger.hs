module Sutori.Logger
( SutLog(SutLogLeave, SutLogNode)
, SutShow(showSut)
, SutLogger(getLog)
) where


-- Simple tree data structure to allow pretty printing of sutori logs
data SutLog = SutLogLeave String
            | SutLogNode String [SutLog]
            deriving Show


-- The logger is just a string for the time being
newtype SutLogger = SutLogger {getLog :: [SutLog]}

instance Monoid SutLogger where
  mempty = SutLogger []
  mappend (SutLogger a) (SutLogger b) = SutLogger (a++b)

instance Show SutLogger where
  show (SutLogger a) = show a


-- Interface for showing Sutori constructs (token, actions, tables, ...)
class SutShow a where
  showSut :: a -> SutLog
