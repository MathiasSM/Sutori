module Sutori.Logger
( SutLog(SutLogLeave, SutLogNode)
, SutShow(showSut)
, SutLogger(SutLogger, getLog)
, fromLeave
) where


-- Simple tree data structure to allow pretty printing of sutori logs
data SutLog = SutLogLeave String
            | SutLogNode String [SutLog]

instance Show SutLog where
  show = showNested 0

showNested :: Int -> SutLog -> String
showNested lvl (SutLogLeave s) = showIndent lvl ++ s ++ "\n"
showNested lvl (SutLogNode s n) = showIndent lvl ++ s ++ "\n" ++ concatMap (showNested (lvl+1)) n

showIndent n = (concat . replicate n) "  "

-- The logger is just a string for the time being
newtype SutLogger = SutLogger {getLog :: String}
instance Monoid SutLogger where
  mempty = SutLogger ""
  mappend (SutLogger a) (SutLogger b) = SutLogger (a++b)
instance Show SutLogger where
  show (SutLogger a) = a
-- newtype SutLogger = SutLogger {getLog :: [SutLog]}
--
-- instance Monoid SutLogger where
--   mempty = SutLogger []
--   mappend (SutLogger a) (SutLogger b) = SutLogger (a++b)
--
-- -- We'll print the log this way, probably. Need a better formatting
-- instance Show SutLogger where
--   show (SutLogger a) = show a

-- Extract log strings
fromLeave :: SutLog -> String
fromLeave (SutLogLeave l) = l

-- Interface for showing Sutori constructs (token, actions, tables, ...)
class SutShow a where
  showSut :: a -> SutLog
