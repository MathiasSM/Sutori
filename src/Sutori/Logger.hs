{-|
Description : Provides API for logging information, errors and the like

Provides a common framework (class) to 'showSut' sutori components/elements/constructs.
-}
module Sutori.Logger
( SutLog(SutLogLeave, SutLogNode)
, SutShow(showSut)
, SutLogger(SutLogger,logInfo, logError)
, fromLeave
) where

import Sutori.Logger.Log
import Sutori.Logger.Writer
