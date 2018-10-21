{-|
Description : API for error handling (internal or user-facing errors)
-}
module Sutori.Error
( SutError(..)
, logError
, lexerError
, parserError
, typeError
, undefinedError
, duplicateSymbolError
, argumentsNumberError
) where

import Sutori.Error.Error
import Sutori.Error.Report
import Sutori.Error.Logger
