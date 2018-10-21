{-|
Description : Defines ways to log information, errors and the like

Provides a common framework (class) to 'showSut' sutori components/elements/constructs.
Right now, error codes are defined here.
-}
module Sutori.Logger.Writer
( SutLogger(SutLogger,logInfo, logError)
) where

import Sutori.Error.Error

import Sutori.Logger.Log

-- |Sutori logger, where we register logs for 'reasons', via a WriterT
data SutLogger = SutLogger {
  logInfo :: [SutLog],                -- ^ The ``info'' log
  logWarning :: [SutLog],             -- ^ The ``warning'' log
  logError :: [(SutError, SutLog)]    -- ^ The ``error'' log
}

-- |SutLogger must be a Monoid to be used with WriterT
instance Monoid SutLogger where
  -- |An empty logger, is the set of empty logs
  mempty = SutLogger [] [] []
  -- |Joining logs is concatenating the every pair of lists
  mappend (SutLogger info warn err) (SutLogger info' warn' err') =
    SutLogger (info++info') (warn++warn') (err++err')

-- |The way we print the logs is to print info logs, then error logs.
instance Show SutLogger where
  show (SutLogger info warn err) = concatMap show info
                                 ++ concatMap show warn
                                 ++ concatMap show err
