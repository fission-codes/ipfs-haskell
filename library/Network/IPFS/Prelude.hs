-- | A custom @Prelude@-like module for this project
module Network.IPFS.Prelude
  ( module Control.Lens
  , module Control.Monad.Logger
  , module Data.Aeson
  , module Flow
  , module RIO
  , module RIO.Process
  , identity
  , logInfo
  , logDebug
  , logWarn
  , logError
  , logOther
  ) where

import Control.Lens         ((%~), (.~), (?~), (^?))
import Control.Monad.Logger (MonadLogger (..), ToLogStr (..), LogLevel (..), logWithoutLoc)
import Data.Aeson

import Network.IPFS.Internal.Orphanage.Utf8Builder ()

import Flow

import RIO.Process
import RIO hiding ( Handler
                  , LogLevel (..)
                  , LogSource
                  , logDebug
                  , logDebugS
                  , logError
                  , logErrorS
                  , logInfo
                  , logInfoS
                  , logOther
                  , logOtherS
                  , logWarn
                  , logWarnS
                  , id
                  , timeout
                  , ($)
                  , (&)
                  )

identity :: a -> a
identity a = a

logInfo :: (ToLogStr msg, MonadLogger m) => msg -> m ()
logInfo = logWithoutLoc "" LevelInfo

logDebug :: (ToLogStr msg, MonadLogger m) => msg -> m ()
logDebug = logWithoutLoc "" LevelDebug

logWarn :: (ToLogStr msg, MonadLogger m) => msg -> m ()
logWarn = logWithoutLoc "" LevelWarn

logError :: (ToLogStr msg, MonadLogger m) => msg -> m ()
logError = logWithoutLoc "" LevelError

logOther :: (ToLogStr msg, MonadLogger m) => LogLevel -> msg -> m ()
logOther = logWithoutLoc ""
