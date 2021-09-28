module Network.IPFS.Remote.Error (StatError (..)) where

import           Servant.Client

import           Network.IPFS.Prelude

import           Network.IPFS.Stat.Error

data StatError
  = WebError ClientError
  | SizeError OverflowDetected
  deriving (Show, Eq)
