{-# LANGUAGE UndecidableInstances #-}

module Network.IPFS.Local.Class
  ( MonadLocalIPFS
  , withIPFSProc
  , ipfsRun
  , getTimeout
  ) where

import Network.IPFS.Prelude

import qualified RIO.ByteString.Lazy as Lazy

import           Network.IPFS.Types  as IPFS
import           Network.IPFS.Internal.Process

class Monad m => MonadLocalIPFS m where
  withIPFSProc ::
               (ProcessConfig stdin stdout () -> m a)
               -> StreamIn  stdin
               -> StreamOut stdout
               -> [Opt]
               -> m a
  ipfsRun  :: [Opt] -> Lazy.ByteString -> m (ExitCode, Lazy.ByteString, Lazy.ByteString)
  getTimeout :: m IPFS.Timeout
