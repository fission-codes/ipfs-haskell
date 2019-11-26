{-# LANGUAGE UndecidableInstances #-}

module Network.IPFS.Local.Class
  ( MonadLocalIPFS
  , withIPFSProc
  , ipfsRun
  ) where

import Network.IPFS.Prelude
import           Network.IPFS.Types as IPFS
import qualified RIO.ByteString.Lazy as Lazy
import qualified Network.IPFS.Config as Config
import Network.IPFS.Internal.Process

class Monad m => MonadLocalIPFS m where
  withIPFSProc ::
               (ProcessConfig stdin stdout () -> m a)
               -> StreamIn  stdin
               -> StreamOut stdout
               -> [Opt]
               -> m a
  ipfsRun  :: [Opt] -> Lazy.ByteString -> m (ExitCode, Lazy.ByteString, Lazy.ByteString)

instance 
  ( HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath cfg
  , Has IPFS.Timeout cfg
  )
  => MonadLocalIPFS (RIO cfg) where
    withIPFSProc processor inStream outStream opts = do
      IPFS.BinPath ipfs <- Config.get
      IPFS.Timeout secs <- Config.get
      let opts' = ("--timeout=" <> show secs <> "s") : opts
      proc ipfs opts' <| processor
                      . setStdin  inStream
                      . setStdout outStream

    ipfsRun opts arg = withIPFSProc readProcess (byteStringInput arg) byteStringOutput opts
