{-# LANGUAGE UndecidableInstances #-}

module Network.IPFS.Local.Class
  ( MonadLocalIPFS
  , withIPFSProc
  , ipfsRun
  ) where

import Network.IPFS.Prelude

import qualified RIO.ByteString.Lazy as Lazy

import           Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Internal.Process

import           Network.IPFS.Config  as Config

class Monad m => MonadLocalIPFS m where
  withIPFSProc ::
    (ProcessConfig stdin stdout () -> m a)
    -> StreamIn  stdin
    -> StreamOut stdout
    -> [Opt]
    -> m a
  ipfsRun ::
    [Opt]
    -> Lazy.ByteString
    -> m (Either Process.Error Process.RawMessage)

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

    ipfsRun opts arg = withIPFSProc readProcess (byteStringInput arg) byteStringOutput opts >>= \case
      (ExitSuccess, contents, _) ->
        return <| Right contents

      (ExitFailure _, _, stdErr)
        | Lazy.isSuffixOf "context deadline exceeded" stdErr -> do
            IPFS.Timeout secs <- Config.get
            return . Left <| Process.Timeout secs

        | otherwise ->
          return . Left <| Process.UnknownErr stdErr
