{-# LANGUAGE UndecidableInstances #-}

module Network.IPFS.Local.Class
  ( MonadLocalIPFS
  , withIPFSProc
  , ipfsRun
  ) where

import Network.IPFS.Prelude

import qualified RIO.ByteString.Lazy as Lazy
import           Data.ByteString.Lazy.Char8 as CL
import qualified Network.IPFS.Internal.UTF8       as UTF8

import           Network.IPFS.Types  as IPFS
import           Network.IPFS.Internal.Process

import           Network.IPFS.Config  as Config

class MonadIO m => MonadLocalIPFS m where
  withIPFSProc ::
    (ProcessConfig stdin stdout () -> m a)
    -> StreamIn  stdin
    -> StreamOut stdout
    -> [Opt]
    -> m a
  ipfsRun ::
    MonadRIO cfg m
    => Has IPFS.Timeout cfg
    => [Opt]
    -> Lazy.ByteString
    -- -> m (ExitCode, Lazy.ByteString, Lazy.ByteString)
    -> m (Either IPFSError CL.ByteString)

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
            return . Left <| ErrTimeout cid <| getSeconds timeout

        | otherwise ->
          return . Left. Unknown <| UTF8.textShow stdErr


data IPFSError
  = ErrTimeout CID
  | ErrPeerUnavailable Peer
  | ErrUnknown RawMessage

type RawMessage = Text
