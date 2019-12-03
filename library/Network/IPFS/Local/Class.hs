{-# LANGUAGE UndecidableInstances #-}

module Network.IPFS.Local.Class
  ( MonadLocalIPFS
  , IPFSError (..)
  , RawMessage
  , withIPFSProc
  , ipfsRun
  ) where

import Network.IPFS.Prelude

import qualified RIO.ByteString.Lazy as Lazy
import           Data.ByteString.Lazy.Char8 as CL

import           Network.IPFS.Types  as IPFS
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
    -> m (Either IPFSError RawMessage)

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
            return . Left <| ErrTimeout secs

        | otherwise ->
          return . Left <| ErrUnknown stdErr


data IPFSError
  = ErrTimeout Natural
  | ErrUnknown RawMessage
  deriving ( Exception
           , Eq
           , Generic
           , Show
           )

type RawMessage = CL.ByteString
