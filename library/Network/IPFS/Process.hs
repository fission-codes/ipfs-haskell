module Network.IPFS.Process (runProc) where

import Network.IPFS.Prelude

import           Network.IPFS.Types         as IPFS
import           Network.IPFS.Internal.Process

runProc ::
  RIOProc cfg m
  => (ProcessConfig stdin stdout () -> m a)
  -> FilePath
  -> StreamIn  stdin
  -> StreamOut stdout
  -> [Opt]
  -> m a
runProc processor binPath inStream outStream opts =
  proc binPath opts <| processor
                  . setStdin  inStream
                  . setStdout outStream
