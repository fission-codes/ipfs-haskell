module Network.IPFS.Process
  ( run
  , run'
  ) where

import qualified RIO.ByteString.Lazy as Lazy

import           Network.IPFS.Prelude
import qualified Network.IPFS.Config as Config
import           Network.IPFS.Internal.Process
import           Network.IPFS.Types as IPFS

run :: RIOProc cfg m
    => Has IPFS.BinPath cfg
    => Has IPFS.Timeout cfg
    => [Opt]
    -> Lazy.ByteString
    -> m (ExitCode, Lazy.ByteString, Lazy.ByteString)
run opts arg = runBS (byteStringInput arg) opts

run' :: RIOProc cfg m
     => Has IPFS.BinPath cfg
     => Has IPFS.Timeout cfg
     => [Opt]
     -> m (ExitCode, Lazy.ByteString, Lazy.ByteString)
run' = runBS createPipe

runBS :: RIOProc cfg m
      => Has IPFS.BinPath cfg
      => Has IPFS.Timeout cfg
      => StreamIn stdin
      -> [Opt]
      -> m (ExitCode, Lazy.ByteString, Lazy.ByteString)
runBS inStream = ipfsProc readProcess inStream byteStringOutput

ipfsProc :: RIOProc          cfg m
         => Has IPFS.BinPath cfg
         => Has IPFS.Timeout cfg
         => (ProcessConfig stdin stdout () -> m a)
         -> StreamIn  stdin
         -> StreamOut stdout
         -> [Opt]
         -> m a
ipfsProc processor inStream outStream opts = do
  IPFS.BinPath ipfs <- Config.get
  IPFS.Timeout secs <- Config.get
  let opts' = ("--timeout=" <> show secs <> "s") : opts
  proc ipfs opts' <| processor
                   . setStdin  inStream
                   . setStdout outStream
