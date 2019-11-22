module Network.IPFS.Internal.Process
  ( RIOProc
  , StreamIn
  , StreamOut
  ) where

import Network.IPFS.Prelude

type RIOProc cfg m = ( MonadRIO          cfg m
                     , HasProcessContext cfg
                     , HasLogFunc        cfg
                     )

type StreamIn  stdin  = StreamSpec 'STInput  stdin
type StreamOut stdout = StreamSpec 'STOutput stdout
