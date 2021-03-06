module Network.IPFS.Client.Param
  ( CID'
  , IsRecursive
  ) where

import           Servant

import           Network.IPFS.CID.Types

type CID' = QueryParam' '[Required, Strict] "arg" CID
type IsRecursive = QueryFlag "recursive"
