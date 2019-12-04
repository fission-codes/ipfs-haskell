module Network.IPFS.DAG
  ( put
  , putNode
  ) where

import Network.IPFS.Prelude
import Network.IPFS.Local.Class as IPFS

import qualified Network.IPFS.Internal.UTF8 as UTF8
import           Data.ByteString.Lazy.Char8 as CL
import qualified RIO.ByteString.Lazy        as Lazy

import           Network.IPFS.Add.Error      as IPFS.Add
import           Network.IPFS.Types          as IPFS
import           Network.IPFS.DAG.Node.Types as DAG

put :: 
  MonadLocalIPFS m
  => Lazy.ByteString
  -> m (Either IPFS.Add.Error IPFS.CID)
put raw = IPFS.runLocal ["dag", "put", "-f", "dag-pb"] raw >>= \case
  Right result -> 
    case CL.lines result of
      [cid] ->
        cid
          |> UTF8.textShow
          |> UTF8.stripN 1
          |> mkCID
          |> Right
          |> return

      bad ->
        pure . Left . UnexpectedOutput <| UTF8.textShow bad

  Left err -> 
    pure . Left . UnknownAddErr <| UTF8.textShow err

putNode :: 
  MonadLocalIPFS m
  => DAG.Node
  -> m (Either IPFS.Add.Error IPFS.CID)
putNode node = put <| encode node
