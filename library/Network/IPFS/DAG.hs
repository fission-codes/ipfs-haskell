module Network.IPFS.DAG
  ( put
  , putNode
  ) where

import Network.IPFS.Prelude
import Network.IPFS.Class

import qualified Network.IPFS.Internal.UTF8 as UTF8
import           Data.ByteString.Lazy.Char8 as CL
import qualified RIO.ByteString.Lazy as Lazy

import           Network.IPFS.Error          as IPFS.Error
import           Network.IPFS.Types          as IPFS
import           Network.IPFS.DAG.Node.Types as DAG

put :: 
  MonadLocalIPFS m
  => Lazy.ByteString
  -> m (Either IPFS.Error.Add IPFS.CID)
put raw = ipfsRun ["dag", "put", "-f", "dag-pb"] raw >>= \case
  (ExitSuccess, result, _) ->
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

  (ExitFailure _, _, err) ->
    pure . Left . UnknownAddErr <| UTF8.textShow err

putNode :: 
  MonadLocalIPFS m
  => DAG.Node
  -> m (Either IPFS.Error.Add IPFS.CID)
putNode node = put <| encode node
