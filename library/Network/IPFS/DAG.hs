module Network.IPFS.DAG
  ( put
  , putNode
  ) where

import Network.Prelude

import qualified Network.Internal.UTF8 as UTF8
import           Data.ByteString.Lazy.Char8 as CL
import qualified RIO.ByteString.Lazy as Lazy

import qualified Network.IPFS.Process        as IPFS.Proc
import           Network.IPFS.Error          as IPFS.Error
import           Network.IPFS.Types          as IPFS
import           Network.IPFS.DAG.Node.Types as DAG

put :: 
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath  cfg
  , Has IPFS.Timeout  cfg
  )
  => Lazy.ByteString
  -> m (Either IPFS.Error.Add IPFS.CID)
put raw = IPFS.Proc.run ["dag", "put", "-f", "dag-pb"] raw >>= \case
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
  ( MonadRIO cfg m
  , HasProcessContext cfg
  , HasLogFunc cfg
  , Has IPFS.BinPath  cfg
  , Has IPFS.Timeout  cfg
  )
  => DAG.Node
  -> m (Either IPFS.Error.Add IPFS.CID)
putNode node = put <| encode node
