module Network.IPFS.Client.Pin
  ( API
  , AddAPI
  , RemoveAPI
  , Response (..)
  ) where

import Servant

import           Network.IPFS.CID.Types
import qualified Network.IPFS.Client.Param as Param
import           Network.IPFS.Prelude

type API = AddAPI :<|> RemoveAPI

type AddAPI
  = "add"
    :> Param.CID
    :> Post '[JSON] Response

type RemoveAPI
  = "rm"
    :> Param.CID
    :> Param.IsRecursive
    :> Delete '[JSON] Response

newtype Response = Response { cids :: [CID] }
  deriving (Eq, Show)

instance FromJSON Response where
  parseJSON = withObject "Pin Response" \obj ->
    Response <$> obj .: "Pins"
