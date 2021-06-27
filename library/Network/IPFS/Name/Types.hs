module Network.IPFS.Name.Types (Name (..)) where

import qualified RIO.Text             as Text

import           Data.Swagger         (ToParamSchema, ToSchema (..))
import           Servant

import           Network.IPFS.Prelude

newtype Name = Name { unName :: String }
  deriving          ( Eq
                    , Generic
                    , Show
                    , Ord
                    )
  deriving newtype  ( IsString
                    , ToSchema
                    , ToParamSchema
                    )

instance Display Name where
  display = displayShow

instance ToJSON Name where
  toJSON (Name n) = toJSON n

instance FromJSON Name where
  parseJSON = withText "IPFSName" (pure . Name . Text.unpack)

instance FromHttpApiData Name where
  parseUrlPiece = \case
    ""  -> Left "Empty Name field"
    txt -> Right . Name $ Text.unpack txt
