-- |

module Network.IPFS.Path.Escaped.Types (Escaped (..)) where

-- import           Data.Swagger               (ToSchema (..))
-- import           Servant
--
-- import qualified Network.IPFS.Internal.UTF8 as UTF8
import           Network.IPFS.Prelude

-- | Verbatim file path, includes spaces
--
-- Exmaple
--
-- > "~/Desktop/foo bar/baz.png"
newtype Escaped = Escaped { unescape :: Text }
  deriving          ( Eq
                    , Generic
                    , Ord
                    , Show
                    )
  deriving newtype  ( IsString
                    -- , ToHttpApiData
                    -- , ToSchema
                    )

-- instance MimeRender PlainText Escaped where
  -- mimeRender _ = UTF8.textToLazyBS . unescape

-- instance MimeRender OctetStream Escaped where
  -- mimeRender _ = UTF8.textToLazyBS . unescape
