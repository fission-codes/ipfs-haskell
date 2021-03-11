module Network.IPFS.Client.DAG where

import qualified RIO.ByteString.Lazy                             as Lazy
import           Servant
import           Servant.Client

import           Network.IPFS.Internal.Orphanage.ByteString.Lazy ()

import           Network.IPFS.CID.Types
import qualified Network.IPFS.File.Types                         as File
import           Network.IPFS.Stat.Types

import qualified Network.IPFS.Client.Add                         as Add
import qualified Network.IPFS.Client.Cat                         as Cat
import qualified Network.IPFS.Client.Pin                         as Pin
import qualified Network.IPFS.Client.Stat                        as Stat

import           Network.IPFS.Prelude                            hiding (object)

-- -- FIXME move
-- dagPut :: MonadRemoteIPFS m => File.Serialized -> m (Either ClientError Response)
-- dagPut file = do
--   boundary  <- liftIO genBoundary
--   runRemote (dagClient True (boundary, File.Form "file" file)) >>= \case
--     Left  err -> return $ Left err
--     Right val -> return $ Right val
