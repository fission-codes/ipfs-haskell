module Network.IPFS.Stat (getSize) where

import           Data.ByteString.Lazy.Char8 as CL
import           Data.List as List

import qualified RIO.ByteString.Lazy as Lazy

import           Network.IPFS.Prelude
import           Network.IPFS.Class
import qualified Network.IPFS.Internal.UTF8       as UTF8

import           Network.IPFS.Error          as IPFS.Error
import           Network.IPFS.Types          as IPFS

getSize ::
  MonadLocalIPFS m
  => IPFS.CID
  -> m (Either IPFS.Error.Add Integer)
getSize (CID hash) = ipfsRun ["object", "stat"] (Lazy.fromStrict <| encodeUtf8 hash) >>= \case
  (ExitSuccess, result, _) -> do
    case parseSize result of
      Nothing -> return . Left . UnexpectedOutput <| "Could not parse CumulativeSize"
      Just (size, _) -> return <| Right size

  (ExitFailure _ , _, err) -> return . Left . UnknownAddErr <| UTF8.textShow err

parseSize :: Lazy.ByteString -> Maybe (Integer, Lazy.ByteString)
parseSize = CL.readInteger . List.last . CL.words . List.last . CL.lines
