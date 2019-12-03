module Network.IPFS.Stat (getSize) where

import           Data.ByteString.Lazy.Char8 as CL
import           Data.List as List

import qualified RIO.ByteString.Lazy as Lazy

import           Network.IPFS.Prelude
import           Network.IPFS.Local.Class
import qualified Network.IPFS.Internal.UTF8 as UTF8

import           Network.IPFS.Get.Error as IPFS.Get
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Types     as IPFS

getSize ::
  MonadLocalIPFS m
  => IPFS.CID
  -> m (Either IPFS.Get.Error Integer)
getSize cid@(CID hash) = ipfsRun ["object", "stat"] (Lazy.fromStrict <| encodeUtf8 hash) >>= \case
  Left err -> case err of
    Process.Timeout secs -> return . Left <| TimedOut cid secs
    Process.UnknownErr raw -> return . Left . UnknownErr <| UTF8.textShow raw
  Right contents -> do
    case parseSize contents of
      Nothing -> return . Left . UnexpectedOutput <| "Could not parse CumulativeSize"
      Just (size, _) -> return <| Right size

parseSize :: Lazy.ByteString -> Maybe (Integer, Lazy.ByteString)
parseSize = CL.readInteger . List.last . CL.words . List.last . CL.lines
