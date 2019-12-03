module Network.IPFS.Get
  ( getFile
  , getFileOrDirectory
  ) where

import           Network.IPFS.Prelude
import           Network.IPFS.Local.Class
import qualified Network.IPFS.Internal.UTF8       as UTF8

import           Data.ByteString.Lazy.Char8 as CL
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text as Text

import qualified Network.IPFS.File.Types     as File
import           Network.IPFS.Get.Error      as IPFS.Get
import           Network.IPFS.Types          as IPFS

getFileOrDirectory ::
  MonadLocalIPFS m
  => IPFS.CID
  -> m (Either IPFS.Get.Error CL.ByteString)
getFileOrDirectory cid@(IPFS.CID hash) = ipfsRun ["get", Text.unpack hash] "" >>= \case
  Right contents -> return <| Right contents
  Left err -> case err of
    ErrTimeout secs -> return . Left <| TimedOut cid secs
    ErrUnknown raw -> return . Left . UnknownErr <| UTF8.textShow raw

getFile ::
  MonadLocalIPFS m
  => IPFS.CID
  -> m (Either IPFS.Get.Error File.Serialized)
getFile cid@(IPFS.CID hash) = ipfsRun ["cat"] (UTF8.textToLazyBS hash) >>= \case
  Right contents -> return . Right <| File.Serialized contents
  Left err -> case err of
    ErrTimeout secs -> return . Left <| TimedOut cid secs
    ErrUnknown raw -> 
      if Lazy.isPrefixOf "Error: invalid 'ipfs ref' path" raw
        then return . Left <| InvalidCID hash
        else return . Left . UnknownErr <| UTF8.textShow raw
