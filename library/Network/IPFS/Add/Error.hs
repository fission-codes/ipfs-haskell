module Network.IPFS.Add.Error (Error (..)) where

import Servant.Server

import           Network.IPFS.Prelude
import           Network.IPFS.ToServerError

import qualified Network.IPFS.Get.Error as Get

data Error
  = InvalidFile
  | UnexpectedOutput Text
  | RecursiveAddErr Get.Error
  | UnknownAddErr Text
  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance Display Error where
  display = \case
    InvalidFile          -> "Invalid file"
    UnexpectedOutput txt -> "Unexpected IPFS output: " <> display txt
    RecursiveAddErr  err -> "Error while adding directory" <> display err
    UnknownAddErr    txt -> "Unknown IPFS add error: " <> display txt

instance ToServerError Error where
  toServerError = \case
    InvalidFile        -> err422 { errBody = "File not processable by IPFS" }
    UnknownAddErr    _ -> err500 { errBody = "Unknown IPFS error" }
    RecursiveAddErr  _ -> err500 { errBody = "Error while adding directory" }
    UnexpectedOutput _ -> err500 { errBody = "Unexpected IPFS result" }
