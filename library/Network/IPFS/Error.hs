module Network.IPFS.Error
  ( Add (..)
  , Error (..)
  , Linearization (..)
  ) where

import Servant.Server

import           Network.IPFS.Prelude
import           Network.IPFS.Types
import           Network.IPFS.ToServerError

import qualified Network.IPFS.Get.Error as Get

data Error
  = AddErr Add
  | GetErr Get.Error
  | LinearizationErr Linearization
  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance ToServerError Error where
  toServerError = \case
    AddErr           addErr -> toServerError addErr
    GetErr           getErr -> toServerError getErr
    LinearizationErr linErr -> toServerError linErr

data Add
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

instance Display Add where
  display = \case
    InvalidFile          -> "Invalid file"
    UnexpectedOutput txt -> "Unexpected IPFS output: " <> display txt
    RecursiveAddErr  err -> "Error while adding directory" <> display err
    UnknownAddErr    txt -> "Unknown IPFS add error: " <> display txt

instance ToServerError Add where
  toServerError = \case
    InvalidFile        -> err422 { errBody = "File not processable by IPFS" }
    UnknownAddErr    _ -> err500 { errBody = "Unknown IPFS error" }
    RecursiveAddErr  _ -> err500 { errBody = "Error while adding directory" }
    UnexpectedOutput _ -> err500 { errBody = "Unexpected IPFS result" }

-- NOTE Will not stay as a newtype in the long term
newtype Linearization = NonLinear SparseTree
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( Exception
                    , ToJSON
                    )

instance Display Linearization where
  display (NonLinear sparseTree) = "Unable to linearize IPFS result: " <> display sparseTree

instance ToServerError Linearization where
  toServerError _ = err500 { errBody = "Unable to linearize IPFS result" }
