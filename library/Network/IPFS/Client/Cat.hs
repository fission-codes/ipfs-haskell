module Network.IPFS.Client.Cat (API) where

import Servant

import qualified Network.File.Types        as File
import qualified Network.IPFS.Client.Param as Param

type API = Param.CID
        :> Get '[PlainText] File.Serialized
