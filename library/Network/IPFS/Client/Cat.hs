module Network.IPFS.Client.Cat (API) where

import Servant

import qualified Network.IPFS.File.Types        as File
import qualified Network.IPFS.Client.Param as Param

type API = Param.CID
        :> Get '[PlainText] File.Serialized
