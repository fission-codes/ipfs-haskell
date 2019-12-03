module Network.IPFS.Add
  ( addRaw
  , addFile
  , addPath
  , addDir
  ) where

import Network.IPFS.Prelude hiding (link)
import           Network.IPFS.Local.Class

import           Data.ByteString.Lazy.Char8 as CL
import           Data.List                  as List

import qualified System.FilePath.Glob as Glob

import           RIO.Directory
import           RIO.FilePath
import qualified RIO.ByteString.Lazy as Lazy

import qualified Network.IPFS.Internal.UTF8 as UTF8

import           Network.IPFS.Error          as IPFS.Error
import           Network.IPFS.Types          as IPFS
import           Network.IPFS.DAG.Node.Types as DAG
import           Network.IPFS.DAG.Link       as DAG.Link

import           Network.IPFS.DAG as DAG

addRaw ::
  MonadLocalIPFS m
  => Lazy.ByteString
  -> m (Either IPFS.Error.Add IPFS.CID)
addRaw raw =
  ipfsRun ["add", "-HQ"] raw >>= \case
    Right result ->
      case CL.lines result of
        [cid] ->
          cid
            |> UTF8.textShow
            |> UTF8.stripN 1
            |> mkCID
            |> Right
            |> return

        bad ->
          return . Left . UnexpectedOutput <| UTF8.textShow bad

    Left err ->
      return . Left . UnknownAddErr <| UTF8.textShow err

addFile ::
  MonadLocalIPFS m
  => Lazy.ByteString
  -> IPFS.Name
  -> m (Either IPFS.Error.Add (IPFS.SparseTree, IPFS.CID))
addFile raw name =
  ipfsRun opts raw >>= \case
    Right result ->
      case CL.lines result of
        [inner, outer] ->
          let
            sparseTree  = Directory [(Hash rootCID, fileWrapper)]
            fileWrapper = Directory [(fileName, Content fileCID)]
            rootCID     = CID <| UTF8.textShow outer
            fileCID     = CID . UTF8.stripN 1 <| UTF8.textShow inner
            fileName    = Key name
          in
            return <| Right (sparseTree, rootCID)

        bad ->
          return . Left . UnexpectedOutput <| UTF8.textShow bad


    Left err ->
      return . Left . UnknownAddErr <| UTF8.textShow err

    where
      opts = [ "add"
             , "-wq"
             , "--stdin-name"
             , unName name
             ]

addPath ::
  MonadLocalIPFS m
  => FilePath
  -> m (Either IPFS.Error.Add CID)
addPath path = ipfsRun ["add", "-HQ", path] "" >>= pure . \case
  Right result ->
    case CL.lines result of
      [cid] -> Right . mkCID . UTF8.stripN 1 <| UTF8.textShow cid
      bad   -> Left . UnexpectedOutput <| UTF8.textShow bad

  Left err ->
    Left . UnknownAddErr <| UTF8.textShow err

addDir ::
  ( MonadIO m
  , MonadLocalIPFS m
  )
  => IPFS.Ignored
  -> FilePath
  -> m (Either IPFS.Error.Add IPFS.CID)
addDir ignored path = doesFileExist path >>= \case
  True -> addPath path
  False -> walkDir ignored path

walkDir ::
  ( MonadIO m
  , MonadLocalIPFS m
  )
  => IPFS.Ignored
  -> FilePath
  -> m (Either IPFS.Error.Add IPFS.CID)
walkDir ignored path = do
  files <- listDirectory path

  let
    toAdd = removeIgnored ignored files
    reducer = foldResults path ignored
    seed = Right <| Node
      { dataBlock = "CAE="
      , links = []
      }

  foldM reducer seed toAdd >>= \case
    Left err -> return <| Left err
    Right node -> DAG.putNode node

foldResults ::
  ( MonadIO m
  , MonadLocalIPFS m
  )
  => FilePath
  -> IPFS.Ignored
  -> Either IPFS.Error.Add Node
  -> FilePath
  -> m (Either IPFS.Error.Add Node)
foldResults _ _ (Left err) _ = return <| Left err
foldResults path ignored (Right node) filename = do
  addDir ignored (path </> filename) >>= \case
    Left err ->  return <| Left err
    Right cid ->
      DAG.Link.create cid (IPFS.Name filename) >>= \case
      Left err -> return . Left <| RecursiveAddErr err
      Right link ->
        return <| Right <| node { links = link:(links node) }

removeIgnored :: IPFS.Ignored -> [FilePath] -> [FilePath]
removeIgnored ignored files = List.filter (not . matchesAny ignored) files

matchesAny :: IPFS.Ignored -> FilePath -> Bool
matchesAny globs path = List.any (\x -> Glob.match x path) globs
