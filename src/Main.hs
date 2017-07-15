{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
module Main where

import Conduit (runResourceT, sinkFile)
import Control.Applicative
import Control.Concurrent.Async (mapConcurrently, race_)
import Control.Concurrent.QSem
       (QSem, newQSem, signalQSem, waitQSem)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
       (TQueue, newTQueue, readTQueue, writeTQueue)
import Control.Exception (bracket_)
import Control.Monad
import Data.Aeson
       (FromJSON(..), ToJSON(..), decodeStrict', encode, genericParseJSON,
        genericToEncoding, genericToJSON)
import Data.Aeson.Types (Options(..), camelTo2, defaultOptions)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.Char (isLower, toLower)
import Data.Monoid
import GHC.Generics (Generic)
import Network.HTTP.Simple (httpSink, parseRequest)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
       ((<.>), (</>), makeValid, takeDirectory, takeExtension)
import System.IO (hPutStrLn, stderr)
import System.Process
       (CreateProcess(std_out), StdStream(CreatePipe), createProcess,
        proc, waitForProcess)
import Control.Monad.Free.Church (F, iterM, liftF)

-- You may be wondering why these all use String rather than Text.
--
-- The answer is, the libraries I use all expect String, so I don't have a
-- use for the Text form of anything anyway.

data TrackMetadata = TrackMetadata
  { trackmetaId :: String
  , trackmetaUploadDate :: String
  , trackmetaTitle :: String
  , trackmetaDescription :: Maybe String
  , trackmetaLicense :: String
  } deriving (Eq, Read, Show, Generic)

data Track = Track
  { trackId :: String
  , trackUploader :: String
  , trackUploadDate :: String
  , trackTitle :: String
  , trackDescription :: Maybe String
  , trackLicense :: String
  , trackWebpageUrl :: String
  , trackThumbnail :: Maybe String
  , trackUrl :: String
  , trackExt :: String
  } deriving (Eq, Read, Show, Generic)

trackMetadata :: Track -> TrackMetadata
trackMetadata t =
  TrackMetadata
  { trackmetaId = trackId t
  , trackmetaUploadDate = trackUploadDate t
  , trackmetaTitle = trackTitle t
  , trackmetaDescription = trackDescription t
  , trackmetaLicense = trackLicense t
  }

data Playlist = Playlist
  { playlistId :: String
  , playlistTitle :: String
  , playlistEntries :: [PlaylistEntry]
  } deriving (Eq, Read, Show, Generic)

newtype PlaylistEntry = PlaylistEntry
  { playlistentryUrl :: String
  } deriving (Eq, Read, Show, Generic)

nameTransformer :: String -> String
nameTransformer = camelTo2 '_' . (\(x:xs) -> toLower x : xs) . dropWhile isLower

jsonOpts :: Options
jsonOpts = defaultOptions {fieldLabelModifier = nameTransformer}

instance ToJSON TrackMetadata where
  toJSON = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

instance FromJSON TrackMetadata where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON Track where
  toJSON = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

instance FromJSON Track where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON Playlist where
  toJSON = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

instance FromJSON Playlist where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON PlaylistEntry where
  toJSON = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

instance FromJSON PlaylistEntry where
  parseJSON = genericParseJSON jsonOpts


data ScarchF a where
  PrintInfo :: String -> a -> ScarchF a
  PrintError :: String -> a -> ScarchF a
  DownloadFile :: FilePath -> String -> a -> ScarchF a
  WriteFile :: FilePath -> Lazy.ByteString -> a -> ScarchF a
  GetMetadata :: String -> (Strict.ByteString -> a) -> ScarchF a
  Concurrently :: forall a t b. Traversable t => t (Scarch b) -> (t b -> a) -> ScarchF a

deriving instance Functor ScarchF

type Scarch = F ScarchF

runScarchIO :: Scarch a -> Env -> IO a
runScarchIO scarch env =
  let o = outQueue env
      e = errQueue env
      sem = reqSem env
      withReqSem = bracket_ (waitQSem sem) (signalQSem sem)
      phi (PrintInfo info m) = atomically (writeTQueue o info) *> m
      phi (PrintError err m) = atomically (writeTQueue e err) *> m
      phi (DownloadFile path url m) = do
        createDirectoryIfMissing True (takeDirectory path)
        withReqSem $ do
          req <- parseRequest url
          runResourceT (httpSink req (const (sinkFile path)))
        m
      phi (WriteFile path contents m) = do
        createDirectoryIfMissing True (takeDirectory path)
        Lazy.writeFile path contents
        m
      phi (GetMetadata url m) = do
        metadata <-
          withReqSem $ do
            (_, Just pout, _, phandle) <-
              createProcess
                (proc "youtube-dl" ["--flat-playlist", "-J", "-f", "best", url])
                {std_out = CreatePipe}
            json <- Strict.hGetContents pout
            void (waitForProcess phandle)
            return json
        m metadata
      phi (Concurrently actions m) = mapConcurrently run actions >>= m
      run :: forall a. Scarch a -> IO a
      run = iterM phi
  in run scarch

data Env = Env
  { reqSem :: QSem -- ^ Semaphore for HTTP Requests
  , outQueue :: TQueue String -- ^ STDOUT
  , errQueue :: TQueue String -- ^ STDERR
  }

printInfo :: String -> Scarch ()
printInfo info = liftF (PrintInfo info ())

printError :: String -> Scarch ()
printError err = liftF (PrintError err ())

downloadFile :: FilePath -> String -> Scarch ()
downloadFile path url = liftF (DownloadFile path url ())

writeFileLBS :: FilePath -> Lazy.ByteString -> Scarch ()
writeFileLBS path contents = liftF (WriteFile path contents ())

getYtdlJson :: String -> Scarch Strict.ByteString
getYtdlJson url = liftF (GetMetadata url id)

concurrently :: Traversable t => t (Scarch a) -> Scarch (t a)
concurrently actions = liftF (Concurrently actions id)

makeValidFileName :: FilePath -> FilePath
makeValidFileName =
  let f '/' = '-'
      f x = x
  in makeValid . map f

saveTrack :: Track -> Scarch ()
saveTrack track = do
  let artistPath = makeValidFileName (trackUploader track)
      songPath = makeValidFileName (trackTitle track <> "-" <> trackId track)
      fullPath = artistPath </> songPath
      metaFile = fullPath </> "metadata.json"
      songFile = fullPath </> songPath <.> trackExt track
  writeFileLBS metaFile (encode (trackMetadata track))
  printInfo ("Wrote metadata: " <> fullPath)
  forM_
    (trackThumbnail track)
    (\thumbUrl ->
       let thumbExt = takeExtension thumbUrl
           thumbFile = fullPath </> "thumbnail" <.> thumbExt
       in downloadFile thumbFile thumbUrl)
  downloadFile songFile (trackUrl track)
  printInfo ("Downloaded track: " <> fullPath)

savePlaylist :: Playlist -> Scarch ()
savePlaylist playlist = do
  printInfo ("Saving playlist: " <> playlistTitle playlist)
  void
    (concurrently
       (map (saveTracks . playlistentryUrl) (playlistEntries playlist)))

saveTracks :: String -> Scarch ()
saveTracks url = do
  json <- getYtdlJson url
  maybe
    (printError ("Decode error while loading JSON from URL: " <> url))
    (\x -> printInfo ("Loaded JSON: " <> url) *> x)
    (fmap saveTrack (decodeStrict' json) <|>
     fmap savePlaylist (decodeStrict' json))

main :: IO ()
main = do
  urls <- fmap lines getContents
  sem <- newQSem 8
  oQueue <- atomically newTQueue
  eQueue <- atomically newTQueue
  let env = Env {reqSem = sem, outQueue = oQueue, errQueue = eQueue}
  race_
    (race_
       (forever (atomically (readTQueue oQueue) >>= putStrLn))
       (forever (atomically (readTQueue eQueue) >>= hPutStrLn stderr)))
    (runScarchIO ((void . concurrently . map saveTracks) urls) env)
  

-- TODO command line options
