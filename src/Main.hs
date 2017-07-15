{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad.Reader (ReaderT(..), ask, asks)
import Control.Monad.Trans.Class (lift)
import Data.Aeson
       (FromJSON(..), ToJSON(..), decodeStrict', encode, genericParseJSON,
        genericToEncoding, genericToJSON)
import Data.Aeson.Types (Options(..), camelTo2, defaultOptions)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.Char (isLower, toLower)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Network.HTTP.Simple (httpSink, parseRequest)
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.FilePath ((<.>), (</>), makeValid, takeExtension)
import System.IO (hPutStrLn, stderr)
import System.Process
       (CreateProcess(..), StdStream(..), createProcess, proc,
        waitForProcess)

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
  , playlistEntries :: Vector PlaylistEntry
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


data Env = Env
  { reqSem :: QSem -- ^ Semaphore for HTTP Requests
  , outQueue :: TQueue String -- ^ STDOUT
  , errQueue :: TQueue String -- ^ STDERR
  }

printInfo :: String -> ReaderT Env IO ()
printInfo info = do
  q <- asks outQueue
  lift (atomically (writeTQueue q info))

printError :: String -> ReaderT Env IO ()
printError err = do
  q <- asks errQueue
  lift (atomically (writeTQueue q err))

withReqSem :: ReaderT Env IO a -> ReaderT Env IO a
withReqSem action = do
  env <- ask
  let sem = reqSem env
  lift (bracket_ (waitQSem sem) (signalQSem sem) (runReaderT action env))

downloadFile :: String -> FilePath -> IO ()
downloadFile url file = do
  req <- parseRequest url
  runResourceT (httpSink req (const (sinkFile file)))

makeValidFileName :: FilePath -> FilePath
makeValidFileName =
  let f '/' = '-'
      f x = x
  in makeValid . map f

getYtdlJson :: String -> ReaderT Env IO Strict.ByteString
getYtdlJson url =
  withReqSem $ do
    printInfo ("Loading JSON: " <> url)
    lift $ do
      (_, Just pout, _, phandle) <-
        createProcess
          (proc "youtube-dl" ["--flat-playlist", "-J", "-f", "best", url])
          {std_out = CreatePipe}
      json <- Strict.hGetContents pout
      void (waitForProcess phandle)
      return json

saveTrack :: Track -> ReaderT Env IO ()
saveTrack track = do
  let artistPath = makeValidFileName (trackUploader track)
      songPath = makeValidFileName (trackTitle track <> "-" <> trackId track)
      fullPath = artistPath </> songPath
      metaFile = fullPath </> "metadata.json"
      songFile = fullPath </> songPath <.> trackExt track
  printInfo ("Writing metadata: " <> fullPath)
  lift $ do
    createDirectoryIfMissing True fullPath
    Lazy.writeFile metaFile (encode (trackMetadata track))
  withReqSem $ do
    printInfo ("Downloading track: " <> fullPath)
    lift $ do
      forM_
        (trackThumbnail track)
        (\thumbUrl ->
           let thumbExt = takeExtension thumbUrl
               thumbFile = fullPath </> "thumbnail" <.> thumbExt
           in downloadFile thumbUrl thumbFile)
      downloadFile (trackUrl track) songFile

savePlaylist :: Playlist -> ReaderT Env IO ()
savePlaylist playlist = do
  env <- ask
  printInfo ("Saving playlist: " <> playlistTitle playlist)
  lift
    (void
       (mapConcurrently
          (flip runReaderT env . saveTracks . playlistentryUrl)
          (playlistEntries playlist)))

saveTracks :: String -> ReaderT Env IO ()
saveTracks url = do
  json <- getYtdlJson url
  fromMaybe
    (printError ("Decode error while loading JSON from URL: " <> url))
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
    (void (mapConcurrently (flip runReaderT env . saveTracks) urls))
  

-- TODO command line options
