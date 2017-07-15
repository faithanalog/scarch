module Main where

import Data.Aeson (decodeStrict', encode)
import Control.Applicative
import Control.Monad
import Data.Monoid
import Scarch
import Scarch.Types
import System.FilePath ((<.>), (</>), makeValid, takeExtension)

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

saveUrlsConcurrently :: Traversable t => t String -> Scarch ()
saveUrlsConcurrently = void . concurrently . fmap saveUrl

savePlaylist :: Playlist -> Scarch ()
savePlaylist playlist = do
  printInfo ("Saving playlist: " <> playlistTitle playlist)
  saveUrlsConcurrently (map playlistentryUrl (playlistEntries playlist))

saveUrl :: String -> Scarch ()
saveUrl url = do
  json <- getYtdlJson url
  maybe
    (printError ("Decode error while loading JSON from URL: " <> url))
    (\m -> printInfo ("Loaded JSON: " <> url) *> m)
    (fmap saveTrack (decodeStrict' json) <|>
     fmap savePlaylist (decodeStrict' json))

main :: IO ()
main = do
  urls <- fmap lines getContents
  runScarchIO (saveUrlsConcurrently urls) 8

-- TODO command line options
