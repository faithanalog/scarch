module Main where

import Data.Aeson (decodeStrict', encode)
import Control.Applicative ((<|>))
import Scarch
import Scarch.Types
import Scarch.Options
import System.FilePath ((<.>), (</>), makeValid, takeExtension)

makeValidFileName :: FilePath -> FilePath
makeValidFileName =
  let f '/' = '_'
      f '\\' = '_'
      f x = x
  in makeValid . map f

saveTrack :: Track -> Scarch ()
saveTrack track = do
  let artistPath = makeValidFileName (trackUploader track)
      songPath = makeValidFileName (trackTitle track ++ "-" ++ trackId track)
      fullPath = artistPath </> songPath
      metaFile = fullPath </> "metadata.json"
      songFile = fullPath </> songPath <.> trackExt track
  writeFileLBS metaFile (encode (trackMetadata track))
  printInfo ("Wrote metadata: " ++ fullPath)
  case trackThumbnail track of
    Just thumbUrl ->
      let thumbExt = takeExtension thumbUrl
          thumbFile = fullPath </> "thumbnail" <.> thumbExt
      in downloadFile thumbFile thumbUrl
    Nothing -> return ()
  downloadFile songFile (trackUrl track)
  printInfo ("Downloaded track: " ++ fullPath)

saveUrlsConcurrently :: Traversable t => t String -> Scarch ()
saveUrlsConcurrently = concurrently_ . fmap saveUrl

savePlaylist :: Playlist -> Scarch ()
savePlaylist playlist = do
  printInfo ("Saving playlist: " ++ playlistTitle playlist)
  saveUrlsConcurrently (map playlistentryUrl (playlistEntries playlist))

saveUrl :: String -> Scarch ()
saveUrl url = do
  json <- getYtdlJson url
  let withJson f = fmap f (decodeStrict' json)
  case withJson saveTrack <|> withJson savePlaylist of
    Just runSaveAction -> do
      printInfo ("Loaded JSON: " ++ url)
      runSaveAction
    Nothing -> printError ("Decode error while loading JSON: " ++ url)

getUrlsFromFile :: FilePath -> IO [String]
getUrlsFromFile "-" = fmap lines getContents
getUrlsFromFile f = fmap lines (readFile f)

main :: IO ()
main = do
  opts <- execScarchOptions
  fileUrls <- fmap concat (mapM getUrlsFromFile (scarchOptFileNames opts))
  let urls = fileUrls ++ scarchOptUrls opts
  runScarchIO (saveUrlsConcurrently urls) (scarchNumJobs opts)
