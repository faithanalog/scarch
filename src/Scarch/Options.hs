module Scarch.Options
  ( ScarchOptions(..)
  , parserScarchOptions
  , execScarchOptions
  ) where

import Options.Applicative
import Data.Monoid

data ScarchOptions = ScarchOptions
  { scarchOptFileNames :: [FilePath]
  , scarchOptUrls :: [String]
  } deriving (Eq, Read, Show)

data Input
  = InputFileName String
  | InputUrl String
  deriving (Eq, Read, Show)

fileName :: Parser Input
fileName =
  fmap
    InputFileName
    (option
       str
       (long "file" <> short 'f' <> metavar "FILE" <>
        help
          "Input file containing one URL per line. Multiple files may be specified by using -f more than once. A filename of '-' tells Scarch to read from STDIN."))

url :: Parser Input
url =
  fmap
    InputUrl
    (strArgument
       (metavar "URLS..." <>
        help
          "URLs to download. If a URL points to a playlist, all tracks in the playlist will be downloaded. A SoundCloud user's tracks and likes are playlists too! Use https://soundcloud.com/username/tracks or https://soundcloud.com/username/likes to download them."))

inputs :: Parser [Input]
inputs = some (fileName <|> url)

scarchOptions :: Parser ScarchOptions
scarchOptions =
  let fileNames xs = [x | InputFileName x <- xs]
      urls xs = [x | InputUrl x <- xs]
      opts xs =
        ScarchOptions
        {scarchOptFileNames = fileNames xs, scarchOptUrls = urls xs}
  in fmap opts inputs

optInfo :: InfoMod a
optInfo =
  fullDesc <> header "Scarch - A SoundCloud archiver" <>
  progDesc
    "Scarch is a tool designed to archive content from SoundCloud. It uses the youtube-dl command line utility to download track and playlist metadata, and retrieve track download links. Scarch will download tracks and playlists from URLs supplied on the command line, and from URLs listed in files specified with the -f flag. All downloaded files will be sorted by uploader. One folder is created per uploader. Within each uploader folder, one folder is created per track, containing the track file, metadata, and the thumbnail if it has one."

parserScarchOptions :: ParserInfo ScarchOptions
parserScarchOptions = info (scarchOptions <**> helper) optInfo

execScarchOptions :: IO ScarchOptions
execScarchOptions = customExecParser (prefs showHelpOnEmpty) parserScarchOptions
