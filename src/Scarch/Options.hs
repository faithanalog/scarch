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
  , scarchNumJobs :: Int
  , scarchOverwriteFIles :: Bool
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

numJobs :: Parser Int
numJobs =
  option
    auto
    (long "jobs" <> short 'j' <> metavar "NUM_JOBS" <> value 8 <> showDefault <>
     help
       "Specify the number of files to download simultaneously. Raising this number may increase speed, but it will also take more processing power and more bandwidth.")

overwrite :: Parser Bool
overwrite =
  flag
    False
    True
    (long "overwrite" <> short 'o' <>
     help
       "By default, Scarch will not overwrite files which already exist. Setting the overwrite flag will force Scarch to re-write metadata files and re-download tracks and thumbnails.")

inputs :: Parser [Input]
inputs = some (fileName <|> url)

scarchOptions :: Parser ScarchOptions
scarchOptions =
  let fileNames xs = [x | InputFileName x <- xs]
      urls xs = [x | InputUrl x <- xs]
      opts ow n xs =
        ScarchOptions
        { scarchOptFileNames = fileNames xs
        , scarchOptUrls = urls xs
        , scarchNumJobs = n
        , scarchOverwriteFIles = ow
        }
  in liftA3 opts overwrite numJobs inputs

optInfo :: InfoMod a
optInfo =
  fullDesc <> header "Scarch - A Parallel SoundCloud archiver" <>
  progDesc
    "Scarch is a wrapper around youtube-dl which can archive content from SoundCloud faster and more conveniently than using youtube-dl by itself. Scarch will download tracks and playlists from URLs supplied on the command line, and from URLs listed in files specified with the -f flag. Downloaded tracks are organized into folders by uploader. Each track is stored in its own folder, along with the track metadata, and track thumbnail if one exists."

parserScarchOptions :: ParserInfo ScarchOptions
parserScarchOptions = info (scarchOptions <**> helper) optInfo

execScarchOptions :: IO ScarchOptions
execScarchOptions = customExecParser (prefs showHelpOnEmpty) parserScarchOptions
