{-# LANGUAGE DeriveGeneric #-}
module Scarch.Types
  ( TrackMetadata(..)
  , Track(..)
  , trackMetadata
  , Playlist(..)
  , PlaylistEntry(..)
  ) where

import Data.Aeson
       (FromJSON(..), ToJSON(..), genericParseJSON, genericToEncoding,
        genericToJSON)
import Data.Aeson.Types (Options(..), camelTo2, defaultOptions)
import Data.Char (isLower, toLower)
import GHC.Generics (Generic)

-- You may be wondering why these all use String rather than Text.
--
-- The answer is, the libraries I use all expect String, so I don't have a
-- use for the Text form of anything anyway.

data TrackMetadata = TrackMetadata
  { trackmetaId :: String
  , trackmetaUploader :: String
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
  , trackmetaUploader = trackUploader t
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
