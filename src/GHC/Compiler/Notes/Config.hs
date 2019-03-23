{-# LANGUAGE OverloadedStrings #-}

module GHC.Compiler.Notes.Config where

import qualified Data.ByteString      as ByteString
import           Data.Yaml
import qualified System.FilePath.Glob as Glob


data NoteConfig = NoteConfig
  { confResource :: NoteConfigResource
  , confOutDir :: String
  , confTargets :: [Glob.Pattern]
  }
  deriving (Eq, Show)

instance FromJSON NoteConfig where
  parseJSON = withObject "NoteConfig" $ \v -> NoteConfig
    <$> v .: "resource"
    <*> v .: "outDir"
    <*> (fmap Glob.compile <$> v .: "targets")


data NoteConfigResource = NoteConfigResource
  deriving (Eq, Show)

instance FromJSON NoteConfigResource where
  parseJSON = withObject "NoteConfigResource" $ \_ -> pure NoteConfigResource


parseConfigFromString :: ByteString.ByteString -> Either ParseException NoteConfig
parseConfigFromString = decodeEither'

parseConfigFromFile :: FilePath -> IO (Either ParseException NoteConfig)
parseConfigFromFile = decodeFileEither
