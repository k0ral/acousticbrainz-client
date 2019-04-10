{-# LANGUAGE OverloadedStrings #-}
module AcousticBrainz.Version where

import           Data.Aeson
import           Data.Aeson.Types


data Version = Version
  { _essentia             :: EssentiaVersion
  , _extractor            :: Text
  , _gaia                 :: Maybe GaiaVersion
  , _modelsEssentiaGitSha :: Text
  } deriving(Eq, Ord, Read, Show)

instance FromJSON Version where
  parseJSON value = flip (withObject "version") value $ \v -> Version
    <$> parseJSON value
    <*> v .: "extractor"
    <*> ((Just <$> parseJSON value) <|> pure Nothing)
    <*> (v .: "models_essentia_git_sha" <|> pure mempty)

-- | Cf https://github.com/MTG/essentia/releases .
data EssentiaVersion = EssentiaVersion
  { _essentiaVersion  :: Text
  , _essentiaBuildSha :: Text
  , _essentiaGitSha   :: Text
  } deriving(Eq, Ord, Read, Show)

instance FromJSON EssentiaVersion where
  parseJSON = withObject "EssentiaVersion" $ \v -> EssentiaVersion
    <$> v .: "essentia"
    <*> v .: "essentia_build_sha"
    <*> v .: "essentia_git_sha"

-- | Cf https://github.com/MTG/gaia/releases .
data GaiaVersion = GaiaVersion
  { _gaiaVersion :: Text
  , _gaiaGitSha  :: Text
  } deriving(Eq, Ord, Read, Show)

instance FromJSON GaiaVersion where
  parseJSON = withObject "GaiaVersion" $ \v -> GaiaVersion
    <$> v .: "gaia"
    <*> v .: "gaia_git_sha"
