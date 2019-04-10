{-# LANGUAGE OverloadedStrings     #-}
module AcousticBrainz.MetaData where

import           AcousticBrainz.Version

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Scientific


data MetaData = MetaData
  { _audioProperties :: AudioProperties
  , _tags            :: Tags
  , _version         :: Version
  } deriving(Eq, Read, Show)



data AudioProperties = AudioProperties
  { _analysisSampleRate :: Natural
  , _bitRate            :: Natural
  , _codec              :: Text
  , _downmix            :: Text
  , _equalLoudness      :: Natural
  , _length             :: Scientific
  , _lossless           :: Bool
  , _md5Encoded         :: Text
  , _replayGain         :: Scientific
  , _sampleRate         :: Natural
  } deriving(Eq, Ord, Read, Show)

instance FromJSON AudioProperties where
  parseJSON = withObject "audio_properties" $ \v -> AudioProperties
    <$> v .: "analysis_sample_rate"
    <*> v .: "bit_rate"
    <*> v .: "codec"
    <*> v .: "downmix"
    <*> v .: "equal_loudness"
    <*> v .: "length"
    <*> ((int2bool <$> v .: "lossless") <|> v .: "lossless")
    <*> v .: "md5_encoded"
    <*> v .: "replay_gain"
    <*> v .: "sample_rate"
    where int2bool :: Int -> Bool
          int2bool 0 = False
          int2bool _ = True


newtype Tags = Tags (HashMap Text Value) deriving(Eq, Read, Show)

instance FromJSON Tags where
  parseJSON = withObject "tags" $ pure . Tags
