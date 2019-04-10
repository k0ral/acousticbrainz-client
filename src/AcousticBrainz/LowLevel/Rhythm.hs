{-# LANGUAGE OverloadedStrings #-}
-- | Rhythm descriptors.
module AcousticBrainz.LowLevel.Rhythm where

import           AcousticBrainz.StatisticalUnits
import qualified AcousticBrainz.LowLevel.Algorithm.BpmHistogramDescriptors as BpmHistogramDescriptors

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Scientific

data Rhythm = Rhythm
  { _beatsCount              :: Int
  , _beatsLoudness           :: StatisticalUnits
  , _beatsLoudnessBandRatio  :: [StatisticalUnits]
  , _beatsPosition           :: [Scientific]
  , _beatsPerMinute          :: Scientific
  , _beatsPerMinuteHistogram :: BpmHistogramDescriptors.Output
  , _danceability            :: Scientific
  , _onsetRate               :: Scientific
  } deriving(Eq, Read, Show)

instance FromJSON Rhythm where
  parseJSON value = flip (withObject "rhythm") value $ \v -> Rhythm
    <$> v .: "beats_count"
    <*> v .: "beats_loudness"
    <*> (_statisticalUnitsList <$> v .: "beats_loudness_band_ratio")
    <*> v .: "beats_position"
    <*> v .: "bpm"
    <*> parseJSON value
    <*> v .: "danceability"
    <*> v .: "onset_rate"
