{-# LANGUAGE OverloadedStrings #-}
-- | Chords algorithms:
-- - https://essentia.upf.edu/documentation/reference/streaming_ChordsDetection.html
-- - https://essentia.upf.edu/documentation/reference/streaming_ChordsDescriptors.html
module AcousticBrainz.LowLevel.Algorithm.Chords where

import           AcousticBrainz.StatisticalUnits
import qualified AcousticBrainz.LowLevel.Algorithm.Key as Key

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Scientific


data Output = Output
  { _changesRate :: Scientific
  , _histogram   :: [Scientific]
  , _key         :: Text
  , _numberRate  :: Scientific
  , _scale       :: Key.Scale
  , _strength    :: StatisticalUnits
  } deriving(Eq, Ord, Read, Show)

instance FromJSON Output where
  parseJSON = withObject "chords" $ \v -> Output
    <$> v .: "chords_changes_rate"
    <*> v .: "chords_histogram"
    <*> v .: "chords_key"
    <*> v .: "chords_number_rate"
    <*> v .: "chords_scale"
    <*> v .: "chords_strength"
