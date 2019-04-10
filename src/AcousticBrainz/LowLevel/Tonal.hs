{-# LANGUAGE OverloadedStrings #-}
-- | Tonal descriptors.
module AcousticBrainz.LowLevel.Tonal where

import           AcousticBrainz.StatisticalUnits
import qualified AcousticBrainz.LowLevel.Algorithm.Chords                 as Chords
import qualified AcousticBrainz.LowLevel.Algorithm.HighResolutionFeatures as HighResolutionFeatures
import qualified AcousticBrainz.LowLevel.Algorithm.HPCP                   as HPCP
import qualified AcousticBrainz.LowLevel.Algorithm.Key                    as Key

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Scientific

data Tonal = Tonal
  { _chords                            :: Chords.Output
  , _highResolutionFeatures            :: HighResolutionFeatures.Output
  , _hpcp                              :: HPCP.Output
  , _harmonicPitchClassProfilesEntropy :: StatisticalUnits
  , _key                               :: Key.Output
  , _tuningFrequency                   :: Scientific
  } deriving(Eq, Ord, Read, Show)

instance FromJSON Tonal where
  parseJSON value = flip (withObject "tonal") value $ \v -> Tonal
    <$> parseJSON value
    <*> parseJSON value
    <*> parseJSON value
    <*> v .: "hpcp_entropy"
    <*> parseJSON value
    <*> v .: "tuning_frequency"

