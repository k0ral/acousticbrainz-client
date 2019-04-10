{-# LANGUAGE OverloadedStrings #-}
-- | HighResolutionFeatures algorithm: https://essentia.upf.edu/documentation/reference/streaming_HighResolutionFeatures.html .
module AcousticBrainz.LowLevel.Algorithm.HighResolutionFeatures where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Scientific


data Output = Output
  { _tuningEqualTemperedDeviation :: Scientific
  , _tuningNonTemperedEnergyRatio :: Scientific
  } deriving(Eq, Ord, Read, Show)

instance FromJSON Output where
  parseJSON = withObject "HighResolutionFeatures" $ \v -> Output
    <$> v .: "tuning_equal_tempered_deviation"
    <*> v .: "tuning_nontempered_energy_ratio"
