{-# LANGUAGE OverloadedStrings #-}
-- | EnergyBand algorithm: https://essentia.upf.edu/documentation/reference/streaming_EnergyBand.html .
module AcousticBrainz.LowLevel.Algorithm.EnergyBand where

import           AcousticBrainz.StatisticalUnits

import           Data.Aeson
import           Data.Aeson.Types


data Output = Output
  { _high       :: StatisticalUnits
  , _low        :: StatisticalUnits
  , _middleHigh :: StatisticalUnits
  , _middleLow  :: StatisticalUnits

  } deriving(Eq, Ord, Read, Show)

instance FromJSON Output where
  parseJSON = withObject "EnergyBand" $ \v -> Output
    <$> v .: "spectral_energyband_high"
    <*> v .: "spectral_energyband_low"
    <*> v .: "spectral_energyband_middle_high"
    <*> v .: "spectral_energyband_middle_low"
