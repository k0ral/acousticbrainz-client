{-# LANGUAGE OverloadedStrings #-}
module AcousticBrainz.LowLevel.Spectral where

import           AcousticBrainz.StatisticalUnits
import qualified AcousticBrainz.LowLevel.Algorithm.EnergyBand as EnergyBand

import           Data.Aeson
import           Data.Aeson.Types

data Spectral = Spectral
  { _centroid             :: StatisticalUnits
  , _complexity           :: StatisticalUnits
  , _contrastCoefficients :: [StatisticalUnits]
  , _contrastValleys      :: [StatisticalUnits]
  , _decrease             :: StatisticalUnits
  , _energy               :: StatisticalUnits
  , _energyBand           :: EnergyBand.Output
  , _entropy              :: StatisticalUnits
  , _flux                 :: StatisticalUnits
  , _kurtosis             :: StatisticalUnits
  , _rms                  :: StatisticalUnits
  , _rolloff              :: StatisticalUnits
  , _skewness             :: StatisticalUnits
  , _spread               :: StatisticalUnits
  , _strongPeak           :: StatisticalUnits
  } deriving(Eq, Ord, Read, Show)

instance FromJSON Spectral where
  parseJSON value = flip (withObject "spectral") value $ \v -> Spectral
    <$> v .: "spectral_centroid"
    <*> v .: "spectral_complexity"
    <*> (_statisticalUnitsList <$> v .: "spectral_contrast_coeffs")
    <*> (_statisticalUnitsList <$> v .: "spectral_contrast_valleys")
    <*> v .: "spectral_decrease"
    <*> v .: "spectral_energy"
    <*> parseJSON value
    <*> v .: "spectral_entropy"
    <*> v .: "spectral_flux"
    <*> v .: "spectral_kurtosis"
    <*> v .: "spectral_rms"
    <*> v .: "spectral_rolloff"
    <*> v .: "spectral_skewness"
    <*> v .: "spectral_spread"
    <*> v .: "spectral_strongpeak"
