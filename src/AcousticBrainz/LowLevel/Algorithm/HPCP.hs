{-# LANGUAGE OverloadedStrings #-}
-- | HPCP algorithm: https://essentia.upf.edu/documentation/reference/streaming_HPCP.html .
module AcousticBrainz.LowLevel.Algorithm.HPCP where

import           AcousticBrainz.StatisticalUnits

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Scientific


data Output = Output
  { _harmonicPitchClassProfile           :: [StatisticalUnits]
  , _transposedHarmonicPitchClassProfile :: [Scientific]
  } deriving(Eq, Ord, Read, Show)

instance FromJSON Output where
  parseJSON value = flip (withObject "HPCP") value $ \v -> Output
    <$> (_statisticalUnitsList <$> v .: "hpcp")
    <*> v .: "thpcp"
