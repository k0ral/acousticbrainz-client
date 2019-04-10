{-# LANGUAGE OverloadedStrings #-}
module AcousticBrainz.LowLevel.MelBands where

import           AcousticBrainz.StatisticalUnits
import           AcousticBrainz.LowLevel.Bands

import           Data.Aeson
import           Data.Aeson.Types

newtype MelBands = MelBands Bands deriving(Eq, Ord, Read, Show)

instance FromJSON MelBands where
  parseJSON = withObject "MelBands" $ \v -> fmap MelBands (Bands
    <$> (_statisticalUnitsList <$> v .: "melbands")
    <*> v .: "melbands_crest"
    <*> v .: "melbands_flatness_db"
    <*> v .: "melbands_kurtosis"
    <*> v .: "melbands_skewness"
    <*> v .: "melbands_spread")
