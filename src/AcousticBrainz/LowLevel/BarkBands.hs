{-# LANGUAGE OverloadedStrings #-}
module AcousticBrainz.LowLevel.BarkBands where

import           AcousticBrainz.StatisticalUnits
import           AcousticBrainz.LowLevel.Bands

import           Data.Aeson                         as JSON
import           Data.Aeson.Types                   as JSON

newtype BarkBands = BarkBands Bands deriving(Eq, Ord, Read, Show)

instance FromJSON BarkBands where
  parseJSON = withObject "BarkBands" $ \v -> fmap BarkBands (Bands
    <$> (_statisticalUnitsList <$> v .: "barkbands")
    <*> v .: "barkbands_crest"
    <*> v .: "barkbands_flatness_db"
    <*> v .: "barkbands_kurtosis"
    <*> v .: "barkbands_skewness"
    <*> v .: "barkbands_spread")
