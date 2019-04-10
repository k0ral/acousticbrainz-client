{-# LANGUAGE OverloadedStrings #-}
module AcousticBrainz.LowLevel.EquivalentRectangularBandwidth where

import           AcousticBrainz.StatisticalUnits
import qualified AcousticBrainz.LowLevel.Bands      as Generic

import           Data.Aeson                         as JSON
import           Data.Aeson.Types                   as JSON

newtype Bands = Bands Generic.Bands deriving(Eq, Ord, Read, Show)

instance FromJSON Bands where
  parseJSON = withObject "Equivalent Rectangular Bandwidth Bands" $ \v -> fmap Bands (Generic.Bands
    <$> (_statisticalUnitsList <$> v .: "erbbands")
    <*> v .: "erbbands_crest"
    <*> v .: "erbbands_flatness_db"
    <*> v .: "erbbands_kurtosis"
    <*> v .: "erbbands_skewness"
    <*> v .: "erbbands_spread")
