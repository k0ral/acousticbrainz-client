{-# LANGUAGE OverloadedStrings #-}
-- | BpmHistogramDescriptors algorithm: https://essentia.upf.edu/documentation/reference/streaming_BpmHistogramDescriptors.html .
module AcousticBrainz.LowLevel.Algorithm.BpmHistogramDescriptors where

import           AcousticBrainz.StatisticalUnits

import           Data.Aeson
import           Data.Aeson.Types


data Output = Output
  { _firstPeakBpm     :: StatisticalUnits
  , _firstPeakSpread  :: StatisticalUnits
  , _firstPeakWeight  :: StatisticalUnits
  , _secondPeakBpm    :: StatisticalUnits
  , _secondPeakSpread :: StatisticalUnits
  , _secondPeakWeight :: StatisticalUnits
  } deriving(Eq, Ord, Read, Show)

instance FromJSON Output where
  parseJSON = withObject "BpmHistogramDescriptors" $ \v -> Output
    <$> v .: "bpm_histogram_first_peak_bpm"
    <*> v .: "bpm_histogram_first_peak_spread"
    <*> v .: "bpm_histogram_first_peak_weight"
    <*> v .: "bpm_histogram_second_peak_bpm"
    <*> v .: "bpm_histogram_second_peak_spread"
    <*> v .: "bpm_histogram_second_peak_weight"
