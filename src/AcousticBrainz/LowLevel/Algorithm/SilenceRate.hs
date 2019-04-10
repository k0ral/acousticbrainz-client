{-# LANGUAGE OverloadedStrings #-}
-- | SilenceRate algorithm: https://essentia.upf.edu/documentation/reference/streaming_SilenceRate.html .
module AcousticBrainz.LowLevel.Algorithm.SilenceRate where

import           AcousticBrainz.StatisticalUnits

import           Data.Aeson
import           Data.Aeson.Types


data Output = Output
  { _20dB :: StatisticalUnits
  , _30dB :: StatisticalUnits
  , _60dB :: StatisticalUnits
  } deriving(Eq, Ord, Read, Show)

instance FromJSON Output where
  parseJSON = withObject "SilenceRate" $ \v -> Output
    <$> v .: "silence_rate_20dB"
    <*> v .: "silence_rate_30dB"
    <*> v .: "silence_rate_60dB"
