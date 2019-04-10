{-# LANGUAGE OverloadedStrings #-}
module AcousticBrainz.LowLevel.CepstralCoefficients where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Scientific


data CepstralCoefficients = CepstralCoefficients
  { _covariance        :: [[Scientific]]
  , _inverseCovariance :: [[Scientific]]
  , _mean              :: [Scientific]
  } deriving(Eq, Ord, Read, Show)

instance FromJSON CepstralCoefficients where
  parseJSON = withObject "CepstralCoefficients" $ \v -> CepstralCoefficients
    <$> v .: "cov"
    <*> v .: "icov"
    <*> v .: "mean"
