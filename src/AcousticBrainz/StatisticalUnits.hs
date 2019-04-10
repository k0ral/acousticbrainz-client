{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
-- | Result of PoolAggregator algorithm: https://essentia.upf.edu/documentation/reference/streaming_PoolAggregator.html .
module AcousticBrainz.StatisticalUnits where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Scientific


data StatisticalUnits = StatisticalUnits
  { _meanOfDerivative :: Scientific
  , _meanOfSecondDerivative :: Scientific
  , _varianceOfDerivative :: Scientific
  , _varianceOfSecondDerivative :: Scientific
  , _maximum :: Scientific
  , _mean :: Scientific
  , _median :: Scientific
  , _minimum :: Scientific
  , _variance :: Scientific
  } deriving(Eq, Ord, Read, Show)

instance FromJSON StatisticalUnits where
  parseJSON = withObject "distribution metrics" $ \v -> StatisticalUnits
    <$> v .: "dmean"
    <*> v .: "dmean2"
    <*> v .: "dvar"
    <*> v .: "dvar2"
    <*> v .: "max"
    <*> v .: "mean"
    <*> v .: "median"
    <*> v .: "min"
    <*> v .: "var"

-- | This newtype is only useful for the associated 'FromJSON' instance.
newtype StatisticalUnitsList = StatisticalUnitsList { _statisticalUnitsList :: [StatisticalUnits] }

instance FromJSON StatisticalUnitsList where
  parseJSON = withObject "distribution metrics list" $ \v -> do
    dmean  <- v .: "dmean"
    dmean2 <- v .: "dmean2"
    dvar <- v .: "dvar"
    dvar2 <- v .: "dvar2"
    max <- v .: "max"
    mean <- v .: "mean"
    median <- v .: "median"
    min <- v .: "min"
    var <- v .: "var"
    let a = zipWith StatisticalUnits dmean dmean2
        b = zipWith ($) a dvar
        c = zipWith ($) b dvar2
        d = zipWith ($) c max
        e = zipWith ($) d mean
        f = zipWith ($) e median
        g = zipWith ($) f min
        h = zipWith ($) g var
    return $ StatisticalUnitsList h
