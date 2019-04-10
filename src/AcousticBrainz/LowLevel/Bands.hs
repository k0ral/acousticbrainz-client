module AcousticBrainz.LowLevel.Bands where

import           AcousticBrainz.StatisticalUnits

data Bands = Bands
  { _bands      :: [StatisticalUnits]
  , _crest      :: StatisticalUnits
  , _flatnessDb :: StatisticalUnits
  , _kurtosis   :: StatisticalUnits
  , _skewness   :: StatisticalUnits
  , _spread     :: StatisticalUnits
  } deriving(Eq, Ord, Read, Show)
