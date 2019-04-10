{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
module AcousticBrainz.HighLevel.Rosamerica where

import           AcousticBrainz.FiniteDistribution

import           Data.Aeson                        as JSON
import           Data.Aeson.Types                  as JSON

data Genre = Cla | Dan | Hip | Jaz | Pop | Rhy | Roc | Spe
  deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Genre) where
  parseJSON = parseFiniteDistribution
    [ (Cla, "cla")
    , (Dan, "dan")
    , (Hip, "hip")
    , (Jaz, "jaz")
    , (Pop, "pop")
    , (Rhy, "rhy")
    , (Roc, "roc")
    , (Spe, "spe")
    ]
