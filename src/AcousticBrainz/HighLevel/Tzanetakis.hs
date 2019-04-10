{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
module AcousticBrainz.HighLevel.Tzanetakis where

import           AcousticBrainz.FiniteDistribution

import           Data.Aeson                        as JSON
import           Data.Aeson.Types                  as JSON

data Genre = Blu | Cla | Cou | Dis | Hip | Jaz | Met | Pop | Reg | Roc
  deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Genre) where
  parseJSON = parseFiniteDistribution
    [ (Blu, "blu")
    , (Cla, "cla")
    , (Cou, "cou")
    , (Dis, "dis")
    , (Hip, "hip")
    , (Jaz, "jaz")
    , (Met, "met")
    , (Pop, "pop")
    , (Reg, "reg")
    , (Roc, "roc")
    ]

