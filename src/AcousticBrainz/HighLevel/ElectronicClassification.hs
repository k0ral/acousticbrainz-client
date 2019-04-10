{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
module AcousticBrainz.HighLevel.ElectronicClassification where

import           AcousticBrainz.FiniteDistribution

import           Data.Aeson                        as JSON
import           Data.Aeson.Types                  as JSON


data Genre = Ambient | DnB | House | Techno | Trance
  deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Genre) where
  parseJSON = parseFiniteDistribution
    [ (Ambient, "ambient")
    , (DnB, "dnb")
    , (House, "house")
    , (Techno, "techno")
    , (Trance, "trance")
    ]

