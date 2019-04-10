{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | K-Pop mood classification: https://www.music-ir.org/mirex/wiki/2019:Audio_K-POP_Mood_Classification .
module AcousticBrainz.HighLevel.MirexMood where

import           AcousticBrainz.FiniteDistribution

import           Data.Aeson
import           Data.Aeson.Types

data Mood = Cluster1 | Cluster2 | Cluster3 | Cluster4 | Cluster5
  deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Mood) where
  parseJSON = parseFiniteDistribution
    [ (Cluster1, "Cluster1")
    , (Cluster2, "Cluster2")
    , (Cluster3, "Cluster3")
    , (Cluster4, "Cluster4")
    , (Cluster5, "Cluster5")
    ]
