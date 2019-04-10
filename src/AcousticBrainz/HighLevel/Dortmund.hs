{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Dortmund genre classification: http://www-ai.cs.uni-dortmund.de/audio.html .
module AcousticBrainz.HighLevel.Dortmund where

import           AcousticBrainz.FiniteDistribution

import           Data.Aeson
import           Data.Aeson.Types

data Genre = Alternative | Blues | Electronic | FolkCountry | FunkSoulRnB | Jazz | Pop | Raphiphop | Rock
  deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Genre) where
  parseJSON = parseFiniteDistribution
    [(Alternative, "alternative")
    , (Blues, "blues")
    , (Electronic, "electronic")
    , (FolkCountry, "folkcountry")
    , (FunkSoulRnB, "funksoulrnb")
    , (Jazz, "jazz")
    , (Pop, "pop")
    , (Raphiphop, "raphiphop")
    , (Rock, "rock")
    ]
