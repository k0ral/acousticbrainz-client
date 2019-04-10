{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | ISMIR 2004 rhythm classification.
module AcousticBrainz.HighLevel.ISMIR04Rhythm where

import           AcousticBrainz.FiniteDistribution

import           Data.Aeson
import           Data.Aeson.Types

data Genre = ChaChaCha | Jive | Quickstep | RumbaAmerican | RumbaInternational | RumbaMisc | Samba | Tango | VienneseWaltz | Waltz
  deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Genre) where
  parseJSON = parseFiniteDistribution
    [ (ChaChaCha, "ChaChaCha")
    , (Jive, "Jive")
    , (Quickstep, "Quickstep")
    , (RumbaAmerican, "Rumba-American")
    , (RumbaInternational, "Rumba-International")
    , (RumbaMisc, "Rumba-Misc")
    , (Samba, "Samba")
    , (VienneseWaltz, "VienneseWaltz")
    , (Waltz, "Waltz")
    ]
