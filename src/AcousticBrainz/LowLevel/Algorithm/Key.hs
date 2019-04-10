{-# LANGUAGE OverloadedStrings #-}
-- | Key algorithm: https://essentia.upf.edu/documentation/reference/streaming_Key.html .
module AcousticBrainz.LowLevel.Algorithm.Key where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Scientific


data Output = Output
  { _key :: Text
  , _scale :: Scale
  , _strength :: Scientific
  , _tuningDiatonicStrength :: Scientific
  } deriving(Eq, Ord, Read, Show)

instance FromJSON Output where
  parseJSON = withObject "key" $ \v -> Output
    <$> v .: "key_key"
    <*> v .: "key_scale"
    <*> v .: "key_strength"
    <*> v .: "tuning_diatonic_strength"

-- data Key = A | B | C | D | E | F | G deriving(Eq, Ord, Read, Show)

-- instance FromJSON Key where
--   parseJSON = withText "key" $ \t ->
--     either (const $ fail $ "Invalid key: " <> toString t) return $ readEither t

data Scale = Major | Minor deriving(Eq, Ord, Read, Show)

instance FromJSON Scale where
  parseJSON = withText "scale" $ \t ->
    if t == "major"
    then return Major
    else if t == "minor"
    then return Minor
    else fail $ "Invalid key scale: " <> toString t
