{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
module AcousticBrainz.FiniteDistribution where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Scientific

newtype FiniteDistribution a = FiniteDistribution (Map a Scientific)

deriving instance Eq a => Eq (FiniteDistribution a)
deriving instance Ord a => Ord (FiniteDistribution a)
deriving instance (Ord a, Read a) => Read (FiniteDistribution a)
deriving instance Show a => Show (FiniteDistribution a)



parseFiniteDistribution :: Ord a => [(a, Text)] -> Value -> Parser (FiniteDistribution a)
parseFiniteDistribution valueKeys = withObject "all" $ \v ->
  FiniteDistribution . fromList <$> sequence (do
    (value, name) <- valueKeys
    return $ (value,) <$> v .: name
  )



