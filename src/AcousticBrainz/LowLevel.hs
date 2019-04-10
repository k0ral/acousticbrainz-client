{-# LANGUAGE OverloadedStrings #-}
-- | Low-level descriptors: https://essentia.upf.edu/documentation/streaming_extractor_music.html#low-level .
module AcousticBrainz.LowLevel where

import           AcousticBrainz.StatisticalUnits
import           AcousticBrainz.MetaData
import qualified AcousticBrainz.LowLevel.Algorithm.SilenceRate as SilenceRate
import           AcousticBrainz.LowLevel.BarkBands
import           AcousticBrainz.LowLevel.CepstralCoefficients
import qualified AcousticBrainz.LowLevel.EquivalentRectangularBandwidth as EquivalentRectangularBandwidth
import           AcousticBrainz.LowLevel.MelBands
import           AcousticBrainz.LowLevel.Rhythm
import           AcousticBrainz.LowLevel.Spectral
import           AcousticBrainz.LowLevel.Tonal
import           MusicBrainz

import           Control.Monad.Catch
import           Data.Aeson                     as JSON
import           Data.Aeson.Types               as JSON
import qualified Data.ByteString.Lazy           as Lazy
import qualified Data.ByteString.Streaming      as Q
import           Data.ByteString.Streaming.HTTP
import           Data.Scientific
import qualified Data.Text                      as Text

-- | Call the following endpoint: https://acousticbrainz.readthedocs.io/api.html#get--api-v1-(uuid-mbid)-low-level .
getLowLevelData :: MonadIO m => MonadThrow m => MusicBrainzIdentifier -> m LowLevelResponse
getLowLevelData (MusicBrainzIdentifier identifier) = io $ do
  request <- parseRequest $ "https://acousticbrainz.org/api/v1/" <> Text.unpack identifier <> "/low-level"
  manager <- newManager tlsManagerSettings
  withHTTP request manager $ \r ->
    r & responseBody & Q.toLazy_ <&> eitherDecode >>= liftEither
  where liftEither (Left e) = throwM $ LowLevelException e
        liftEither (Right a) = return a

data LowLevelResponse = LowLevelResponse
  { _data     :: LowLevelData
  , _tonal    :: Tonal
  , _rhythm   :: Rhythm
  , _metadata :: MetaData
  } deriving(Eq, Read, Show)

instance FromJSON LowLevelResponse where
  parseJSON = withObject "response" $ \v -> LowLevelResponse
    <$> v .: "lowlevel"
    <*> v .: "tonal"
    <*> v .: "rhythm"
    <*> (v .: "metadata" >>= parseMetaData)


parseMetaData = withObject "metadata" $ \v -> MetaData
  <$> v .: "audio_properties"
  <*> v .: "tags"
  <*> v .: "version"


data LowLevelData = LowLevelData
  { _averageLoudness :: Scientific
  , _barkBands :: BarkBands
  , _dissonance :: StatisticalUnits
  , _dynamicComplexity :: Scientific
  , _equivalentRectangularBandwidthBands :: EquivalentRectangularBandwidth.Bands
  , _gfcc :: CepstralCoefficients
  , _highFrequencyContent :: StatisticalUnits
  , _melBands :: MelBands
  , _mfcc :: CepstralCoefficients
  , _pitchSalience :: StatisticalUnits
  , _silenceRate :: SilenceRate.Output
  , _spectral :: Spectral
  , _zeroCrossingRate :: StatisticalUnits
  } deriving(Eq, Ord, Read, Show)

instance FromJSON LowLevelData where
  parseJSON value = flip (withObject "lowlevel") value $ \v -> LowLevelData
    <$> v .: "average_loudness"
    <*> parseJSON value
    <*> v .: "dissonance"
    <*> v .: "dynamic_complexity"
    <*> parseJSON value
    <*> v .: "gfcc"
    <*> v .: "hfc"
    <*> parseJSON value
    <*> v .: "mfcc"
    <*> v .: "pitch_salience"
    <*> parseJSON value
    <*> parseJSON value
    <*> v .: "zerocrossingrate"


newtype LowLevelException = LowLevelException String deriving(Eq, Ord, Read, Show)
instance Exception LowLevelException
