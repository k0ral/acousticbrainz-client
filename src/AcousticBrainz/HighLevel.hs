{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
module AcousticBrainz.HighLevel where

import           AcousticBrainz.FiniteDistribution
import           AcousticBrainz.MetaData
import qualified AcousticBrainz.HighLevel.Dortmund                 as Dortmund
import qualified AcousticBrainz.HighLevel.ElectronicClassification as ElectronicClassification
import qualified AcousticBrainz.HighLevel.ISMIR04Rhythm            as ISMIR04Rhythm
import qualified AcousticBrainz.HighLevel.MirexMood                as Mirex
import qualified AcousticBrainz.HighLevel.Rosamerica               as Rosamerica
import qualified AcousticBrainz.HighLevel.Tzanetakis               as Tzanetakis
import           AcousticBrainz.Version
import           MusicBrainz

import           Control.Monad.Catch
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Streaming                         as Q
import           Data.ByteString.Streaming.HTTP
import           Data.Scientific
import qualified Data.Text                                         as Text
import           Lens.Micro.Aeson

-- | Call the following endpoint: https://acousticbrainz.readthedocs.io/api.html#get--api-v1-(uuid-mbid)-high-level .
getHighLevelData :: MonadIO m => MonadThrow m => MusicBrainzIdentifier -> m HighLevelResponse
getHighLevelData (MusicBrainzIdentifier identifier) = io $ do
  request <- parseRequest $ "https://acousticbrainz.org/api/v1/" <> Text.unpack identifier <> "/high-level"
  manager <- newManager tlsManagerSettings
  withHTTP request manager $ \r ->
    r & responseBody & Q.toLazy_ <&> eitherDecode >>= liftEither
  where liftEither (Left e) = throwM $ HighLevelException e
        liftEither (Right a) = return a


data Danceability = Danceable | NotDanceable deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Danceability) where
  parseJSON = parseFiniteDistribution [(Danceable, "danceable"), (NotDanceable, "not_danceable")]


data Gender = Female | Male deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Gender) where
  parseJSON = parseFiniteDistribution [(Female, "female"), (Male, "male")]


data Acoustic = Acoustic | NotAcoustic deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Acoustic) where
  parseJSON = parseFiniteDistribution [(Acoustic, "acoustic"), (NotAcoustic, "not_acoustic")]


data Aggressive = Aggressive | NotAggressive deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Aggressive) where
  parseJSON = parseFiniteDistribution [(Aggressive, "aggressive"), (NotAggressive, "not_aggressive")]


data Electronic = Electronic | NotElectronic deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Electronic) where
  parseJSON = parseFiniteDistribution [(Electronic, "electronic"), (NotElectronic, "not_electronic")]


data Happy = Happy | NotHappy deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Happy) where
  parseJSON = parseFiniteDistribution [(Happy, "happy"), (NotHappy, "not_happy")]


data Party = Party | NotParty deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Party) where
  parseJSON = parseFiniteDistribution [(Party, "party"), (NotParty, "not_party")]


data Relaxed = Relaxed | NotRelaxed deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Relaxed) where
  parseJSON = parseFiniteDistribution [(Relaxed, "relaxed"), (NotRelaxed, "not_relaxed")]


data Sad = Sad | NotSad deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Sad) where
  parseJSON = parseFiniteDistribution [(Sad, "sad"), (NotSad, "not_sad")]


data Timbre = Bright | Dark deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Timbre) where
  parseJSON = parseFiniteDistribution [(Bright, "bright"), (Dark, "dark")]


data Tonal = Atonal | Tonal deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Tonal) where
  parseJSON = parseFiniteDistribution [(Atonal, "atonal"), (Tonal, "tonal")]


data Vocal = Instrumental | Vocal deriving(Eq, Ord, Read, Show)

instance FromJSON (FiniteDistribution Vocal) where
  parseJSON = parseFiniteDistribution [(Instrumental, "instrumental"), (Vocal, "voice")]


data Feature t = Feature Version (FiniteDistribution t)

deriving instance Eq t => Eq (Feature t)
deriving instance Ord t => Ord (Feature t)
deriving instance (Ord t, Read t) => Read (Feature t)
deriving instance Show t => Show (Feature t)

instance (FromJSON (FiniteDistribution t)) => FromJSON (Feature t) where
  parseJSON = withObject "Feature" $ \v -> Feature
    <$> v .: "version"
    <*> v .: "all"


data HighLevelData = HighLevelData
  { _danceability    :: Feature Danceability
  , _gender          :: Feature Gender
  , _genreDortmund   :: Feature Dortmund.Genre
  , _genreElectronic :: Feature ElectronicClassification.Genre
  , _genreRosamerica :: Feature Rosamerica.Genre
  , _genreTzanetakis :: Feature Tzanetakis.Genre
  , _ismir04rhythm   :: Feature ISMIR04Rhythm.Genre
  , _mirexMood       :: Feature Mirex.Mood
  , _moodAcoustic    :: Feature Acoustic
  , _moodAggressive  :: Feature Aggressive
  , _moodElectronic  :: Feature Electronic
  , _moodHappy       :: Feature Happy
  , _moodParty       :: Feature Party
  , _moodRelaxed     :: Feature Relaxed
  , _moodSad         :: Feature Sad
  , _timbre          :: Feature Timbre
  , _tonal           :: Feature Tonal
  , _vocal           :: Feature Vocal
  } deriving(Eq, Ord, Read, Show)

instance FromJSON HighLevelData where
  parseJSON = withObject "highlevel" $ \v -> HighLevelData
    <$> v .: "danceability"
    <*> v .: "gender"
    <*> v .: "genre_dortmund"
    <*> v .: "genre_electronic"
    <*> v .: "genre_rosamerica"
    <*> v .: "genre_tzanetakis"
    <*> v .: "ismir04_rhythm"
    <*> v .: "moods_mirex"
    <*> v .: "mood_acoustic"
    <*> v .: "mood_aggressive"
    <*> v .: "mood_electronic"
    <*> v .: "mood_happy"
    <*> v .: "mood_party"
    <*> v .: "mood_relaxed"
    <*> v .: "mood_sad"
    <*> v .: "timbre"
    <*> v .: "tonal_atonal"
    <*> v .: "voice_instrumental"



parseMetaData = withObject "metadata" $ \v -> MetaData
  <$> v .: "audio_properties"
  <*> v .: "tags"
  <*> (v .: "version" >>= withObject "version" (.: "highlevel"))

data HighLevelResponse = HighLevelResponse
  { _data     :: HighLevelData
  , _metadata :: MetaData
  } deriving(Eq, Read, Show)

instance FromJSON HighLevelResponse where
  parseJSON = withObject "response" $ \v -> HighLevelResponse
    <$> v .: "highlevel"
    <*> (v .: "metadata" >>= parseMetaData)


newtype HighLevelException = HighLevelException String deriving(Eq, Ord, Read, Show)
instance Exception HighLevelException
