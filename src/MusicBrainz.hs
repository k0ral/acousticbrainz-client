module MusicBrainz where

import           Control.Monad.Catch
import           Data.Text           as Text


-- | Cf https://musicbrainz.org/doc/MusicBrainz_Identifier .
newtype MusicBrainzIdentifier = MusicBrainzIdentifier Text deriving(Eq, Ord, Read, Show)

newtype InvalidMusicBrainzIdentifier = InvalidMusicBrainzIdentifier Text deriving(Eq, Ord, Read, Show)
instance Exception InvalidMusicBrainzIdentifier

mkMusicBrainzIdentifier :: MonadThrow m => Text -> m MusicBrainzIdentifier
mkMusicBrainzIdentifier rawIdentifier = do
  when (Text.length rawIdentifier /= 36) $ throwM $ InvalidMusicBrainzIdentifier rawIdentifier
  return $ MusicBrainzIdentifier rawIdentifier
