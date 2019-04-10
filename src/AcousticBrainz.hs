{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module AcousticBrainz where

import           Relude.Extra.Map as Map
import           Data.Aeson                     as JSON
import           Data.Aeson.Types               as JSON
import qualified Data.ByteString                as Strict
import qualified Data.ByteString.Lazy           as Lazy
import qualified Data.ByteString.Streaming      as Q
import           Data.ByteString.Streaming.HTTP
import           Data.Scientific
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Text.Encoding             as Text
import           Lens.Micro.Platform
import           Lens.Micro.Aeson


-- countQueryString :: [MusicBrainzIdentifier] -> ByteString
-- countQueryString identifiers = "recording_ids=" <> value where
--   value = Strict.intercalate ";" $ do
--     MusicBrainzIdentifier i <- identifiers
--     return $ Text.encodeUtf8 i

-- newtype CountResponse = CountResponse Lazy.ByteString deriving(Read, Show)

-- getCount :: MonadIO m => [MusicBrainzIdentifier] -> m CountResponse
-- getCount identifiers = io $ do
--   request <- parseRequest "https://acousticbrainz.org/api/v1/count"
--   let request' = request { queryString = countQueryString identifiers }
--   manager <- newManager tlsManagerSettings
--   withHTTP request' manager $ \response -> CountResponse <$> Q.toLazy_ (responseBody response)


-- liftParser' :: String -> Either Text a -> Parser a
-- liftParser' _ (Right a) = return a
-- liftParser' typename (Left e) = fail $ "Unable to parse " <> typename <> toString e


-- asObject :: MonadFail m => Monad m => Value -> m Object
-- asObject (Object a) = pure a
-- asObject value = fail $ "expected object, got " <> show value

-- asNumber :: MonadFail m => Monad m => Value -> m Scientific
-- asNumber (Number a) = pure a
-- asNumber value = fail $ "expected number, got " <> show value

