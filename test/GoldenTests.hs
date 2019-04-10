{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
import           AcousticBrainz.HighLevel
import           AcousticBrainz.LowLevel
import           MusicBrainz

import           Data.Aeson
import           Data.String
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import qualified Data.Text.Lazy.Encoding  as Lazy
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.Golden        (findByExtension, goldenVsString)
import           Text.Pretty.Simple

main :: IO ()
main = do
  a <- highLevelTests
  b <- lowLevelTests
  defaultMain $ testGroup "Golden tests" [a, b]

highLevelTests :: IO TestTree
highLevelTests = do
  files <- findByExtension [".highlevel"] "test"

  return $ testGroup "High level tests" $ do
    file <- files
    let goldenFile = addExtension file ".golden"
    return $ goldenVsString file goldenFile $
      Text.readFile file <&> Text.strip >>= mkMusicBrainzIdentifier >>= getHighLevelData <&> pShowNoColor <&> Lazy.encodeUtf8

lowLevelTests :: IO TestTree
lowLevelTests = do
  files <- findByExtension [".lowlevel"] "test"

  return $ testGroup "Low level tests" $ do
    file <- files
    let goldenFile = addExtension file ".golden"
    return $ goldenVsString file goldenFile $
      Text.readFile file <&> Text.strip >>= mkMusicBrainzIdentifier >>= getLowLevelData <&> pShowNoColor <&> Lazy.encodeUtf8
