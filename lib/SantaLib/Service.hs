{-# LANGUAGE OverloadedStrings #-}

module SantaLib.Service where

import Advent.Types
import Control.Monad
import Data.Bifunctor
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Servant.API
import Text.Read (readEither, readMaybe)

parseDay :: Text -> Either Text Day
parseDay txt = do
  num <- maybe (Left $ "invalid day: " <> txt) Right $ readMaybe (T.unpack txt)
  maybe (Left $ "invalid day: " <> T.show num) Right $ mkDay num

parsePart :: Text -> Either Text Part
parsePart txt = do
  num <- maybe (Left $ "invalid part: " <> txt) Right $ readMaybe (T.unpack txt)
  case num of
    1 -> return Part1
    2 -> return Part2
    _ -> Left $ "invalid part: " <> T.show num

instance FromHttpApiData Day where
  parseUrlPiece = parseDay
  parseHeader = parseHeader >=> parseDay
  parseQueryParam = parseDay

instance FromHttpApiData Part where
  parseUrlPiece = parsePart
  parseHeader = parseHeader >=> parsePart
  parseQueryParam = parsePart

type AocAPI =
  "solve"
    :> Capture "day" Day
    :> Capture "part" Part
    :> QueryParam' [Required, Strict] "input" Text
    :> Post '[JSON] Text

aocApi :: Proxy AocAPI
aocApi = Proxy @AocAPI
