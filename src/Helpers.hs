module Helpers where

import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.IO (IOMode (ReadMode), hIsEOF, withFile)
import Text.Read (readMaybe)

readInputLines :: forall inputType. Read inputType => FilePath -> IO [inputType]
readInputLines = parseInputLines (readMaybe . Text.unpack)

parseInputLines :: (Text -> Maybe inputType) -> FilePath -> IO [inputType]
parseInputLines parse filePath =
  Maybe.catMaybes <$> withFile filePath ReadMode getWords
 where
  getWords handle = do
    hIsEOF handle >>= \case
      True -> pure []
      False -> do
        line <- Text.IO.hGetLine handle
        (parse line :) <$> getWords handle

readLines :: Read inputType => [Text] -> [inputType]
readLines = parseLines (readMaybe . Text.unpack)

parseLines :: (Text -> Maybe inputType) -> [Text] -> [inputType]
parseLines = Maybe.mapMaybe
