module Helpers where

import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.IO (IOMode (ReadMode), hIsEOF, withFile)
import Text.Read (readMaybe)

readInputLines :: forall inputType. Read inputType => FilePath -> IO [inputType]
readInputLines filePath =
  Maybe.catMaybes <$> withFile filePath ReadMode getWords
 where
  getWords handle = do
    hIsEOF handle >>= \case
      True -> pure []
      False -> do
        line <- Text.IO.hGetLine handle
        (readMaybe @inputType (Text.unpack line) :) <$> getWords handle
