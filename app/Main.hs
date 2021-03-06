{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternGuards #-}

module Main where

import Advent2021.Day01 qualified
import Advent2021.Day02 qualified
import Advent2021.Day03 qualified
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case getYearDay args of
    Nothing -> usage
    Just yearDay -> case yearDay of
      (2021, 1) -> Advent2021.Day01.run
      (2021, 2) -> Advent2021.Day02.run
      (2021, 3) -> Advent2021.Day03.run
      (year, day) -> unimplemented year day

usage :: IO ()
usage = putStrLn "Usage: advent <year> <day> - e.g. advent 2021 1"

unimplemented :: (Show a, Show b) => a -> b -> IO ()
unimplemented year day = putStrLn $ "Unimplemented " <> show year <> " " <> show day

getYearDay :: [String] -> Maybe (Word, Word)
getYearDay args
  | (yearStr : dayStr : _) <- args
    , Just year <- readMaybe yearStr
    , Just day <- readMaybe dayStr =
    Just (year, day)
  | otherwise = Nothing
