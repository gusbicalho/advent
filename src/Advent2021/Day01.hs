module Advent2021.Day01 where

import Helpers qualified

run :: IO ()
run = do
  input <- getInput
  putStrLn "Part 1:"
  putStrLn $ "Example: " <> show (countIncrements example)
  putStrLn $ "Final: " <> show (countIncrements input)
  putStrLn "Part 2:"
  putStrLn $ "Example: " <> show (countIncrements (windows example))
  putStrLn $ "Final: " <> show (countIncrements (windows input))

windows :: [Word] -> [Word]
windows (a : more@(b : c : _)) = a + b + c : windows more
windows _ = []

countIncrements :: [Word] -> Word
countIncrements [] = 0
countIncrements (w : ws) = go 0 w ws
 where
  go !incs previous = \case
    [] -> incs
    (next : more)
      | next > previous -> go (incs + 1) next more
      | otherwise -> go incs next more

example :: [Word]
example =
  [ 199
  , 200
  , 208
  , 210
  , 200
  , 207
  , 240
  , 269
  , 260
  , 263
  ]

getInput :: IO [Word]
getInput = Helpers.readInputLines "inputs/2021/day01.txt"
