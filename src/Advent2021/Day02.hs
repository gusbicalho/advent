{-# LANGUAGE DerivingVia #-}

module Advent2021.Day02 (run) where

import Data.Coerce (coerce)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Monoid (Sum (Sum))
import Data.Text (Text)
import Data.Text qualified as Text
import Helpers qualified
import Text.Read (readMaybe)

run :: IO ()
run = do
  input <- Helpers.parseInputLines parseInstruction "inputs/2021/day02.txt"
  putStrLn "Part 1"
  putStrLn $ "Example position: " <> show (simulatePart1 example)
  let finalPosition = simulatePart1 input
  putStrLn $ "Final position: " <> show finalPosition
  putStrLn $ "Response: " <> show (finalPosition & \(Pos (x, y)) -> x * y)
  putStrLn "Part 2"
  putStrLn $ "Example position: " <> show (simulatePart2 example)
  let finalPosition = simulatePart2 input
  putStrLn $ "Final position: " <> show finalPosition
  putStrLn $ "Response: " <> show (finalPosition & \(Pos (x, y)) -> x * y)

simulatePart2 :: [Instruction] -> Position
simulatePart2 = snd . foldl' execute (mempty, mempty)
 where
  execute (aim, pos) (I direction n) =
    let aimChange = case direction of
          Forward -> mempty
          Up -> Aim . negate . fromIntegral $ n
          Down -> Aim . fromIntegral $ n
        posChange = case direction of
          Forward ->
            let n' = fromIntegral n :: Int
             in Pos (n', n' * getAim aim)
          _ -> mempty
     in (aim <> aimChange, pos <> posChange)

newtype Aim = Aim {getAim :: Int}
  deriving (Semigroup, Monoid) via (Sum Int)

simulatePart1 :: [Instruction] -> Position
simulatePart1 = coerce . foldMap (coerce @_ @Position . toPositionDelta)
 where
  toPositionDelta :: Instruction -> (Int, Int)
  toPositionDelta (I Forward n) = (fromIntegral n, 0)
  toPositionDelta (I Up n) = (0, negate . fromIntegral $ n)
  toPositionDelta (I Down n) = (0, fromIntegral n)

newtype Position = Pos (Int, Int)
  deriving (Semigroup, Monoid) via (Sum Int, Sum Int)
  deriving stock (Show)
data Direction = Forward | Up | Down
data Instruction = I !Direction !Word

parseInstruction :: Text -> Maybe Instruction
parseInstruction (Text.break (== ' ') -> (front, tail)) =
  I <$> parseDirection front <*> readMaybe (Text.unpack tail)

parseDirection :: Text -> Maybe Direction
parseDirection t
  | t == "forward" = Just Forward
  | t == "up" = Just Up
  | t == "down" = Just Down
  | otherwise = Nothing

example :: [Instruction]
example =
  Helpers.parseLines
    parseInstruction
    [ "forward 5"
    , "down 5"
    , "forward 8"
    , "up 3"
    , "down 8"
    , "forward 2"
    ]
