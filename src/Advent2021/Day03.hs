{-# LANGUAGE DerivingVia #-}

module Advent2021.Day03 where

import Control.Applicative (ZipList (ZipList))
import Data.Foldable (foldl')
import Data.Monoid (Ap (..), Sum (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Generic.Data (Generically (..))
import Helpers qualified

run :: IO ()
run = do
  input <- Helpers.parseInputLines parsePCBits "inputs/2021/day03.txt"
  putStrLn "Part 1"
  putStrLn $ "Example: " <> show (powerConsumption example)
  putStrLn $ "Final: " <> show (powerConsumption input)
  pure ()

powerConsumption :: [PCBits] -> Word
powerConsumption bits = gamma * epsilon
 where
  gamma = bitsToNum @Word $ summary mostCommon bits
  epsilon = bitsToNum @Word $ summary (bitflip . mostCommon) bits

summary :: (BitHistogram -> Bit) -> [PCBits] -> PCBits
summary pick =
  fmap pick . foldMap (fmap boolToBitHisto)

mostCommon :: BitHistogram -> Bit
mostCommon BH{countZeroes, countOnes} =
  if countZeroes > countOnes then Zero else One

boolToBitHisto :: Bit -> BitHistogram
boolToBitHisto Zero = BH 1 0
boolToBitHisto One = BH 0 1

data Bit = Zero | One

bitflip :: Bit -> Bit
bitflip Zero = One
bitflip One = Zero

data BitHistogram = BH {countZeroes :: Sum Word, countOnes :: Sum Word}
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via (Generically BitHistogram)

type PCBits = Ap ZipList Bit

bitsToNum :: Num a => PCBits -> a
bitsToNum = foldl' (\acc i -> acc * 2 + i) 0 . fmap bitToNum

bitToNum :: Num p => Bit -> p
bitToNum Zero = 0
bitToNum One = 1

parsePCBits :: Text -> Maybe PCBits
parsePCBits s = do
  let chars = Text.unpack s
  Ap . ZipList <$> traverse bit chars
 where
  bit '0' = Just Zero
  bit '1' = Just One
  bit _ = Nothing

example :: [PCBits]
example =
  Helpers.parseLines
    parsePCBits
    [ "00100"
    , "11110"
    , "10110"
    , "10111"
    , "10101"
    , "01111"
    , "00111"
    , "11100"
    , "10000"
    , "11001"
    , "00010"
    , "01010"
    ]
