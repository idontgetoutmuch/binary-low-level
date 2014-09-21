module Main where

import qualified Data.ByteString as B
import Data.Word

import qualified Data.Binary.Strict.BitGet as BG

t :: (Eq a, Show a) => [Word8] -> BG.BitGet a -> a -> Bool
t bytes m v = if result == v then True else error (show (bytes, v, result)) where
  Right result = BG.runBitGet (B.pack bytes) m

tests = [
    t [1] BG.getWord8 1
  , t [128] BG.getBit True
  , t [64] BG.getBit False
  , t [1, 0] BG.getWord16be 256
  , t [1, 2] BG.getWord16be 258
  , t [1, 0] BG.getWord16le 1
  , t [2, 1] BG.getWord16le 258
  , t [192, 0] (BG.getBit >> BG.getWord8) 128
  , t [193, 0] (BG.getBit >> BG.getWord8) 130
  , t [193, 42] (sequence (take 8 $ repeat BG.getBit) >> BG.getWord8) 42
  , t [193] (BG.getAsWord8 3) 6
  , t [193] (BG.getAsWord8 4) 12
  , t [193, 128] (BG.skip 4 >> BG.getAsWord8 4) 1
  , t [193, 128] (BG.skip 8 >> BG.getAsWord8 4) 8
  , t [1, 2, 3, 0] (BG.getAsWord32 24) 66051
  , t [1, 2, 3, 0] (BG.getAsWord32 8) 1
  , t [1, 2, 3, 4, 5] (BG.getAsWord64 40) 4328719365
  ]

main = do
  print $ length $ filter id tests
  putStrLn "PASS"
