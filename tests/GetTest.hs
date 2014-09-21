{-# OPTIONS_GHC -fno-monomorphism-restriction #-}
module Main where

import Control.Monad (MonadPlus(..))
import qualified Data.ByteString as B
import Data.Binary.Strict.Class
import qualified Data.Binary.Strict.Get as G
import qualified Data.Binary.Strict.IncrementalGet as IG
import Control.Applicative

import Debug.Trace (trace)

debug x = trace (show x) x

testP1 = getWord32be
testP2 = mzero <|> getWord32be
testP3 = (getWord8 >> fail "a") <|> getWord32be
testP4 = do
  let f = do
        a <- b <|> c
        fail ""
        return a
      b = getWord32be
      c = undefined

  f <|> getWord32be

testP5 = (getWord8 >> getWord8 >> getWord8 >> getWord8 >> fail "") <|> getWord32be

testP6 = do
  a <- spanOf (== 0)
  b <- getWord8

  if B.length a /= 3
     then fail "a"
     else return (a, b)

--incrementalTest :: (Eq r, Show r, BinaryParser g) => g r -> B.ByteString -> Bool
incrementalTest p input = a == b && resta == restb where
  IG.Finished resta a = IG.runGet p input
  IG.Finished restb b = f input $ IG.runGet p B.empty
  f bs (IG.Partial cont)
    | B.length bs == 1 = cont bs
    | otherwise = f (B.tail bs) $ cont $ B.singleton $ B.head bs

main = do
  print $ G.runGet testP1 $ B.pack [0, 0, 0, 1]
  print $ G.runGet testP2 $ B.pack [0, 0, 0, 1]
  print $ G.runGet testP3 $ B.pack [0, 0, 0, 1]
  print $ G.runGet testP4 $ B.pack [0, 0, 0, 1]
  print $ G.runGet testP5 $ B.pack [0, 0, 0, 1]
  print $ G.runGet testP6 $ B.pack [0, 0, 0, 1]
  print $ incrementalTest testP1 $ B.pack [0, 0, 0, 1]
  print $ incrementalTest testP2 $ B.pack [0, 0, 0, 1]
  print $ incrementalTest testP3 $ B.pack [0, 0, 0, 1]
  print $ incrementalTest testP4 $ B.pack [0, 0, 0, 1]
  print $ incrementalTest testP5 $ B.pack [0, 0, 0, 1]
  print $ incrementalTest testP6 $ B.pack [0, 0, 0, 1]
