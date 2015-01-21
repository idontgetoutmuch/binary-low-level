{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Word (Word8, Word16)
import Data.List (unfoldr)
import Data.Bits (shiftL, (.&.))
import Data.ByteString.Char8 ()
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary.BitBuilder as BB

import Test.QuickCheck hiding ((.&.))

-- This test constructs random sequences of elements from the following list.
-- The first element of each pair is some BitBuilder and the second element
-- is a list of the bits that such a BitBuilder should append. Then we run
-- the sequence of BitBuilders and concat the sequence of [Bool] and check
-- that the output is the same

type TestElement = (BB.BitBuilder, [Bool])
elems :: [TestElement]
elems = [ (BB.singleton True, [True])
        , (BB.singleton False, [False])
        , (BB.fromByteString ("\x01\x02", 0), byteToBits 1 ++ byteToBits 2)
        , (BB.fromByteString ("\x01\x02", 4), byteToBits 1 ++ replicate 4 False)
        , (BB.fromByteString ("\x01\x02", 1), byteToBits 1 ++ [False])
        , (BB.fromByteString ("", 0), [])
        , (BB.fromBits 0 (0::Int), [])
        , (BB.fromByteString ("\xfc", 7), replicate 6 True ++ [False])
        , (BB.fromBits 7 (0xfe::Word8), replicate 6 True ++ [False])
        , (BB.fromBits 3 (3::Word8), [False, True, True])
        , (BB.fromBits 64 (0::Word16), replicate 64 False)
        ]

lbsToBits = concatMap byteToBits . L.unpack
byteToBits = unfoldr f . (,) 8 where
  f (0, _) = Nothing
  f (n, byte) = Just (byte .&. 0x80 == 0x80, (n-1, byte `shiftL` 1))

zeroExtend :: [Bool] -> [Bool]
zeroExtend x = x ++ (replicate ((8 - (length x)) `mod` 8) False)

prop_order xs = lbsToBits (BB.toLazyByteString bb) == zeroExtend list where
  es = map (\x -> elems !! (x `mod` (length elems))) xs
  bb = foldr mappend mempty $ map fst es
  list = concat $ map snd es

main = quickCheckWith (stdArgs { maxSize = 10000}) prop_order
