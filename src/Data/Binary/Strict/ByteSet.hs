-- | A ByteSet is a fast Set object for Word8's. The construction of these
--   objects isn't terribly quick, but the member function should be about
--   as good as you can get. Thus, you should use this when @member@ is the
--   most common operation
--
--   This object is designed to be imported qualified:
--
--   > import qualified Data.Binary.Strict.ByteSet as BSet
module Data.Binary.Strict.ByteSet
  ( ByteSet
  -- * Construction
  , empty
  , full
  , singleton
  , fromList
  , range

  -- * Combination
  , union
  , intersection
  , difference

  -- * Uniary functions
  , complement
  , toList
  , member

  ) where

import Data.List (foldl')
import Data.Word (Word8, Word64)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed hiding ((!), range)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.Bits as Bits

data ByteSet = ByteSet !(UArray Word8 Word64)

-- | We don't import the usual array indexing function from the array
--   modules. Instead, we implement it ourselves and remove the bounds
--   checking
(!) :: UArray Word8 Word64 -> Word8 -> Word64
(!) arr = unsafeAt arr . fromIntegral

instance Show ByteSet where
  show = ((++) "ByteSet ") . show . toList

-- | An empty set
empty :: ByteSet
empty = ByteSet $ listArray (0, 3) $ replicate 4 0

-- | The set contained all elements
full :: ByteSet
full = ByteSet $ listArray (0, 3) $ replicate 4 0xffffffffffffffff

-- | A set with a single element
singleton :: Word8 -> ByteSet
singleton n
  | n < 64 = ByteSet $ listArray (0, 3) $ [wordsArray ! n, 0, 0, 0]
  | n < 128 = ByteSet $ listArray (0, 3) $ [0, wordsArray ! (n - 64), 0, 0]
  | n < 196 = ByteSet $ listArray (0, 3) $ [0, 0, wordsArray ! (n - 128), 0]
  | otherwise = ByteSet $ listArray (0, 3) $ [0, 0, 0, wordsArray ! (n - 196)]

wordsArray :: UArray Word8 Word64
wordsArray = listArray (0, 63) $ map (\x -> (1 :: Word64) `shiftL` x) [0..63]

-- | A generic binary function
binary :: (Word64 -> Word64 -> Word64) -> ByteSet -> ByteSet -> ByteSet
binary f (ByteSet a) (ByteSet b) =
  ByteSet $ listArray (0, 3) [f (a ! 0) (b ! 0), f (a ! 1) (b ! 1), f (a ! 2) (b ! 2), f (a ! 3) (b ! 3)]

union :: ByteSet -> ByteSet -> ByteSet
union = binary (.|.)
intersection :: ByteSet -> ByteSet -> ByteSet
intersection = binary (.&.)
difference :: ByteSet -> ByteSet -> ByteSet
difference = binary (\a b -> a .&. Bits.complement b)

complement :: ByteSet -> ByteSet
complement (ByteSet a) =
  ByteSet $ amap Bits.complement a

word64ToList :: Word64 -> [Word8]
word64ToList x = filter (\n -> (x .&. ((1 :: Word64) `shiftL` (fromIntegral n))) /= 0) [0..63]

toList :: ByteSet -> [Word8]
toList (ByteSet a) =
  word64ToList (a ! 0) ++ map (+ 64) (word64ToList (a ! 1)) ++ map (+ 128) (word64ToList (a ! 2)) ++ map (+ 196) (word64ToList (a ! 3))

fromList :: [Word8] -> ByteSet
fromList = foldl' union empty . map singleton

-- | Construct a ByteSet containing all the elements from a to b, inclusive.
range :: Word8 -> Word8 -> ByteSet
range a b = fromList [a..b]

member :: ByteSet -> Word8 -> Bool
member (ByteSet a) n = (a ! (n `shiftR` 6)) .&. (wordsArray ! (n .&. 63)) /= 0
