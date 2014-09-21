module Data.Binary.Strict.BitUtil
  ( topNBits
  , bottomNBits
  , leftShift
  , rightShift
  , leftTruncateBits
  , rightTruncateBits
  ) where

import Data.Word (Word8)
import qualified Data.ByteString as B
import Data.Bits (shiftL, shiftR, (.|.), (.&.))

-- | This is used for masking the last byte of a ByteString so that extra
--   bits don't leak in
topNBits :: Int -> Word8
topNBits 0 = 0
topNBits 1 = 0x80
topNBits 2 = 0xc0
topNBits 3 = 0xe0
topNBits 4 = 0xf0
topNBits 5 = 0xf8
topNBits 6 = 0xfc
topNBits 7 = 0xfe
topNBits 8 = 0xff
topNBits x = error ("topNBits undefined for " ++ show x)

-- | Return a Word8 with the bottom n bits set
bottomNBits :: Int -> Word8
bottomNBits 0 = 0
bottomNBits 1 = 0x01
bottomNBits 2 = 0x03
bottomNBits 3 = 0x07
bottomNBits 4 = 0x0f
bottomNBits 5 = 0x1f
bottomNBits 6 = 0x3f
bottomNBits 7 = 0x7f
bottomNBits 8 = 0xff
bottomNBits x = error ("bottomNBits undefined for " ++ show x)

-- | Shift the whole ByteString some number of bits left where 0 <= @n@ < 8
leftShift :: Int -> B.ByteString -> B.ByteString
leftShift 0 = id
leftShift n = snd . B.mapAccumR f 0 where
  f acc b = (b `shiftR` (8 - n), (b `shiftL` n) .|. acc)

-- | Shift the whole ByteString some number of bits right where 0 <= @n@ < 8
rightShift :: Int -> B.ByteString -> B.ByteString
rightShift 0 = id
rightShift n = snd . B.mapAccumL f 0 where
  f acc b = (b .&. (bottomNBits n), (b `shiftR` n) .|. (acc `shiftL` (8 - n)))

-- | Truncate a ByteString to a given number of bits (counting from the left)
--   by masking out extra bits in the last byte
leftTruncateBits :: Int -> B.ByteString -> B.ByteString
leftTruncateBits n = B.take ((n + 7) `div` 8) . snd . B.mapAccumL f n where
  f bits w | bits >= 8 = (bits - 8, w)
           | bits == 0 = (0, 0)
           | otherwise = (0, w .&. topNBits bits)

-- | Truncate a ByteString to a given number of bits (counting from the right)
--   by masking out extra bits in the first byte
rightTruncateBits :: Int -> B.ByteString -> B.ByteString
rightTruncateBits n bs = B.drop (B.length bs - ((n + 7) `div` 8)) $ snd $ B.mapAccumR f n bs where
  f bits w | bits >= 8 = (bits - 8, w)
           | bits == 0 = (0, 0)
           | otherwise = (0, w .&. bottomNBits bits)
