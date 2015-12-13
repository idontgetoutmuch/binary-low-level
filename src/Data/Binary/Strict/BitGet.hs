{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
-- for unboxed shifts
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Strict.BitGet
-- Copyright   : Adam Langley
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Adam Langley <agl@imperialviolet.org>
-- Stability   : experimental
--
-- This is a reader monad for parsing bit-aligned data. The usual Get monad
-- handles byte aligned data well.
--
-- In this monad, the current offset into the input is a number of bits, and
-- fetching n bits from the current position will shift everything correctly.
-- Bit vectors are represented as ByteStrings here either the first @n@ bits
-- are valid (left aligned) or the last @n@ bits are (right aligned).
--
-- If one is looking to parse integers etc, right alignment is the easist to
-- work with, however left alignment makes more sense in some situations.
-----------------------------------------------------------------------------

module Data.Binary.Strict.BitGet (
  -- * Get @BitGet@ type
    BitGet
  , runBitGet

  -- * Utility
  , skip
  , remaining
  , isEmpty
  , lookAhead

  -- * Generic parsing
  , getBit
  , getLeftByteString
  , getRightByteString

  -- ** Interpreting some number of bits as an integer
  , getAsWord8
  , getAsWord16
  , getAsWord32
  , getAsWord64

  -- ** Parsing particular types
  , getWord8
  , getWord16le
  , getWord16be
  , getWord16host
  , getWord32le
  , getWord32be
  , getWord32host
  , getWord64le
  , getWord64be
  , getWord64host
  , getWordhost
) where

#include "Common.h"

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Data.Binary.Strict.BitUtil
import Foreign
import Data.Bits

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word
#endif

#ifndef __HADDOCK__
data S = S {-# UNPACK #-} !B.ByteString  -- input
           {-# UNPACK #-} !Word8  -- bit offset in current byte
#endif

newtype BitGet a = BitGet { unGet :: S -> (Either String a, S) }

instance Functor BitGet where
  fmap = liftM

instance Applicative BitGet where
  pure  = return
  (<*>) = ap

instance Monad BitGet where
  return a = BitGet (\s -> (Right a, s))
  m >>= k = BitGet (\s -> case unGet m s of
                            (Left err, s') -> (Left err, s')
                            (Right a, s') -> unGet (k a) s')
  fail err = BitGet (\s -> (Left err, s))

-- | Run a BitGet on a ByteString
runBitGet :: B.ByteString -> BitGet a -> Either String a
runBitGet input m =
  case unGet m (S input 0) of
    (a, _) -> a

get :: BitGet S
get = BitGet (\s -> (Right s, s))

put :: S -> BitGet ()
put s = BitGet (const (Right (), s))

-- | Run @ga@, but return without consuming its input.
-- Fails if @ga@ fails.
lookAhead :: BitGet a -> BitGet a
lookAhead g = do
    s <- get
    a <- g
    put s
    return a

-- | Same as the standard splitAt, but in this version both parts share a byte
--   so that splitting [1,2,3,4] at 2 results in ([1,2], [2, 3, 4]).
splitAtWithDupByte :: Int -> B.ByteString -> (B.ByteString, B.ByteString)
splitAtWithDupByte n bs = (B.take n bs, B.drop (n - 1) bs)

-- | Used as a flag argument to readN to control weather the resulting
--   ByteString is left or right aligned
data Direction = BLeft | BRight deriving (Show)

-- | Fetch some number of bits from the input and return them as a ByteString
--   after applying the given function
readN :: Direction -> Int -> (B.ByteString -> a) -> BitGet a
readN d n f = do
  S bytes boff <- get
  let bitsRemaining = B.length bytes * 8 - boffInt
      boffInt = fromIntegral boff
      (shiftFunction, truncateFunction) =
        case d of
             BLeft -> (leftShift, leftTruncateBits)
             BRight -> (\off -> rightShift $ (((8 - (n `mod` 8)) `mod` 8) - off) `mod` 8,
                        rightTruncateBits)
  if bitsRemaining < n
     then fail "Too few bits remain"
     else do let bytesRequired = ((n - 1 + boffInt) `div` 8) + 1 -- (n `div` 8) + (if boffInt + (n `mod` 8) > 0 then 1 else 0)
                 boff' = (boffInt + n) `mod` 8
             let (r, rest) = if boff' == 0
                                then B.splitAt bytesRequired bytes
                                else splitAtWithDupByte bytesRequired bytes
             put $ S rest $ fromIntegral boff'
             return $ f $ truncateFunction n $ shiftFunction boffInt r

-- | Skip @n@ bits of the input. Fails if less then @n@ bits remain
skip :: Int -> BitGet ()
skip n = readN BLeft (fromIntegral n) (const ())

-- | Return the number of bits remaining to be parsed
remaining :: BitGet Int
remaining = do
  S bytes boff <- get
  return $ B.length bytes * 8 - fromIntegral boff

-- | Return true if there are no more bits to parse
isEmpty :: BitGet Bool
isEmpty = do
  S bytes _ <- get
  return $ B.null bytes

getPtr :: Storable a => Int -> BitGet a
getPtr n = do
    (fp, o, _) <- readN BRight (n * 8) BI.toForeignPtr
    return . BI.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)
{-# INLINE getPtr #-}

-- | Get a single bit from the input
getBit :: BitGet Bool
getBit = readN BRight 1 (not . ((==) 0) . B.head)

-- | Get a ByteString with the given number of bits, left aligned.
getLeftByteString :: Int -> BitGet B.ByteString
getLeftByteString n = readN BLeft n id

-- | Get a ByteString with the given number of bits in, right aligned.
getRightByteString :: Int -> BitGet B.ByteString
getRightByteString n = readN BRight n id

getRightByteStringBytes :: Int -> BitGet B.ByteString
getRightByteStringBytes = getRightByteString . ((*) 8)

leftPad :: Int -> B.ByteString -> B.ByteString
leftPad len bs = if B.length bs < len then padded else bs where
  padded = (B.pack $ take extraBytes $ repeat 0) `B.append` bs
  extraBytes = len - B.length bs

GETWORDS(BitGet, getRightByteStringBytes)
GETHOSTWORDS(BitGet)

getAsWord8 :: Int -> BitGet Word8
getAsWord8 n = readN BRight n $ (flip B.index) 0

-- | Read a Word16 in big endian format
getAsWord16 :: Int -> BitGet Word16
getAsWord16 n = do
    s <- readN BRight n id >>= return . leftPad 2
    return $! DECWORD16BE(s)
{-# INLINE getWord16be #-}

-- | Read a Word32 in big endian format
getAsWord32 :: Int -> BitGet Word32
getAsWord32 n = do
    s <- readN BRight n id >>= return . leftPad 4
    return $! DECWORD32BE(s)
{-# INLINE getWord32be #-}

-- | Read a Word64 in big endian format
getAsWord64 :: Int -> BitGet Word64
getAsWord64 n = do
    s <- readN BRight n id >>= return . leftPad 8
    return $! DECWORD64BE(s)
{-# INLINE getWord64be #-}

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)
#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
#endif

