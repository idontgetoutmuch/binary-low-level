{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.BitBuilder
-- Copyright   : Lennart Kolmodin, Ross Paterson, Adam Langley
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Adam Langley <agl@imperialviolet.org>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC
--
-- Efficient construction of lazy bytestrings, bit by bit.
-----------------------------------------------------------------------------

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Data.Binary.BitBuilder (
    -- * The Builder type
      BitBuilder
    , toLazyByteString

    -- * Constructing Builders
    , empty
    , singleton
    , append
    , fromByteString        -- :: S.ByteString -> BitBuilder
    , fromLazyByteString    -- :: L.ByteString -> BitBuilder
    , fromBits

    -- * Flushing the buffer state
    , flush
  ) where

import Foreign hiding (unsafePerformIO)
import Data.Monoid
import Data.Semigroup (Semigroup((<>)))
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import System.IO.Unsafe (unsafePerformIO)

#ifdef BYTESTRING_IN_BASE
import Data.ByteString.Base (inlinePerformIO)
import qualified Data.ByteString.Base as S
#else
import Data.ByteString.Internal (inlinePerformIO)
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy.Internal as L
#endif

import Data.Binary.Strict.BitUtil

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base hiding ( empty, foldr )
#endif

------------------------------------------------------------------------

-- | A 'BitBuilder' is an efficient way to build lazy 'L.ByteString's.
-- There are several functions for constructing 'BitBuilder's, but only one
-- to inspect them: to extract any data, you have to turn them into lazy
-- 'L.ByteString's using 'toLazyByteString'.
--
-- Internally, a 'BitBuilder' constructs a lazy 'L.Bytestring' by filling byte
-- arrays piece by piece.  As each buffer is filled, it is \'popped\'
-- off, to become a new chunk of the resulting lazy 'L.ByteString'.
-- All this is hidden from the user of the 'BitBuilder'.
--
-- This is closely based on the Builder monad, but this one deals with
-- single bits at a time.

newtype BitBuilder = BitBuilder {
        -- Invariant (from Data.ByteString.Lazy):
        --      The lists include no null ByteStrings.
        runBitBuilder :: (Buffer -> [S.ByteString]) -> Buffer -> [S.ByteString]
    }

instance Show BitBuilder where
  show = const "<BitBuilder>"

instance Semigroup BitBuilder where
  (<>) = append

instance Monoid BitBuilder where
    mempty  = empty
    mappend = append

------------------------------------------------------------------------

-- | /O(1)./ The empty BitBuilder, satisfying
--
--  * @'toLazyByteString' 'empty' = 'L.empty'@
--
empty :: BitBuilder
empty = BitBuilder id

-- | /O(1)./ A BitBuilder taking a single bit, satisfying
--
--  * @'toLazyByteString' ('singleton' b) = 'L.singleton' b@
--
singleton :: Bool -> BitBuilder
singleton bit = writeN 1 $ \p phase -> do
  byte <- peek p
  let mask = complement (0x80 `shiftR` phase)
      value = if not bit then 0 else 0x80 `shiftR` phase
  poke p $ (byte .&. mask) .|. value
{-# INLINE singleton #-}

fromByteString :: (S.ByteString, Int) -> BitBuilder
fromByteString (bs, bsPhase) = withPhase f where
  f phase
      -- the trival case, a no-op
    | S.length bs == 0 = empty
    | phase == 0 && bsPhase == 0 =
      -- if we are the start of a byte, and the bytestring is an exact number
      -- of bytes long, we can just include it in our output
        flush `append` (BitBuilder $ \k buf -> bs : k buf)
    | phase == 0 =
      -- if we are at the beginning of a byte, in general, we can still just
      -- include most of the string in our output, we just need to handle the
      -- partial byte at the end
        flush `append` (BitBuilder $ \k buf -> S.init bs :
                         (runBitBuilder (writeN bsPhase (\p _ -> poke p (S.last bs))) k buf))
    | otherwise =
      -- the fully general case. We take the first n bits from the bytestring,
      -- phase shift the rest and recurse.
        writeN (8 - phase) (mergeByte $ S.head bs) `mappend` fromByteString shiftedBS where
          mergeByte nextByte p phase = do
            byte <- peek p
            let takingBits = 8 - phase
                mask = topNBits phase
                a = topNBits takingBits .&. nextByte
                b = a `shiftR` phase
                c = (byte .&. mask) .|. b
            poke p c
          shiftedBS = (S.take newLength shifted, bsPhase')
          shifted = leftShift (8 - phase) bs
          oldBitLength =
            if bsPhase == 0
               then 8 * S.length bs
               else (S.length bs - 1) * 8 + bsPhase
          newLength = ((oldBitLength - (8 - phase)) + 7) `div` 8
          bsPhase' = (bsPhase - (8 - phase)) `mod` 8

-- | Construct a BitBuilder by taking the bottom n bits of a Bits instance. If
--   the instance has less than n bits, this acts as if there was an infinite
--   zero filled prefix
fromBits :: (Integral a, Bits a) => Int -> a -> BitBuilder
fromBits n v
  | n == 0 = empty
  | otherwise = writeN n $ f n where
      f n p phase = do
        let space = 8 - phase
        if n <= space
           then g p phase v n
           else g p phase (v `shiftR` (n - space)) space >> f (n - space) (p `plusPtr` 1) 0
      g p phase v n = do
        byte <- peek p
        let mask = topNBits phase
            bits = ((fromIntegral v) .&. bottomNBits n) `shiftL` ((8 - phase) - n)
        poke p $ (byte .&. mask) .|. bits
{-# INLINE fromBits #-}

------------------------------------------------------------------------

-- | /O(1)./ The concatenation of two BitBuilders, an associative operation
-- with identity 'empty', satisfying
--
--  * @'toLazyByteString' ('append' x y) = 'L.append' ('toLazyByteString' x) ('toLazyByteString' y)@
--
append :: BitBuilder -> BitBuilder -> BitBuilder
append (BitBuilder f) (BitBuilder g) = BitBuilder (f . g)

-- | /O(1)./ A BitBuilder taking a lazy 'L.ByteString', satisfying
--
--  * @'toLazyByteString' ('fromLazyByteString' bs) = bs@
--
fromLazyByteString :: L.ByteString -> BitBuilder
fromLazyByteString = foldr (append . fromByteString . flip (,) 0) empty . L.toChunks

------------------------------------------------------------------------

-- Our internal buffer type
-- The pointer points to the start of the buffer. This never changes
-- for a given buffer. We may 'flush' a partial buffer, in which case
-- the new Buffer has the same pointer, but starts at a different byte
-- offset.
--
-- The bit offset gives the number of valid bits (from the MSB
-- downwards) in the current byte. This ranges from 0 to 8. The number
-- of used bytes does not include the current byte and the number of
-- bytes left is as if the current byte was empty.
data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int                -- byte offset
                     {-# UNPACK #-} !Int                -- bit offset (0..7)
                     {-# UNPACK #-} !Int                -- used bytes
                     {-# UNPACK #-} !Int                -- bytes left

------------------------------------------------------------------------

-- | /O(n)./ Extract a lazy 'L.ByteString' from a 'BitBuilder'.
-- The construction work takes place if and when the relevant part of
-- the lazy 'L.ByteString' is demanded.
--
toLazyByteString :: BitBuilder -> L.ByteString
toLazyByteString m = L.fromChunks $ unsafePerformIO $ do
    fp <- S.mallocByteString (defaultSize `div` 8)
    let buf = Buffer fp 0 0 0 (defaultSize `div` 8)
    return (runBitBuilder (m `append` zeroExtendFinalByte `append` flush) (const []) buf)

-- | /O(1)./ Pop the 'S.ByteString' we have constructed so far, if any,
-- yielding a new chunk in the result lazy 'L.ByteString'.
flush :: BitBuilder
flush = BitBuilder $ \ k buf@(Buffer p bo phase u l) ->
    if u == 0
      then k buf
      else S.PS p bo u : k (Buffer p (bo+u) phase 0 l)

------------------------------------------------------------------------

-- | The default size of a new chunk, in bits
defaultSize :: Int
defaultSize = 8 * (512 - overhead) where
  overhead = 2 * sizeOf (undefined :: Int)

------------------------------------------------------------------------

-- | Sequence an IO operation on the buffer
unsafeLiftIO :: (Buffer -> IO Buffer) -> BitBuilder
unsafeLiftIO f =  BitBuilder $ \ k buf -> inlinePerformIO $ do
    buf' <- f buf
    return (k buf')
{-# INLINE unsafeLiftIO #-}

-- | Get the size of the buffer, in bits
withSize :: (Int -> BitBuilder) -> BitBuilder
withSize f = BitBuilder $ \ k buf@(Buffer _ _ phase _ l) ->
    runBitBuilder (f $ l*8 - phase) k buf

withPhase :: (Int -> BitBuilder) -> BitBuilder
withPhase f = BitBuilder $ \ k buf@(Buffer _ _ phase _ _) ->
    runBitBuilder (f phase) k buf

------------------------------------------------------------------------

zeroExtendFinalByte :: BitBuilder
zeroExtendFinalByte = withPhase $ \phase ->
  if phase == 0
     then empty
     else writeN (8 - phase) (\p phase -> do
       byte <- peek p
       poke p $ byte .&. topNBits phase)

-- | Ensure that there are at least @n@ many bits available.
ensureFree :: Int -> BitBuilder
ensureFree n = n `seq` withSize $ \ l ->
    if n <= l then empty else
        flush `append` unsafeLiftIO (newBuffer (max n defaultSize))
{-# INLINE ensureFree #-}

-- | Ensure that @n@ many bits are available, and then use @f@ to write some
-- bytes into the memory.
writeN :: Int -> (Ptr Word8 -> Int -> IO ()) -> BitBuilder
writeN n f = ensureFree n `append` unsafeLiftIO (writeNBuffer n f)
{-# INLINE [1] writeN #-}

writeNBuffer :: Int -> (Ptr Word8 -> Int -> IO ()) -> Buffer -> IO Buffer
writeNBuffer n f (Buffer fp bo phase u l) = do
    withForeignPtr fp (\p -> f (p `plusPtr` (bo+u)) phase)
    let (bytesUsed, phase') = divMod (phase + n) 8
    return (Buffer fp bo phase' (u+bytesUsed) (l-bytesUsed))
{-# INLINE writeNBuffer #-}

-- | Create a new buffer of, at least, the given bit size. The current
--   buffer is passed in. If it has a partial byte in progress,
--   that byte is merged in. The given buffer must be in the form as
--   produced by 'flush'
newBuffer :: Int -> Buffer -> IO Buffer
newBuffer size (Buffer p bo phase u _) =
  if phase == 0
     then do
       let byteSize = (size + 7) `div` 8
       fp <- S.mallocByteString byteSize
       return $! Buffer fp 0 0 0 byteSize
     else do
       let byteSize = (size + 15) `div` 8
       fp <- S.mallocByteString byteSize
       withForeignPtr fp (\fp ->
         withForeignPtr p (\p -> do
           byte <- peek (p `plusPtr` (bo+u))
           poke fp byte))
       return $! Buffer fp 0 phase 0 byteSize
