{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
-- for unboxed shifts

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Strict.Get
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Adam Langley <agl@imperialviolet.org>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC.
--
-- This is a strict version of the Get monad from the binary package. It's
-- pretty much just a copy and paste job from the original source code.
-- The binary team are currently unsure about their future plans w.r.t.
-- strictness, so this is a stop gap measure.
--
-- To use, write a function in the Get monad:
--
-- > import Data.Binary.Strict.Get as BinStrict
-- > import Data.ByteString as BS
-- > parse :: BinStrict.Get
-- > parse = getWord16be
-- > main = print $ runGet parse $ BS.pack [1, 1]
--
-- This results in a tuple of (Right 257, \"\") (where the second element is
-- just the remaining data after the parser has run)
-----------------------------------------------------------------------------

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

#include "Common.h"

module Data.Binary.Strict.Get (
    -- * The Get type
      Get
    , runGet

    -- * Parsing
    , lookAhead
    , lookAheadM
    , lookAheadE
    , zero
    , plus
    , spanOf

    -- * Utility
    , skip
    , bytesRead
    , remaining
    , isEmpty

    -- * Parsing particular types
    , getWord8

    -- ** ByteStrings
    , getByteString

    -- ** Big-endian reads
    , getWord16be
    , getWord32be
    , getWord64be

    -- ** Little-endian reads
    , getWord16le
    , getWord32le
    , getWord64le

    -- ** Host-endian, unaligned reads
    , getWordhost
    , getWord16host
    , getWord32host
    , getWord64host

    -- ** Floating point
    , getFloat32host
    , getFloat64host
) where

import Control.Applicative(Alternative(..), Applicative(..))
import Control.Monad (MonadPlus(..), ap)

import Control.Monad (when)
import Data.Maybe (isNothing)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

import Foreign
import Foreign.C.Types

import qualified Data.Binary.Strict.Class as Class

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word
#endif

-- | The parse state
data S = S {-# UNPACK #-} !B.ByteString  -- input
           {-# UNPACK #-} !Int  -- bytes read

newtype Get a = Get { unGet :: S -> (Either String a, S) }

instance Functor Get where
    fmap f m = Get (\s -> case unGet m s of
                               (Right a, s') -> (Right $ f a, s')
                               (Left err, s') -> (Left err, s'))

instance Monad Get where
  return a = Get (\s -> (Right a, s))
  m >>= k = Get (\s -> case unGet m s of
                            (Left err, s') -> (Left err, s')
                            (Right a, s') -> unGet (k a) s')
  fail err = Get (\s -> (Left err, s))

get :: Get S
get = Get (\s -> (Right s, s))

put :: S -> Get ()
put s = Get (const (Right (), s))

initState :: B.ByteString -> S
initState input = S input 0
{-# INLINE initState #-}

plus :: Get a -> Get a -> Get a
plus p1 p2 =
  Get $ \s ->
    case unGet p1 s of
         (Left _, _) -> unGet p2 s
         v@(Right _, _) -> v

zero :: Get a
zero = Get $ \s -> (Left "", s)

instance MonadPlus Get where
  mzero = zero
  mplus = plus

instance Applicative Get where
  pure = return
  (<*>) = ap

instance Alternative Get where
  empty = zero
  (<|>) = plus

instance Class.BinaryParser Get where
  skip = skip
  bytesRead = bytesRead
  remaining = remaining
  isEmpty = isEmpty
  spanOf = spanOf
  getWord8 = getWord8
  getByteString = getByteString

  getWord16be = getWord16be
  getWord32be = getWord32be
  getWord64be = getWord64be

  getWord16le = getWord16le
  getWord32le = getWord32le
  getWord64le = getWord64le

  getWordhost = getWordhost
  getWord16host = getWord16host
  getWord32host = getWord32host
  getWord64host = getWord64host

spanOf :: (Word8 -> Bool) -> Get B.ByteString
spanOf p =
  Get $ \(S s i) ->
    let
      (left, rest) = B.span p s
    in
      (Right left, S rest (i + B.length left))

-- | Run a parser on the given input and return the result (either an error
--   string from a call to @fail@, or the parsing result) and the remainder of
--   of the input.
runGet :: Get a -> B.ByteString -> (Either String a, B.ByteString)
runGet m input =
  case unGet m (initState input) of
       (a, ~(S _ offset)) -> (a, B.drop offset input)

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get ()
skip n = readN (fromIntegral n) (const ())

-- | Run @ga@, but return without consuming its input.
-- Fails if @ga@ fails.
lookAhead :: Get a -> Get a
lookAhead ga = do
    s <- get
    a <- ga
    put s
    return a

-- | Like 'lookAhead', but consume the input if @gma@ returns 'Just _'.
-- Fails if @gma@ fails.
lookAheadM :: Get (Maybe a) -> Get (Maybe a)
lookAheadM gma = do
    s <- get
    ma <- gma
    when (isNothing ma) $
        put s
    return ma

-- | Like 'lookAhead', but consume the input if @gea@ returns 'Right _'.
-- Fails if @gea@ fails.
lookAheadE :: Get (Either a b) -> Get (Either a b)
lookAheadE gea = do
    s <- get
    ea <- gea
    case ea of
        Left _ -> put s
        _      -> return ()
    return ea

-- | Get the total number of bytes read to this point.
bytesRead :: Get Int
bytesRead = do
  S _ b <- get
  return b

-- | Get the number of remaining unparsed bytes.
-- Useful for checking whether all input has been consumed.
remaining :: Get Int
remaining = do
  S s _ <- get
  return (fromIntegral (B.length s))

-- | Test whether all input has been consumed,
-- i.e. there are no remaining unparsed bytes.
isEmpty :: Get Bool
isEmpty = do
  S s _ <- get
  return $ B.null s

------------------------------------------------------------------------
-- Utility with ByteStrings

-- | An efficient 'get' method for strict ByteStrings. Fails if fewer
-- than @n@ bytes are left in the input.
getByteString :: Int -> Get B.ByteString
getByteString n = readN n id
{-# INLINE getByteString #-}

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Get B.ByteString
getBytes n = do
    S s offset <- get
    if n <= B.length s
        then do let (consume, rest) = B.splitAt n s
                put $! S rest (offset + fromIntegral n)
                return $! consume
        else fail "too few bytes"
{-# INLINE getBytes #-}

-- Pull n bytes from the input, and apply a parser to those bytes,
-- yielding a value. If less than @n@ bytes are available, fail with an
-- error. This wraps @getBytes@.
readN :: Int -> (B.ByteString -> a) -> Get a
readN n f = fmap f $ getBytes n
{-# INLINE readN #-}

getPtr :: Storable a => Int -> Get a
getPtr n = do
    (fp, o, _) <- readN n B.toForeignPtr
    return . B.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)
{-# INLINE getPtr #-}

GETWORDS(Get, getBytes)
GETHOSTWORDS(Get)

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

------------------------------------------------------------------------
-- Floating point support

getFloat32host :: Get Float
getFloat32host = (getPtr :: Int -> Get CFloat) 4 >>= return . fromRational . toRational

getFloat64host :: Get Double
getFloat64host = (getPtr :: Int -> Get CDouble) 8 >>= return . fromRational . toRational
