{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
-- for unboxed shifts

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.Strict.IncrementalGet
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Adam Langley <agl@imperialviolet.org>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC.
--
-- This is a version of the Get monad for incremental parsing. The parser is
-- written as if a single, huge, strict ByteString was to be parsed. It
-- produces results as it parses by calling yield.
--
-- However, if the parser runs out of data, rather than failing the caller sees
-- a Partial result, which includes the list of yielded values so far and a
-- continuation. By calling the continuation with more data, the parser
-- continues, none the wiser.
--
-- Take the following example
--
-- > testParse = do
-- >   a <- getWord16be
-- >   b <- getWord16be
-- >   return $ a + b
-- >
-- > test = runGet testParse $ B.pack [1,0,0]
--
-- Here @testParse@ needs to read 4 bytes in order to complete, so test is
-- a Partial, which includes the continuation function, so which you can pass
-- more data until it completes
--
-- The lookahead functions have been removed from this parser because of their
-- incompatibility with the incremental monad at the moment.
-----------------------------------------------------------------------------

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

#include "Common.h"

module Data.Binary.Strict.IncrementalGet (
    -- * The Get type
      Get
    , Result(..)
    , runGet

    -- * Utility
    , skip
    , bytesRead
    , remaining
    , isEmpty
    , plus
    , zero
    , spanOf
    , suspend

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
) where

import Control.Applicative(Alternative(..), Applicative(..))
import Control.Monad (MonadPlus(..), ap)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL

import Foreign

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word
#endif

import qualified Data.Binary.Strict.Class as Class

-- | The parse state
data S = S {-# UNPACK #-} !BL.ByteString  -- input
           {-# UNPACK #-} !Int  -- bytes read
           {-# UNPACK #-} ![B.ByteString]
           {-# UNPACK #-} !Int  -- the failure depth

-- | The result of a partial parse
data Result a = Failed String
                -- ^ the parse failed with the given error message
              | Finished B.ByteString a
                -- ^ the parse finished and produced the given list of
                --   results doing so. Any unparsed data is returned.
              | Partial (B.ByteString -> Result a)
                -- ^ the parse ran out of data before finishing, but produced
                --   the given list of results before doing so. To continue the
                --   parse pass more data to the given continuation

-- | This is the internal version of the above. This is the type which is
--   actually used by the code, as it has the extra information needed
--   for backtracking. This is converted to an external friendly @Result@
--   type just before giving it to the outside world.
data IResult a = IFailed S String
               | IFinished S a
               | IPartial (B.ByteString -> IResult a)

instance Show (IResult a) where
  show (IFailed _ err) = "IFailed " ++ err
  show (IFinished _ _) = "IFinished"
  show (IPartial _) = "IPartial"

instance (Show a) => Show (Result a) where
  show (Failed err) = "Failed " ++ err
  show (Finished rest rs) = "Finished " ++ show rest ++ " " ++ show rs
  show (Partial _) = "Partial"

newtype Get r a = Get { unGet :: S -> (a -> S -> IResult r) -> IResult r }

instance Functor (Get r) where
    fmap f m = Get (\s -> \cont -> unGet m s (cont . f))

instance Monad (Get r) where
  return a = Get (\s -> \k -> k a s)
  m >>= k = Get (\s -> \cont -> unGet m s (\a -> \s' -> unGet (k a) s' cont))
#ifdef MIN_VERSION_GLASGOW_HASKELL
#if MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)

instance MonadFail (Get r) where
  fail err = Get (\s -> const $ IFailed s err)
#else
  fail err = Get (\s -> const $ IFailed s err)
#endif
#endif

get :: Get r S
get = Get (\s -> \k -> k s s)

strictToLazy :: B.ByteString -> BL.ByteString
strictToLazy x
  | B.null x = BL.Empty
  | otherwise = BL.Chunk x BL.Empty

lazyToStrict :: BL.ByteString -> B.ByteString
lazyToStrict = B.concat . BL.toChunks

initState :: B.ByteString -> S
initState input = S (strictToLazy input) 0 [] 0
{-# INLINE initState #-}

-- | This turns an internal Result into one safe for the outside world
toplevelTranslate :: IResult a -> Result a
toplevelTranslate (IFailed _ err) = Failed err
toplevelTranslate (IFinished (S rest _ _ _) value) = Finished (lazyToStrict rest) value
toplevelTranslate (IPartial k) = Partial $ toplevelTranslate . k

-- | This is the final continuation that turns a passed value into an IFinished
terminalContinuation :: a -> S -> IResult a
terminalContinuation v s = IFinished s v

-- | Start a parser and return the first Result.
runGet :: Get r r -> B.ByteString -> Result r
runGet m input =
  toplevelTranslate $ unGet m (initState input) terminalContinuation

-- | I'm not sure if this is a huge bodge or not. It probably is.
--
--   When performing a choice (in @plus@), the failure depth in the current
--   state is incremented. If a failure is generated inside the attempted path,
--   the state carried in the IFailure will have this incremented failure
--   depth. However, we don't want to backtrack after the attempted path has
--   completed. Thus we insert this cut continuation, which decrements the failure
--   count of any failure passing though, thus it would be caught in @plus@ and
--   doesn't trigger a backtrack.
cutContinuation :: (a -> S -> IResult r) -> a -> S -> IResult r
cutContinuation k v s =
  case k v s of
       IFailed (S lb i adds failDepth) err -> IFailed (S lb i adds (failDepth - 1)) err
       x -> x

-- | This is the choice operator. If the first option fails, the second is
--   tried. The failure of the first option must happen within this function
--   otherwise rollback is not attempted.
plus :: Get r a -> Get r a -> Get r a
plus p1 p2 =
  Get $ \(S lb i adds failDepth) k ->
    let
      filter f@(IFailed (S _ _ adds' failDepth') _)
        | failDepth' == failDepth + 1 = unGet p2 (S (lb `BL.append` (BL.fromChunks $ reverse adds')) i (adds' ++ adds) failDepth) k
        | otherwise = f
      filter (IPartial cont) = IPartial (filter . cont)
      filter v@(IFinished _ _) = v
    in
      filter $ unGet p1 (S lb i [] (failDepth + 1)) (cutContinuation k)

zero :: Get r a
zero = fail ""

instance MonadPlus (Get r) where
    mzero = zero
    mplus = plus

instance Applicative (Get r) where
    pure = return
    (<*>) = ap

instance Alternative (Get r) where
    empty = zero
    (<|>) = plus

instance Class.BinaryParser (Get r) where
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

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get r ()
skip n = readN (fromIntegral n) (const ())

-- | Get the total number of bytes read to this point.
bytesRead :: Get r Int
bytesRead = do
  S _ b _ _ <- get
  return b

-- | Get the number of remaining unparsed bytes.
-- Useful for checking whether all input has been consumed.
remaining :: Get r Int
remaining = do
  S s _ _ _<- get
  return (fromIntegral (BL.length s))

-- | Test whether all input has been consumed,
-- i.e. there are no remaining unparsed bytes.
isEmpty :: Get r Bool
isEmpty = do
  S s _ _ _ <- get
  return $ BL.null s

------------------------------------------------------------------------
-- Utility with ByteStrings

-- | An efficient 'get' method for strict ByteStrings. Fails if fewer
-- than @n@ bytes are left in the input.
getByteString :: Int -> Get r B.ByteString
getByteString n = readN n id
{-# INLINE getByteString #-}

-- | Yield a partial and get more data
suspend :: Get r ()
suspend = Get $ \(S lb i adds failDepth) k ->
  IPartial (\s -> k () (S (BL.append lb $ strictToLazy s) i (s : adds) failDepth))

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Get r B.ByteString
getBytes n = Get $ \(S s offset adds failDepth) -> \cont ->
  if fromIntegral n <= BL.length s
     then let (consume, rest) = BL.splitAt (fromIntegral n) s
           in cont (lazyToStrict consume) $ S rest (offset + fromIntegral n) adds failDepth
     else IPartial (\s' -> unGet (getBytes n) (S (BL.append s $ strictToLazy s') offset (s' : adds) failDepth) cont)
{-# INLINE getBytes #-}

-- Pull n bytes from the input, and apply a parser to those bytes,
-- yielding a value. If less than @n@ bytes are available, fail with an
-- error. This wraps @getBytes@.
readN :: Int -> (B.ByteString -> a) -> Get r a
readN n f = fmap f $ getBytes n
{-# INLINE readN #-}

getPtr :: Storable a => Int -> Get r a
getPtr n = do
    (fp, o, _) <- readN n B.toForeignPtr
    return . B.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)
{-# INLINE getPtr #-}

GETWORDS(Get r, getBytes)
GETHOSTWORDS(Get r)

spanOf :: (Word8 -> Bool) -> Get r B.ByteString
spanOf p =
  Get $ \(S lb i adds failDepth) k ->
    let
      (left, rest) = BL.span p lb
    in
      if BL.null rest
         then IPartial (\s -> unGet (spanOf p) (S (strictToLazy s) (i + (fromIntegral $ BL.length lb)) (s : adds) failDepth) (\a -> k $ B.append (lazyToStrict left) a))
         else k (lazyToStrict left) (S rest (i + (fromIntegral $ BL.length left)) adds failDepth)

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
