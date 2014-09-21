{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary.BitPut
-- Copyright   : Dominic Steinitz
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Dominic Steinitz <dominic.steinitz@blueyonder.co.uk>
-- Stability   : experimental
--
-- This is the writer dual to BitGet. It allows one to append bits in a monad
-- and get a strict ByteString as a result. Bits are appended from the MSB of
-- the first byte towards the LSB of the last byte.
--
-- This is best suited to small bit-fields because it accumulates bytes using
-- snoc, so large results will cause a lot of copying. It would be possible
-- to switch to using something similar to the Builder monad if need arises.
-- However, since most protocols only have small bit fields, this should
-- suffice for many cases.
-----------------------------------------------------------------------------
module Data.Binary.BitPut
  ( BitPut
  , BitPutM
  , BitPutT
  , runBitPut
  , runBitPutM
  , runBitPutT
  , putBit
  , putBitT
  , putNBits
  , putNBitsT
  , putBits
  , putByteString
  , putLeftByteString
  ) where

import Data.Bits (bitSize, Bits)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.BitBuilder as BB

newtype BitPutM a = BitPutM { unPut :: (a, BB.BitBuilder) }

type BitPut = BitPutM ()

instance Functor BitPutM where
   fmap f m = BitPutM (let (a, w) = unPut m in (f a, w))

instance Monad BitPutM where
   return a = BitPutM (a,BB.empty)
   m >>= k = BitPutM (let (a, w) = unPut m
                          (b, w') = unPut (k a)
                       in (b, w `BB.append` w'))

   m >> k = BitPutM (let (_, w) = unPut m
                         (b, w') = unPut k
                      in (b, w `BB.append` w'))
   {-# INLINE (>>) #-}

newtype BitPutT m a = BitPutT { unPutT :: m (a, BB.BitBuilder) }

-- | Append a single bit
putBit :: Bool -> BitPut
putBit bit = BitPutM ((), BB.singleton bit)

-- | Append the bottom n bits of the given bits value. In the case that more
--   bits are requested than the value provides, this acts as if the value
--   has as unlimited number of leading 0 bits.
putNBits :: (Integral a, Bits a) => Int -> a -> BitPut
putNBits n v = BitPutM ((), BB.fromBits n v)

-- | Append a value. Note that this function is undefined for instances of Bits
--   which have no fixed bitsize (like Integer)
putBits :: (Integral a, Bits a) => a -> BitPut
putBits v = putNBits (bitSize v) v

-- | Append a ByteString
putByteString :: B.ByteString -> BitPut
putByteString bs = BitPutM ((), BB.fromByteString (bs, 0))

-- | Append a left aligned ByteString where ByteString has a partial byte
--   with the given number of valid bits, from the MSB downwards. The number
--   of such bits must be 0..7. (A normal ByteString, which all bytes full
--   would use 0)
putLeftByteString :: (B.ByteString, Int) -> BitPut
putLeftByteString bs = BitPutM ((), BB.fromByteString bs)

runBitPut :: BitPut -> BL.ByteString
runBitPut m = let (_, w) = unPut m
               in BB.toLazyByteString w

runBitPutM :: BitPutM a -> (a, BL.ByteString)
runBitPutM m = let (x, w) = unPut m
               in (x, BB.toLazyByteString w)

instance Monad m => Functor (BitPutT m) where
   fmap f m = BitPutT $ do
      ~(x, w) <- unPutT m
      return (f x, w)

instance Monad m => Monad (BitPutT m) where
   return a = BitPutT $ return (a, BB.empty)
   m >>= k = BitPutT $ do
      ~(a, w) <- unPutT m
      ~(b, w') <- unPutT (k a)
      return (b, w `BB.append` w')

runBitPutT :: Monad m => BitPutT m a -> m (a, BL.ByteString)
runBitPutT m = do
  ~(x, w) <- unPutT m
  return (x, BB.toLazyByteString w)

putBitT :: (Monad m) => Bool -> BitPutT m ()
putBitT bit = BitPutT $ return ((), BB.singleton bit)

putNBitsT :: (Monad m, Integral a, Bits a) => Int -> a -> BitPutT m ()
putNBitsT n v = BitPutT $ return ((), BB.fromBits n v)

instance MonadTrans BitPutT where
   lift m = BitPutT $ do
      a <- m
      return (a, BB.empty)

instance MonadError e m => MonadError e (BitPutT m) where
   throwError = lift . throwError
   m `catchError` h = BitPutT $ do
      unPutT m `catchError` \e -> unPutT (h e)
