{-# LANGUAGE CPP #-}
-- | This module contains a single class which abstracts over Get and
--   IncrementalGet, so that one can write parsers which work in both.
--   If you are using this module, you may find that
--   -fno-monomorphism-restriction is very useful.
module Data.Binary.Strict.Class where

import           Control.Applicative (Alternative, (<|>))

import qualified Data.ByteString     as B
import           Data.Word

-- | This is the generic class for the set of binary parsers. This lets you
--   write parser functions which are agnostic about the pattern of parsing
--   in which they get used (incremental, strict, bitwise etc)
#ifdef MIN_VERSION_GLASGOW_HASKELL
#if MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)
class (Monad m, MonadFail m, Alternative m) => BinaryParser m where
#else
class (Monad m, Alternative m) => BinaryParser m where
#endif
#endif
  skip :: Int -> m ()
  bytesRead :: m Int
  remaining :: m Int
  isEmpty :: m Bool
  spanOf :: (Word8 -> Bool) -> m B.ByteString
  spanOf1 :: (Word8 -> Bool) -> m B.ByteString
  spanOf1 p = do
    result <- spanOf p
    if B.null result
       then fail ""
       else return result

  string :: B.ByteString -> m ()
  string s = do
    s' <- getByteString $ B.length s
    if s == s'
       then return ()
       else fail $ "expecting:" ++ show s

  word8 :: Word8 -> m ()
  word8 w = do
    w' <- getWord8
    if w == w'
       then return ()
       else fail ""

  oneOf :: (Word8 -> Bool) -> m Word8
  oneOf p = do
    w <- getWord8
    if p w
       then return w
       else fail ""

  many :: m a -> m [a]
  many p = do
    v <- (p >>= return . Just) <|> (return Nothing)
    case v of
         Just x -> do
           rest <- many p
           return $ x : rest
         Nothing -> return []

  many1 :: m a -> m [a]
  many1 p = do
    result <- many p
    case result of
         [] -> fail ""
         x  -> return x

  optional :: m a -> m (Maybe a)
  optional p = (p >>= return . Just) <|> return Nothing

  getWord8 :: m Word8
  getByteString :: Int -> m B.ByteString

  getWord16be :: m Word16
  getWord32be :: m Word32
  getWord64be :: m Word64

  getWord16le :: m Word16
  getWord32le :: m Word32
  getWord64le :: m Word64

  getWordhost :: m Word
  getWord16host :: m Word16
  getWord32host :: m Word32
  getWord64host :: m Word64
