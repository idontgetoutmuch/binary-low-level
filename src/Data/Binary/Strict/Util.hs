module Data.Binary.Strict.Util
  ( hexDumpString
  , hexDump
  ) where

import Data.List (intersperse)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString.Lazy.Char8 as BLC

import Text.Printf (printf)

-- | Convert a strict ByteString to a lazy Char8 ByteString, where the format
--   is the same as running hexdump -C on it.
hexDumpString :: B.ByteString -> BLC.ByteString
hexDumpString = BLC.fromChunks . dumpLine (0 :: Int) where
  dumpLine offset bs
    | B.null bs = []
    | otherwise = line : (dumpLine (offset + 16) $ B.drop 16 bs) where
        line = s $ a ++ b ++ "  " ++ c ++ padding ++ right ++ newline
        s = BC.pack
        a = printf "%08x  " offset
        b = concat $ intersperse " " $ map (printf "%02x") $ B.unpack $ B.take 8 bs
        c = concat $ intersperse " " $ map (printf "%02x") $ B.unpack $ B.take 8 $ B.drop 8 bs
        padding = replicate paddingSize ' '
        paddingSize = 2 + (16 - (min 16 $ B.length bs)) * 3 - if B.length bs <= 8 then 1 else 0
        right = map safeChar $ B.unpack $ B.take 16 bs
        newline = "\n"
        safeChar c
          | c >= 32 && c <= 126 = w2c c
          | otherwise = '.'

-- | Performs the same operation as hexDumpString, but also writes it to stdout
hexDump :: B.ByteString -> IO ()
hexDump = BLC.putStr . hexDumpString
