import Data.List (unfoldr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Strict.BitGet as BG
import qualified Data.Binary.BitPut as BP
import Test.QuickCheck -- hiding (test)
import Data.Word


encodeNNBIntBits
    = reverse . (map fromInteger) . unfoldr h

ennb :: (Integer,Integer) -> BP.BitPut
ennb = f
   where
      f = bitPutify . (map fromIntegral) . encodeNNBIntBits
      bitPutify = mapM_ g
      g :: Word8 -> BP.BitPut
      g = BP.putNBits 1

h (_,0) = Nothing
h (0,w) = Just (0, (0, w `div` 2))
h (n,w) = Just (fromIntegral (n `mod` 2), (n `div` 2, w `div` 2))

fromNonNeg xs =
   sum (zipWith (*) (map fromIntegral xs) ys)
   where
      l = length xs
      ys = map (2^) (f (l-1))
      f 0 = [0]
      f x = x:(f (x-1))

bits 0 = return []
bits n =
   do x <- BG.getBit
      xs <- bits (n-1)
      return ((fromEnum x):xs)

test x m = 
   let bs = B.concat $ BL.toChunks $ BP.runBitPut (ennb (x,m)) 
       l  = length (encodeNNBIntBits (x,m))
       in 
          case BG.runBitGet bs (bits l) of
             Left s -> error s
             Right xs -> fromNonNeg xs

prop_toAndFrom x m =
   m >= x && x > 0 ==> x == test x m

main = quickCheck prop_toAndFrom
