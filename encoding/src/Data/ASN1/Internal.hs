{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

{- |
Module      : Data.ASN1.Internal
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown
-}

module Data.ASN1.Internal
  ( uintOfBytes
  , intOfBytes
  , bytesOfUInt
  , bytesOfInt
  , putVarEncodingIntegral
    -- * Strict bytestring versions
  , uintegerOfBytes
  , integerOfBytes
  , bytesOfUInteger
  , bytesOfInteger
  , word64OfBytes
  ) where

import           Data.Bits
                   ( Bits, (.&.), (.|.), bit, complement, countLeadingZeros
                   , shiftL, shiftR, testBit
                   )
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B ( unsafeCreate )
import qualified Data.ByteString.Unsafe as B
import           Data.Int ( Int64 )
import           Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import           Data.Word ( Word64, Word8 )
import           Foreign.Ptr ( Ptr, plusPtr )
import           Foreign.Storable ( pokeByteOff )

-- | Helper function while base < 4.15.0.0 (GHC < 9.0.1) is supported.
singletonNE :: a -> NonEmpty a
#if MIN_VERSION_base(4,15,0)
singletonNE = NE.singleton
#else
singletonNE a = a :| []
#endif

-- | Returns the number of bytes and the unsigned integer represented by the
-- bytes.
uintOfBytes :: ByteString -> (Int, Integer)
uintOfBytes b = (B.length b, uintegerOfBytes b)

-- | The unsigned integer represented by big-endian bytes.
--
-- Up to eight bytes are read as a machine word. Longer inputs are split in
-- two (the low half a whole number of words) and the halves combined with
-- a single shift, which takes time proportional to the length of the input
-- times its logarithm, rather than to its square as a byte-at-a-time fold
-- does.
uintegerOfBytes :: ByteString -> Integer
uintegerOfBytes b
  | len <= 8  = toInteger (word64OfBytes b)
  | otherwise =
      (uintegerOfBytes hi `shiftL` (8 * loLen)) .|. uintegerOfBytes lo
 where
  len = B.length b
  loLen = ((len + 7) `div` 16) * 8
  (hi, lo) = B.splitAt (len - loLen) b

-- | The (at most eight) big-endian bytes, as a machine word.
word64OfBytes :: ByteString -> Word64
word64OfBytes = B.foldl' (\acc w -> (acc `shiftL` 8) .|. fromIntegral w) 0
{-# INLINE word64OfBytes #-}

bytesOfUInt :: Integer -> NonEmpty Word8
bytesOfUInt x
  | x < 0     = singletonNE (fromIntegral x)
  | otherwise = NE.fromList $ B.unpack $ bytesOfUInteger x

-- | Returns the number of bytes in the list and the represented integer by a
-- two's completement list of bytes.
intOfBytes :: ByteString -> (Int, Integer)
intOfBytes b = (B.length b, integerOfBytes b)

-- | The integer represented by two's complement big-endian bytes, @0@ for no
-- bytes.
integerOfBytes :: ByteString -> Integer
integerOfBytes b
  | len == 0  = 0
  | len <= 8  =
      -- Sign-extend: move the top byte to the top of the word, then shift
      -- back arithmetically.
      let s = 64 - 8 * len
      in  toInteger ((fromIntegral (word64OfBytes b `shiftL` s) :: Int64) `shiftR` s)
  | testBit (B.unsafeHead b) 7 = uintegerOfBytes b - bit (8 * len)
  | otherwise = uintegerOfBytes b
 where
  len = B.length b

-- | Convert an integer into a two's completemented list of bytes.
bytesOfInt :: Integer -> NonEmpty Word8
bytesOfInt = NE.fromList . B.unpack . bytesOfInteger

-- | The shortest two's complement big-endian bytes of an integer: at least
-- one byte, and no leading @0x00@ (@0xff@) byte unless the next byte has its
-- top bit set (clear).
bytesOfInteger :: Integer -> ByteString
bytesOfInteger i
  | i >= toInteger (minBound :: Int64) && i <= toInteger (maxBound :: Int64) =
      int64Bytes (fromInteger i)
  | i >= 0 = withSignByte 0 (bytesOfUInteger i)
    -- For negative i, the bits of i are the complement of those of -i - 1.
  | otherwise = B.map complement $ withSignByte 0 (bytesOfUInteger (negate i - 1))
 where
  withSignByte b u
    | testBit (B.unsafeHead u) 7 = B.cons b u
    | otherwise                  = u

int64Bytes :: Int64 -> ByteString
int64Bytes v = B.unsafeCreate len $ \p -> pokeWord64 p len (fromIntegral v)
 where
  magnitude = if v < 0 then complement v else v
  -- one more bit than the magnitude needs, for the sign
  len = (64 - countLeadingZeros magnitude + 1 + 7) `div` 8

-- | The shortest big-endian bytes of a non-negative integer: at least one
-- byte, and no leading @0x00@ byte otherwise.
bytesOfUInteger :: Integer -> ByteString
bytesOfUInteger x
  | x <= toInteger (maxBound :: Word64) = word64Bytes (fromInteger x)
  | otherwise = B.unsafeCreate len $ \p -> pokeInteger p len x
 where
  len = byteLength x

word64Bytes :: Word64 -> ByteString
word64Bytes w = B.unsafeCreate len $ \p -> pokeWord64 p len w
 where
  len = max 1 ((64 - countLeadingZeros w + 7) `div` 8)

-- | The number of bytes a positive integer of more than eight bytes takes:
-- the smallest @k@ with @x < 2^(8k)@. An exponential search for an upper
-- bound, then a binary one.
byteLength :: Integer -> Int
byteLength x = up 16
 where
  up k
    | x < bit (8 * k) = search (k `div` 2) k
    | otherwise       = up (2 * k)
  -- x >= 2^(8 lo) and x < 2^(8 hi)
  search lo hi
    | hi - lo <= 1 = hi
    | x < bit (8 * mid) = search lo mid
    | otherwise = search mid hi
   where
    mid = (lo + hi) `div` 2

-- | Write a non-negative integer as exactly @len@ big-endian bytes (it must
-- fit), splitting it in two when it is longer than a machine word.
pokeInteger :: Ptr Word8 -> Int -> Integer -> IO ()
pokeInteger p len x
  | len <= 8  = pokeWord64 p len (fromInteger x)
  | otherwise = do
      pokeInteger p hiLen (x `shiftR` (8 * loLen))
      pokeInteger (p `plusPtr` hiLen) loLen (x .&. (bit (8 * loLen) - 1))
 where
  loLen = ((len + 7) `div` 16) * 8
  hiLen = len - loLen

-- | Write the low @len@ (at most eight) bytes of a word, big-endian.
pokeWord64 :: Ptr Word8 -> Int -> Word64 -> IO ()
pokeWord64 p len w = go 0
 where
  go !i
    | i >= len  = pure ()
    | otherwise = do
        pokeByteOff p i (fromIntegral (w `shiftR` (8 * (len - 1 - i))) :: Word8)
        go (i + 1)
{-# INLINE pokeWord64 #-}

{- ASN1 often uses a particular kind of 7-bit encoding of integers like
   in the case of long tags or encoding of integer component of OID's.
   Use this function for such an encoding. Assumes a positive integer.

   Here is the description of the algorithm of the above encoding:

   1. The integer is chunked up into 7-bit groups. Each of these 7bit
      chunks are encoded as a single octet.

   2. All the octets except the last one has its 8th bit set.
-}
putVarEncodingIntegral :: (Bits i, Integral i) => i -> ByteString
putVarEncodingIntegral i = B.reverse $ B.unfoldr genOctets (i, True)
 where
  genOctets (x, first)
    | x > 0     =
        let out = fromIntegral (x .&. 0x7F) .|. (if first then 0 else 0x80)
        in  Just (out, (shiftR x 7, False))
    | otherwise = Nothing
